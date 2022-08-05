library(tidyverse)
library(tidybayes)
library(cmdstanr)
library(broom)
library(mlogit)
library(evd)
library(data.table)
options(mc.cores = 4)
set.seed(1342)
ts_model = cmdstan_model("stan/ts-mvn.stan")
choice_model = cmdstan_model("stan/group-mlogit.stan")





simulate_data = function(N, shares, beta, J) {
    # beta is K x J where K is n treat, J is N arms
    D = sample(1:nrow(beta), size = N, replace = TRUE, prob = shares)
    Y = beta[D, 1:J] + matrix(rnorm(N*J, mean = 0, sd = 3), nrow = N, ncol = J)
    colnames(Y) = paste0("Y_", 1:J)
    df = as_tibble(Y) %>%
        bind_cols(D = D)
    return(df)
}

simulate_choice_data = function(N, beta_hat_array, util_weights) {
    K = nrow(beta_hat_array[[1]]) 
    J = ncol(beta_hat_array[[1]])
    res_matrix = matrix(NA, nrow = N, ncol = (1 + J*K))
    for (i in 1:N) {
        Y_latent = beta_hat_array[[i]] + 0*matrix(rnorm(K*J), nrow = K, ncol = J)
        U_Y_latent =  Y_latent %*% util_weights + matrix(rgumbel(K), nrow = K)
        res_matrix[i, 1] = which.max(U_Y_latent)
        res_matrix[i, 2:ncol(res_matrix)] = as.vector(beta_hat_array[[i]])
    }
    df = as_tibble(res_matrix)

    colnames(df) = c("D", expand.grid(K = 1:K, J = 1:J) %>%
        mutate(colname = paste0("beta_J_",J, ".", K)) %>%
        select(colname) %>%
        pull())
    return(df)
}


fit_ts = function(sim_data, N, K, J){

    stan_data = list(
        N = N,
        J = J,
        K = K,
        y = as.matrix(sim_data %>% select(contains("Y_"))),
        treat_arm = sim_data$D
    )
    stan_fit = ts_model$sample(
        data = stan_data,
        chains = 1, 
        refresh = 0,
        show_messages = FALSE
    )

    return(stan_fit)
}


extract_shares = function(ts_fit, utility_weight_df){
     clean_draws = ts_fit %>%
        gather_draws(beta[k, j])

    # Add good weights
    clean_draws = clean_draws %>%
        left_join(
            utility_weight_df %>% select(j, .draw, weight), 
            by = c("j", ".draw")
        )

       best_draw_pr =
            # weight beta draws by corresponding outcome weight
            clean_draws  %>%
            mutate(weighted_value = .value*weight) %>%
            group_by(.draw, k) %>%
            # For each arm sum utility
            summarise(U_Y = sum(weighted_value)) %>%
            # Now choose arm with highest overall utility
            pivot_wider(
                values_from = U_Y, 
                names_from = k,
            ) %>%
            ungroup() %>%
            select(-contains(".")) %>%
            # Count how many times each arm K is optimal
            mutate(best_draw = names(.)[max.col(.)])  %>%
            mutate(best_draw = factor(best_draw, levels = 1:max(clean_draws$k))) %>%
            group_by(best_draw, .drop = FALSE) %>%
            summarise(pr = n()/nrow(.)) %>% 
            arrange(best_draw)
    return(best_draw_pr$pr)
}



#' Creates list of length K with N x J matrix of data
extract_x_k_matrix = function(data, k){
    
    mat_dat = data %>%
        select(contains(paste0(".", k))) %>%
        as.matrix()

    return(mat_dat)
}


TS_draw = function(N,
                   K, 
                   J, 
                   shares, 
                   beta, 
                   util_weights, 
                   weight_type = "estimated", 
                   prev_data = NULL) {
    # For now, N in welfare sample == N in treatment sample
    N_choice = N
    # Create fake data for new wave
    ts_draw = simulate_data(N, shares, beta, J)
    # Add new wave data to original
    ts_draw = bind_rows(
        ts_draw,
        prev_data$data
    )
    # Fit TS and find treatment arm MUs
    ts_fit = fit_ts(
        ts_draw, 
        N = nrow(ts_draw), 
        K = nrow(beta), 
        J = J) 
    # extract estimated betas
    clean_draws = ts_fit %>%
        gather_draws(beta[k, j])

    if (weight_type == "estimated") {
        
        # If we estimate weights, take a random draw from 
        # TE posterior
        random_draws = ts_fit %>%
            tidy_draws() %>%
            slice_sample(n = N_choice, replace = TRUE) %>%
            select(contains("beta"))
        # Reshape into an array of matrices
        beta_hat_matrix_array = apply(
            random_draws,
            1,
            function(x){matrix(x, nrow = K)}, 
            simplify = FALSE)
        # Create choices for new wave, given reported posterior TE draws
        choice_df = simulate_choice_data(N_choice, beta_hat_matrix_array, util_weights)
        # Add to old wave
        choice_df = bind_rows(
            choice_df,
            prev_data$choice_data
        )
        # Convert into a weird block matrix for Stan 
        k_block_mat = map(1:K, ~extract_x_k_matrix(data = choice_df, .x))
        stan_data = list(
            N = nrow(choice_df), 
            K = K, 
            J = J, 
            y = choice_df$D, 
            x = k_block_mat
        )
       # Estimate weights using multinomial logit 
        weight_fit = choice_model$sample(
            stan_data, 
            chains = 1 
        )
        # Extract weights
        clean_weight_fit = weight_fit %>%
            gather_draws(beta[j])
        weight_draws = clean_weight_fit %>%
            mutate(weight = .value) %>%
            ungroup() %>%
            select(j, .draw, weight) 
    }
    if (weight_type == "equal") {
        # Weight every outcome the same
        weight_draws = tibble(
            j = rep(1:J, 1000),
            .draw = 1:(1000*J),
            weight = rep(1, J*1000)
        )
        # No choice data
        choice_df = NULL
    }

    if (weight_type == "first") {
        # Only weight first outcome
        weight_draws = tibble(
            j = rep(1:J, 1000),
            .draw = 1:(1000*J),
        ) %>%
        mutate(weight = 1*(j <= 1))
        choice_df = NULL
    }
    
    # Extract optimal shares from weighted fit
    ts_pr = extract_shares(ts_fit, weight_draws)
    # Create an MRS df
    clean_mrs = weight_draws %>%
        group_by(.draw) %>%
        mutate(.value = weight/weight[j == 1]) %>%
        ungroup() %>%
        select(.value, j ) %>%
        group_by(j) %>%
        mean_qi() %>%
        mutate(.variable = "mrs")

    # Create posterior medians of: weights, MRSs, TEs
    posterior_means = bind_rows(
        clean_weight_fit %>%
            mean_qi() %>%
            mutate(.variable = "weights"),
        clean_mrs,
        clean_draws %>%
            mean_qi() 
    ) %>%
        to_broom_names()

    return(list(
        shares = ts_pr, 
        data = ts_draw, 
        choice_data = choice_df,
        weight_draws = weight_draws, 
        posterior_means = posterior_means))
}

################################
TS_simulation = function(N_rounds, beta, util_weights, K, N, J, weight_type = "estimated") {
    ## Create initial values
    initial_shares = rep(1/K, K) 
    result_dt = data.table(
        round = 1:N_rounds
    ) 
    # Pre-allocate matrices to fill
    Y_matrix = matrix(
        NA, nrow = N_rounds, ncol = J
    )
    share_matrix = matrix(
        NA, nrow = N_rounds, ncol = K
    )
    weight_matrix = matrix(
        NA, nrow = N_rounds, ncol = J
    )
    # Fill t = 1
    share_matrix[1, ] = initial_shares 
    result_dt[1, "time_period"] = 1
    weight_matrix[1, ] = rep(1/J, J)
    prev_data = NULL
    posterior_means = NULL
    ## Simulate each round of bandit problem
    for (i in 2:N_rounds) {
        draw = TS_draw(
            N = N, 
            K = K, 
            J = J, 
            shares = share_matrix[i-1,], 
            beta = beta,
            util_weights = util_weights,
            weight_type = weight_type,
            prev_data = prev_data)
        # Fill in shares and weights
        share_matrix[i,] = draw$shares
        weight_matrix[i,] = draw$posterior_means %>%
            filter(term == "mrs") %>%
            select(estimate) %>%
            pull() 
        Y_matrix[i, ] = colMeans(draw$data[, 1:J])
        result_dt[i, "time_period"] = i
        # Update previous TE and welfare sample data
        prev_data = list(data = draw$data, choice_data =  draw$choice_data)
        posterior_means = bind_rows(posterior_means, draw$posterior_means %>% mutate(round = i))
        print(paste0("Round: ", i))
    }
    # Create names for matrices
    share_names = paste0("arm_", 1:K)
    weight_names = paste0("J_", 1:J)
    Y_names = paste0("Y_", 1:J)
    # Add to a data.table
    result_dt[, c(share_names) := as.data.table(share_matrix)]
    result_dt[, c(weight_names) := as.data.table(weight_matrix)]
    result_dt[, c(Y_names) := as.data.table(Y_matrix)]
    result_dt[, weight_type := weight_type]
    posterior_means$weight_type = weight_type

    return(list(
        result_dt = result_dt, 
        posterior_means = posterior_means,
        beta = beta, 
        util_weights = util_weights))
}
N_rounds = 5
N_per_round = 10
N_rounds*N_per_round
N_arms = 4
J_outcomes = 3
util_weights = rnorm(J_outcomes)
beta = matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes)
er_true = beta %*% (util_weights)
er_naive = beta %*% rep(1, J_outcomes)

while (which.max(er_true) == which.max(er_naive)) {
    util_weights = rnorm(J_outcomes)
    beta = matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes)
    er_true = beta %*% (util_weights)
    er_naive = beta %*% rep(1, J_outcomes)
}
er_true
er_naive
thompson_sim = TS_simulation(
    N_rounds, 
    beta, 
    util_weights,
    N_arms,
    N_per_round,
    J_outcomes)



long_result_df %>%
    ggplot(aes(x = round, y = value, colour = factor(variable_n))) +
    facet_wrap(~variable, ncol = 1, scales = "free") +
    geom_line() +
    geom_point()
analyse_MAB_results = function(MAB_sim) {
    MAB_sim = thompson_sim
    res_dt = MAB_sim$result_dt
    post_df = MAB_sim$posterior_means
    beta = MAB_sim$beta
    util_weights = MAB_sim$util_weights


    util_df = create_weight_df(util_weights)

    true_arm_util = beta %*% util_weights
    true_optimal_arm = which.max(true_arm_util)
    Delta = max(true_arm_util) - sort(true_arm_util)[length(true_arm_util) - 1]

    long_result_df = thompson_sim$result_dt %>%
        select(-time_period) %>%
        gather(variable, value, -round, -weight_type) %>%
        as_tibble() %>%
        mutate(variable_n = str_extract(variable, "(?<=_)\\d+") %>% as.numeric(),
            variable = str_extract(variable, ".*(?=_)"))

    estimated_optimal_arm = long_result_df %>%
        filter(variable == "arm" & round == max(round)) %>%
        filter(value == max(value)) %>%
        select(variable_n) %>%
        pull()

    mean_welfare = long_result_df %>%
        filter(variable == "Y") %>%
        filter(round > 1) %>%
        left_join( 
            util_df %>% select(j, true_weight = value),
            by = c("variable_n" = "j")
        ) %>%
        summarise(
           mean_U =   sum(value * true_weight)
        ) %>%
        pull()
    mean_welfare


    analysis_df = tibble(
        true_optimal_arm = true_optimal_arm,
        estimated_optimal_arm = estimated_optimal_arm,
        mean_welfare = mean_welfare,
        Delta = Delta
    )

}

create_beta_df = function(true_beta) {
    true_beta_df  = c(true_beta) %>%
        enframe() %>%
        mutate(
            k = rep(1:nrow(beta), ncol(beta)), 
            j = rep(1:ncol(beta), each = nrow(beta)))
    return(true_beta_df)
}


create_weight_df = function(true_util_weights) {
    true_weight_df = enframe(true_util_weights, name = "name") %>%
        mutate(name = paste0("Y_", name), 
               rel_weight = value/value[name == "Y_1"]) %>%
        mutate(j = str_extract(name, "(?<=_)\\d+") %>% as.numeric())
    return(true_weight_df)
}

true_beta_df = create_beta_df(beta)
true_weight_df = create_weight_df(util_weights)
true_weight_df


inner_join(
    true_beta_df,
    true_weight_df %>% select(-value, -name), 
    by = "j"
) %>%
    group_by(k) %>%
    summarise(E_U = sum(value*rel_weight))


find_optimal_arm = function(beta, util_weights) {
    optimal_arm = which.max(beta %*% util_weights)
    return(optimal_arm)
}



thompson_sim$result_dt %>%
    pivot_longer(
        cols = !c("round", "time_period")
    ) 



thompson_sim$posterior_means %>%
    filter(term == "beta") %>%
    ggplot(aes(
        x = round, 
        ymin = conf.low, 
        ymax = conf.high,
        y = estimate, 
        colour = factor(j) 
    )) +
    geom_pointrange() +
    geom_line() +
    facet_grid(k~j, scales = "free") +
    geom_hline(
        data = true_beta_df,
        aes(yintercept = value),
        linetype = "longdash"
    )

thompson_sim$posterior_means %>%
    filter(term == "mrs") %>%
    ggplot(aes(
        x = round, 
        ymin = conf.low, 
        ymax = conf.high,
        y = estimate,
        colour = factor(j)
    )) +
    geom_pointrange() +
    geom_line() +
    geom_hline(
        data = true_weight_df %>% mutate(j = 1:n()),
        aes(
            yintercept = rel_weight
        ),
        linetype = "longdash")  +
        facet_wrap(~j, ncol = 1, scales = "free") +
        theme_bw()


# naive_thompson_sim = TS_simulation(
#     N_rounds, 
#     beta, 
#     util_weights,
#     N_arms,
#     N_per_round,
#     J_outcomes, 
#     estimate_weights = FALSE)
true_weight_df

util_weights / util_weights[1]

thompson_sim$result_dt %>%
    pivot_longer(
        cols = !c("round", "time_period")
    ) %>%
    filter(str_detect(name, "arm|J")) %>%
    mutate(name = str_replace(name, "J", "Y")) %>%
    mutate(type = str_extract(name, ".*(?=_)"), 
            number = str_extract(name, "\\d+") %>% as.numeric()) %>%
    ggplot(aes(
        x = round, 
        y = value, 
        colour = name 
    )) +
    facet_wrap(~type, ncol = 1, scales = "free") +
    geom_line() +
    geom_point() +
    geom_hline(
        data = true_weight_df %>% mutate(type = "Y", name = str_replace(name, "J", "Y")),
        aes(
            yintercept = rel_weight, 
            colour = name
        ),
        linetype = "longdash") +
    theme_bw() +
    scale_x_continuous(breaks = 1:20) +
    theme(panel.grid.minor = element_blank()) +
    labs(
        title = "Joint Estimation of Optimal Treatment and MRS", 
        x = "Round", 
        y = "Proportion/MRS", 
        colour = "Arm/Outcome"
    )
er_true
util_weights/util_weights[1]

beta %*% util_weights

# ggsave("data/output/plots/mab-mrs-ot.png", 
#        width = 8, 
#        height = 6)

thompson_sim %>%
    pivot_longer(
    contains("J")
    )  %>%
    ggplot(aes(
        x = time_period, 
        y = value, 
        colour = name
    )) +
    geom_point() +
    geom_line() +
    geom_hline(
        data = true_weight_df,
        aes(
            yintercept = rel_weight, 
            colour = name
        ),
        linetype = "longdash") +
    theme_bw()  +
    labs(
        x = "Round", 
        y = "Weights", 
        colour = "Outcome", 
        title = "Outcome Weights Across Rounds"
    ) +
    scale_x_continuous(breaks = thompson_sim$time_period) +
    theme(panel.grid.minor = element_blank())

# ggsave("data/output/plots/outcome-weights-example.png", 
#         width = 8,
#         height = 6)

comp_thompson_sim = bind_rows(
    thompson_sim %>% mutate(algorithm = "utility-weighted"),
    naive_thompson_sim %>% mutate(algorithm = "unweighted"),
) %>%
    as_tibble()
comp_thompson_sim %>%
    pivot_longer(
    contains("arm")
    ) %>% 
    ggplot(aes(
        x = time_period, 
        y = value, 
        colour = name
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~algorithm, ncol = 1) +
    theme_bw() +
    scale_x_continuous(breaks = thompson_sim$time_period) +
    labs(
        title = "Utility Bandit Treatment Assignment",
        x = "Round", 
        y = "Arm Proportion",
        colour = "Treatment Arm"
    ) +
    theme(panel.grid.minor = element_blank())

# ggsave("data/output/plots/comp-prop.png", 
# width = 8, 
# height = 6)
ra_df = tibble(
    round = 2:10,
    util = beta %*% util_weights %>% mean(),
    algorithm = "random-assignment"
)

comp_thompson_sim %>%
    pivot_longer(
    contains("Y")
    )  %>%
    left_join(
        true_weight_df %>% mutate(name = str_replace(name, "J", "Y")) %>% select(name, rel_weight), 
        by = "name"
    )  %>%
    mutate(util = value*rel_weight) %>%
    group_by(round, algorithm) %>%
    summarise(util = sum(util)) %>%
    bind_rows( 
        ra_df
    ) %>%
    mutate(algorithm = factor(algorithm, levels = c("utility-weighted", "unweighted", "random-assignment"))) %>% 
    filter(round > 1) %>%
    ggplot(aes(
        x = round, 
        y = util, 
        colour = algorithm)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs( 
        title = "In Sample Welfare Across Algorithms", 
        x = "Round", 
        y = "Welfare", 
        colour = "Algorithm"
    ) +
    theme(legend.position = "bottom", 
           panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = 2:10)


# ggsave(
#     "data/output/plots/in-sample-welfare.png", 
#     width = 8, 
#     height = 6
# )

naive_thompson_sim %>%
    pivot_longer(
    contains("arm")
    )  %>%
    ggplot(aes(
        x = time_period, 
        y = value, 
        colour = name
    )) +
    geom_point() +
    geom_line()

thompson_sim %>%
    pivot_longer(
    contains("arm")
    )  %>%
    ggplot(aes(
        x = time_period, 
        y = value, 
        colour = name
    )) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_x_continuous(breaks = thompson_sim$time_period) +
    labs(
        title = "Utility Bandit Treatment Assignment",
        x = "Round", 
        y = "Arm Proportion",
        colour = "Treatment Arm"
    ) +
    theme(panel.grid.minor = element_blank())
# ggsave(
#     "data/output/plots/util-bandit-assignment-example.png",
#     width = 8, 
#     height = 6
# )


beta %*% (util_weights/util_weights[1])
beta %*% rep(1, J_outcomes)

Y_cols = paste0("Y_", 1:J_outcomes)
Y_cols
as.matrix(thompson_sim[2:nrow(thompson_sim), ..Y_cols]) %*% util_weights

beta %*% util_weights
############# Scratch


welfare_sim = function(draw){


    N_rounds = 10
    N_per_round = 100
    N_rounds*N_per_round
    N_arms = 4
    J_outcomes = 3
    util_weights = rnorm(J_outcomes)
    beta = matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes)
    er_true = beta %*% (util_weights)
    er_naive = beta %*% rep(1, J_outcomes)

    while (which.max(er_true) == which.max(er_naive)) {
        util_weights = rnorm(J_outcomes)*3
        beta = matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes)
        er_true = beta %*% (util_weights)
        er_naive = beta %*% rep(1, J_outcomes)
    }

    true_weight_df = enframe(util_weights) %>%
        mutate(name = paste0("J_", name), 
            rel_weight = value/value[name == "J_1"])


    thompson_sim = TS_simulation(
        N_rounds, 
        beta, 
        util_weights,
        N_arms,
        N_per_round,
        J_outcomes)

    naive_thompson_sim = TS_simulation(
        N_rounds, 
        beta, 
        util_weights,
        N_arms,
        N_per_round,
        J_outcomes, 
        estimate_weights = FALSE)

    comp_thompson_sim = bind_rows(
        thompson_sim %>% mutate(algorithm = "utility-weighted"),
        naive_thompson_sim %>% mutate(algorithm = "unweighted"),
    ) %>%
        as_tibble()

    ra_df = tibble(
        round = 2:10,
        util = beta %*% util_weights %>% mean(),
        algorithm = "random-assignment"
    )
    welfare_df = comp_thompson_sim %>%
    pivot_longer(
    contains("Y")
    )  %>%
    left_join(
        true_weight_df %>% mutate(name = str_replace(name, "J", "Y")) %>% select(name, rel_weight), 
        by = "name"
    )   %>%
    mutate(util = value*rel_weight) %>%
    group_by(round, algorithm) %>%
    summarise(util = sum(util)) %>%
    bind_rows( 
        ra_df
    ) %>%
    welfare_df$draw = draw
    return(welfare_df)
}



library(furrr)
plan(multisession, workers = 8)

sims = future_map_dfr(
    1:100, 
    welfare_sim, 
    .progress = TRUE, 
    .options = furrr_options(seed = TRUE, packages = c("broom"), scheduling = 2L)
)

sims

# sims %>%
#     group_by(
#         draw
#     ) %>%


# ra_df = tibble(
#     round = 2:10,
#     util = beta %*% util_weights %>% mean(),
#     algorithm = "random-assignment"
# )

# comp_thompson_sim %>%
#     pivot_longer(
#     contains("Y")
#     )  %>%
#     left_join(
#         true_weight_df %>% mutate(name = str_replace(name, "J", "Y")) %>% select(name, rel_weight), 
#         by = "name"
#     )  %>%
#     mutate(util = value*rel_weight) %>%
#     group_by(round, algorithm) %>%
#     summarise(util = sum(util)) %>%
#     bind_rows( 
#         ra_df
#     ) %>%
#     mutate(algorithm = factor(algorithm, levels = c("utility-weighted", "unweighted", "random-assignment"))) %>% 
#     filter(round > 1) %>%




# #######################
# # # Stan fit test 
# # N = 1000
# # J = 2
# # K = 4
# # shares = rep(1/K, K) 
# # beta = matrix(rnorm(J*K), nrow = K, ncol = J)
# # beta
# # sim_data = simulate_data(N, shares, beta, J = J)
# # sim_data$D
# # stan_data = list(
# #     N = N,
# #     J = 2,
# #     K = K,
# #     y = as.matrix(sim_data %>% select(contains("Y_"))),
# #     treat_arm = sim_data$D
# # )

# # stan_fit = ts_model$sample(
# #     data = stan_data
# # )