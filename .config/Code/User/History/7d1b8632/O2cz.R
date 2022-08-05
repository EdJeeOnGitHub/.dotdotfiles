library(tidyverse)
library(extraDistr)
library(furrr)
library(sandwich)
library(lmtest)
library(fixest)

#' Generate "Data" for simulations
#' 
#' Here data really means hyperparameters such as number of observations, 
#' number of covariates,#' 
#' @param N Number of total observations
meta_gen_data_function = function(N = 1000, 
                                  J = 3, 
                                  n_clusters = 1, 
                                  n_villages = 100, 
                                  randomisation_level = "individual",
                                  rho = 0, 
                                  pr_arm = rep(1/J, J),
                                  n_cluster_per_village = 3){
    function(seed) {
        set.seed(seed + 1e6)
        N = N
        J = J
        treatment_arm = sample(1:J, N, pr_arm, replace = TRUE)
        # there must be a more efficient way to do this
        # want to remap cluster id: 1, 3, 10 => 1, 2, 3 i.e. contig IDs
        cluster_df = tibble(
            cluster_id = sample(1:n_clusters, N, replace = TRUE)
        ) %>% 
            group_by(cluster_id) %>%
            mutate(contig_cluster_id = cur_group_id())
        cluster_id = cluster_df$contig_cluster_id
        max_cluster_id = max(cluster_id)

        if (n_clusters == 1) {
            village_id = rep(1:n_villages, length.out = N)
        } else {
            # each cluster has a village
            village_id_c = rep(1:n_villages, length.out = max_cluster_id)
            # now map into indiv level vill id
            village_id = village_id_c[cluster_id]
            # N.B. this doesn't quite follow what we want. Not guaranteed 3 
            # clusters per village for edge cases
        }



        # assign treatment at cluster level
        if (randomisation_level == "cluster") {
            treatment_arm_c = sample(1:J, max_cluster_id, replace = TRUE)
            treatment_arm = treatment_arm_c[cluster_id]

        } else {
            treatment_arm_c = treatment_arm
        }
    
        return(list(
            N = N,
            J = J,
            treatment_arm = treatment_arm,
            treatment_arm_c = treatment_arm_c,
            n_clusters = n_clusters,
            cluster_id = cluster_id,
            rho = rho,
            village_id = village_id
        ))
    }
}


#' Generate parameters 
#' 
#' These are the parameters we want to estimate.
meta_gen_params = function(TEs, baseline_mean){
    function(seed,
             data){

        set.seed(seed + 2e6)

        return(list(
            TEs = TEs,
            baseline_mean = baseline_mean
        ))
    
    }

}



#' Generate Modelled Data
#'
#'
#' Using hyper parameters from `gen_data` and parameters from `gen_params`,
#' create our modelled data. That is, create binary outcome data according to the generative model. 
#' 
#'  ICC DGP uses draws from beta-bernoulli distribution. Y is the number of 
#' events within a cluster and alpha and beta can be chosen to give a pr(success) 
#' and rho in each cluster where rho is ICC.
#' 
meta_gen_modeled_data_function = function(dgp = "logit"){
    function(seed,
             data,
             params) {
        set.seed(seed + 3e6)

    
        TEs = params$TEs
        treatment_arm = data$treatment_arm
        mu_vec = params$baseline_mean + TEs[treatment_arm]
        rho = data$rho
        if (dgp == "probit") {
            p_vec = pnorm(mu_vec)
        } 
        if (dgp == "logit") {
            p_vec = exp(mu_vec)/(1 + exp(mu_vec))
        }
        if (dgp == "icc") {
            # This is a mess...
            cluster_id = data$cluster_id
            # create mapping of indiv id, cluster id, and id within cluster
            cluster_df = tibble(
                cluster_id
            ) %>%
                mutate(indiv_i_id = 1:n()) %>%
                group_by(cluster_id) %>%
                mutate(indiv_c_id = 1:n())
            # count how many people per cluster
            n_c = tibble(
                cluster_id = cluster_id) %>%
                group_by(cluster_id) %>%
                summarise(n = n()) %>%
                select(n) %>%
                pull()
            # create unique cluster index sorted from 1:n_cluster
            cluster_c = sort(unique(cluster_id))
            # treatment arm by cluster
            treatment_arm_c = data$treatment_arm_c
            # prob of vaccination by cluster
            pi_p_c = pmin(params$baseline_mean + TEs[treatment_arm_c], 1)
            # alpha and beta for beta binomial dist by cluster 
            alpha_beta_c = map(pi_p_c, ~calculate_alpha_beta(rho = rho, .x))
            # number of event by cluster
            n_y_c = map2_dbl(alpha_beta_c, n_c, ~rbbinom(n = 1, size = .y, alpha = .x$alpha, beta = .x$beta ))
            # generate indiv level outcomes by cluster
            if (any(is.na(n_y_c))) {
                stop("NAs in the number of successes in a cluster.")
            }
            y_indiv = map2(n_c, n_y_c, ~c(rep(1, .y), rep(0, .x - .y))) 
            # index through indiv data by cluster using ids generated in cluster_df
            y = map2_dbl(cluster_df$cluster_id, cluster_df$indiv_c_id, ~y_indiv[[.x]][[.y]])

        } else {
            y = rbinom(n = data$N, size = 1, prob = p_vec) 
        }


        return(list(
            y = y
        ))
    }

}


simulate_data = function(seed = 1234,
                         hyper_params){ 

    gen_data = meta_gen_data_function(
        N = hyper_params$N,
        J = hyper_params$J,
        rho =  hyper_params$rho,
        n_clusters = hyper_params$n_clusters
    )
    gen_modeled_data = meta_gen_modeled_data_function(dgp = hyper_params$dgp)
    gen_params = meta_gen_params(TEs = hyper_params$TEs, baseline_mean = hyper_params$baseline_mean)
    data = gen_data(seed)
    params = gen_params(seed,
                        data)
    modeled_data = gen_modeled_data(seed,
                                    data,
                                    params)
    sim_data_list = c(
        data,
        params,
        modeled_data
    )
    return(sim_data_list)
}


#' Fit Estimators 
#'  
#'  Should return a tidy table of results.
#' 
#' Bit of a mess atm as would like to make this more modular.
#' 
#' ATM have commented out everything but clustered OLS. TODO
#' 
fit_estimators = function(sim_data) {
    sim_df = tibble(
        y = sim_data$y,
        treatment_arm = factor(sim_data$treatment_arm),
        cluster_id = sim_data$cluster_id
    )
    # If everyone is vaccinated/no one is vaccinated report back an error
    if (var(sim_df$y) == 0) {
        tidy_tests = tibble(
            fail = TRUE
        )
    } else {
        # tidy_chisq = chisq.test(sim_df$y, sim_df$treatment_arm)  %>%
        #     broom::tidy() %>%
        #     mutate(
        #         term = paste0("factor(treatment_arm)", parameter + 1),
        #         vcv = "standard",
        #         model = "chisq")  %>%
        #     select(-parameter, -method)

        lm_fit = lm(y ~ factor(treatment_arm), data = sim_df)
        # logit_fit = glm(y ~ factor(treatment_arm), family = binomial(link = "logit"), data = sim_df)

        cluster_tests = map_dfr(
            list(
                lm_fit
                # logit_fit
            ),
            ~coeftest(.x, cluster = ~cluster_id, vcov = vcovCL) %>% broom::tidy() %>% mutate(vcv = "cluster", model = class(.x)[1])
        )
        # robust_tests = map_dfr(
        #     list(
        #         lm_fit,
        #         logit_fit
        #     ),
        #     ~coeftest(.x,  vcov = vcovHC) %>% broom::tidy() %>% mutate(vcv = "standard", model = class(.x)[1])
        # )
        tidy_tests = bind_rows(
            # tidy_chisq,
            cluster_tests
            # robust_tests
        ) 
    }
    mean_df = sim_df %>%
        group_by(treatment_arm) %>%
        summarise(mean = mean(y))
    return(list(
        tidy_tests = tidy_tests,
        mean_df = mean_df
    ))
}



#' Generate default hyper parameters
#'  
#'  Handy function to generate a set of defaults but you can pass anything to 
#' just update that parameter. Why isn't this just a function with default arguments?
#' Good question.
#' 
#'  This is why we should learn how to make classes in R.
#' 
#' Note that when using ICC baseline_mean is in probability space (0.1 indicates 
#' 10% baseline uptake). When using logit/probit this is in logit/probit space 
#' so you need to use log(1/9) for logit etc (i.e. log odds).
#' 
default_hyper_params = function(...){
    J = 2
    hp = list(
        N = 2250,
        TEs = c(0, 2),
        J = J,
        n_clusters = floor(2250/8),
        dgp = "logit", 
        rho = 0.5,
        baseline_mean = log(1/9),
        pr_arm = rep(1/J, J)
    )
    new_vals = list(...)
    hp = modifyList(hp, new_vals)
    return(hp)
}
calculate_alpha_beta = function(rho, pi_pr) {
    alpha = ((1 - rho)/(rho))*pi_pr
    beta = ((1 - rho)/(rho))*(1 - pi_pr)

    return(list(
        "alpha" = alpha,
        "beta" = beta
    ))
}


# Calculate ICC just to verify our beta-binomial model is generating correct rhos
icc_aov = function(y, cid) {
    k = length(unique(cid))

    # Number of observations in each cluster
    ni <- as.vector(table(cid))
    # Total number of observations
    N <- sum(ni)


    # ::: ANOVA method :::

    n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
    yi <- aggregate(y, by = list(cid), sum)[ , 2]
    yisq <- yi^2
    msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
    msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))
    rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)
    return(rho.aov)
}



#'  Handy wrapper function for power simulation.
#' 
#'  Give it a set of hyper params and number of draws and it will just 
#' generate S draws from that set of hyper params.
simulate_power = function(S, hyper_params) {
    sim_draws = map(
        1:S,
        ~simulate_data(seed = .x, hyper_params = hyper_params)
    )

    sim_fits = map(
        sim_draws,
        fit_estimators
        )

    test_fits = map_dfr(sim_fits, "tidy_tests")
    mean_dfs = map_dfr(sim_fits, "mean_df")
    
    subset_test_fits = test_fits %>%
        filter(str_detect(term, "treatment_arm")) %>%
        select(p.value, model, term, vcv)

    summ_test_fits = subset_test_fits %>%
        group_by( 
            model, vcv
        ) %>%
        summarise(
            pr_signif = mean(p.value < 0.05),
            .groups = "drop"
        ) 
    treatment_means = mean_dfs %>%
        group_by(treatment_arm) %>%
        summarise(mean = mean(mean)) %>%
        select(mean) %>%
        pull()

    n_NA = test_fits %>%
        filter(is.na(p.value)) %>%
        nrow()
    

    res = c(
        hyper_params[!str_detect(names(hyper_params), "TEs|pr_arm")],
        "TEs" = hyper_params$TEs,
        "pr_arm" = hyper_params$pr_arm,
        "treatment_means" = treatment_means,
        "n_NA" = n_NA
    ) 
    result_df = bind_cols(
        summ_test_fits,
        as_tibble(res)
    )
    return(result_df)
}


### Old scratch stuff
# #-------------------------------------------------------------------------------

TE_grid = seq(from = 0.0, to = 0.2, length.out = 50)

mde_grid = expand.grid(
    TE = TE_grid,
    dgp = "icc",
    N = seq(from = 400, to = 2000, by = 200),
    # baseline_mean = c(log(1/9), log(3/7), log(5/5), log(7/3))
    baseline_mean  = 0.10,
    rho = c(0.1, 0.25, 0.5, 0.75, 0.9, 1.0),
    pr_t = 1/3,
    pr_c = 2/3
) %>%
    as_tibble() %>%
    mutate(n_clusters = floor(N/8))
# plan(multisession, workers = 10)
example_mde_grid = mde_grid %>%
    head(1)

example_mde_sims = pmap_dfr(
    list(
        mde_grid$TE,
        ,
        mde_grid$baseline_mean,
        mde_grid$dgp,
        mde_grid$rho,
        mde_grid$n_clusters,
        mde_grid$pr_t,
        mde_grid$pr_c
    ),
    ~simulate_power(
        20,
        hyper_params = default_hyper_params(
            TEs = c(0, ..1),
            N = ..2,
            baseline_mean = ..3,
            dgp = ..4,
            rho = ..5,
            n_clusters = ..6,
            pr_arm = c(..7, ..8)
        )
    )
)

# mde_sims = future_pmap_dfr(
#     list(
#         mde_grid$TE,
#         mde_grid$N,
#         mde_grid$baseline_mean,
#         mde_grid$dgp,
#         mde_grid$rho,
#         mde_grid$n_clusters,
#         mde_grid$pr_t,
#         mde_grid$pr_c
#     ),
#     ~simulate_power(
#         20,
#         hyper_params = default_hyper_params(
#             TEs = c(0, ..1),
#             N = ..2,
#             baseline_mean = ..3,
#             dgp = ..4,
#             rho = ..5,
#             n_clusters = ..6,
#             pr_arm = c(..7, ..8)
#         )
#     ),
#     .progress = TRUE,
#     .options = furrr_options(seed = TRUE)
# )

# mde_sims %>%
#     mutate(diff = treatment_means2 - treatment_means1) 

# group_vars = c("N", "rho")


# create_mde_df = function(sim_data, group_vars){

#     mde_df = sim_data %>%
#         group_by(
#             across(.cols = all_of(group_vars))
#         ) %>%
#         filter(n_NA == 0) %>%
#         filter(pr_signif > 0.8) %>%
#         filter(pr_signif == min(pr_signif)) %>%
#         mutate(diff = mean(treatment_means2) - mean(treatment_means1)) %>%
#         summarise_if(is.numeric, mean)  %>%
#         ungroup()
#     return(mde_df)
# }
# mde_sims
# mde_sim_df = mde_sims %>%
#     create_mde_df(group_vars = c("N", "rho", "model", "vcv"))

# mde_sim_df %>%
#     filter(model == "chisq") %>%
#     ggplot(aes( 
#         x = N,
#         y = diff,
#         colour = factor(rho)
#     )) +
#     geom_point() +
#     geom_line() +
#     facet_grid(vcv ~ model) +
#     geom_hline(yintercept = 0.06, linetype = "longdash")

# mde_summ = mde_sims %>%
#     group_by( 
#         N, J, n_clusters, rho, baseline_mean, 
#         model, vcv
#     ) %>%
#     summarise(
#         pr_signif = mean(p.value < 0.05),
#         diff = mean(treatment_means2) - mean(treatment_means1)
#     ) 
# mde_summ  %>%
#     filter(pr_signif > 0.8) %>%
#     filter(pr_signif == min(pr_signif)) %>%
#     ggplot(aes(
#         x = rho,
#         y = diff,
#         colour = model,
#         linetype = vcv
#     )) +
#     geom_point() +
#     geom_line()

# mde_sims %>%
#     group_by( 
#         N, J, n_clusters, rho, baseline_mean, TEs2, 
#         model, vcv
#     ) %>%
#     summarise(
#         pr_signif = mean(p.value < 0.05)
#     ) %>%
#     filter(rho < 1, !is.na(model)) %>%
#     ggplot(aes( 
#         x = TEs2, 
#         y = pr_signif, 
#         colour = factor(rho),
#         group = factor(rho)
#     )) +
#     facet_grid(model ~ vcv) +
#     geom_point() +
#     geom_line()

# mde_sims %>%
#     group_by( 
#         N, J, n_clusters, rho, baseline_mean,  
#         model, vcv
#     ) %>%
#     summarise(
#         pr_signif = mean(p.value < 0.05), 
#         TEs2 = mean(TEs2)
#     ) %>%
#     filter(rho == 0.1) %>%
#     filter( rho < 1) %>% 
#     filter(pr_signif > 0.8) %>%
#     filter(pr_signif == min(pr_signif)) 

# mde_sims %>%
#     group_by(N, baseline_mean, rho) %>%
#     filter(pr_signif >= 0.8) %>%
#     filter(pr_signif == min(pr_signif)) %>%
#     summarise(
#         # p_1 = exp(baseline_mean)/(1 + exp(baseline_mean)),
#         p_1 = baseline_mean,
#         diff = mean(treatment_means2 - treatment_means1))  %>%
#     filter(N == 1200) %>%
#     summarise(diff = mean(diff))

# mde_sims %>%



# mde_sims %>%
#     mutate(diff = treatment_means2 - treatment_means1) %>%
#     filter(rho < 1) %>%
#     ggplot(aes( 
#         x = TEs2, 
#         y = pr_signif,
#         colour = factor(rho),
#         group = N
#     )) +
#     geom_point() +
#     # geom_line() + 
#     geom_hline(yintercept = 0.8)
# mde_sims %>%
#     mutate(diff = treatment_means2 - treatment_means1) %>%
#     filter(rho < 1) %>%
#     ggplot(aes( 
#         x = TEs2, 
#         y = pr_signif,
#         colour = factor(rho)
#     )) +
#     geom_point() +
#     geom_line() + 
#     geom_hline(yintercept = 0.8) +
#     facet_wrap(~N)

# mde_sims %>%
#     group_by(N, baseline_mean, rho) %>%
#     filter(pr_signif >= 0.8) %>%
#     filter(pr_signif == min(pr_signif)) %>%
#     summarise(
#         # p_1 = exp(baseline_mean)/(1 + exp(baseline_mean)),
#         p_1 = baseline_mean,
#         diff = mean(treatment_means2 - treatment_means1)) %>%
#     ggplot(aes( 
#         x = N,
#         y = diff,
#         colour = factor(rho)
#     )) +
#     geom_point() +
#     geom_line() +
#     theme_bw() +
#     labs(colour = "rho", 
#          y = "MDE(delta)",
#          title = "ICC Two Proportions Chi Squared Power",
#          subtitle = "Baseline Mean = 0.10")

# ggsave("data/output/plots/icc-two-prop.png", width = 8, height = 6)    





# mde_sims %>%
#     group_by(N, baseline_mean, rho) %>%
#     filter(pr_signif >= 0.8) %>%
#     filter(pr_signif == min(pr_signif)) %>%
#     summarise(
#         # p_1 = exp(baseline_mean)/(1 + exp(baseline_mean)),
#         p_1 = baseline_mean,
#         diff = mean(treatment_means2 - treatment_means1))  %>%
#     filter(N == 1200) %>%
#     summarise(diff = mean(diff))

# rho_grid = seq(from = 0, to = 1, length.out = 50)
# rho_fit = rho_grid %>%
#     map_dbl(
#         ~{ 
#             sim_df = simulate_data(
#             seed = 1, 
#             default_hyper_params(
#                 n_clusters = floor(2250/8),
#                 rho = .x,
#                 dgp = "icc",
#                 TEs = c(0, 0.2), 
#                 baseline_mean = 0.1
#             ))
            
#             rho = icc_aov(sim_df$y, sim_df$cluster_id)
#         }
       
#     )

# rho_df = tibble(
#     true_rho = rho_grid,
#     fit_rho = rho_fit
# )


# rho_df %>%
#     ggplot(aes( 
#         x = true_rho,
#         y = fit_rho
#     )) + 
#     geom_point() +
#     geom_abline()
# 1:10 %>%
#     map(
#         ~simulate_data(
#             seed = .x, 
#             default_hyper_params(
#                 n_clusters = floor(2250/8),
#                 rho = 0.2,
#                 dgp = "icc",
#                 TEs = c(0, 0.2), 
#                 baseline_mean = 0.1
#             )
#             ) %>%
#                 fit_estimators()
       
#     )

