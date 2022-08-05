library(tidyverse)
library(broom)
library(furrr)


rural_df = read_csv("clean_rural_data.csv")
urban_df = read_csv("clean_urban_data.csv")

karachi_east_df = urban_df %>%
    filter(district == "KARACHI EAST DISTRICT") 

rural_kambar_df = rural_df %>%
    filter(str_detect(district, "KAMBAR")) %>%
    filter(!(is.na(pop) | is.na(lit))) # two observations



karachi_east_df %>%
    group_by(subdivision_id) %>%
    summarise(
        charges = n_distinct(charge_id), 
        circles = n_distinct(circle_id), 
        pop = sum(pop, na.rm = TRUE)
    ) 

rural_kambar_df %>%
    group_by(taluka_id) %>%
    summarise(
        stc = n_distinct(stc_id), 
        tc = n_distinct(tc_id), 
        vills = n(),
        pop = sum(pop, na.rm = TRUE)
    )  %>%
    mutate(tot_stc = sum(stc), tot_tc = sum(tc))

#####################


complement <- function(y, rho, x) {
  if (missing(x)) x <- rnorm(length(y), mean = 0, sd = 0.1) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  x_tilde = rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
  x = pmax(0, x_tilde - mean(x_tilde) + 0.05)
  return(x)
}

calculate_alpha_beta = function(rho, pi_pr) {
    alpha = ((1 - rho)/(rho))*pi_pr
    beta = ((1 - rho)/(rho))*(1 - pi_pr)

    return(list(
        "alpha" = alpha,
        "beta" = beta
    ))
}

prep_data = function(df){
    df = df %>%
        mutate(
            unique_id = 1:n(), 
            lit = lit/100,
            ) %>%
        mutate(
            locality_var = lit*(1 - lit)*pop
        )
    df %>%
        filter(is.na(lit) | is.na(pop))
    between_variance = df %>%
        group_by(district) %>%
        summarise( 
            between_variance = mean((lit - weighted.mean(lit, w = pop))^2)
        ) 

    within_variance = df %>%
        group_by(district) %>%
        summarise(within_variance = sum(locality_var)/(sum(pop) - n())) 


    rho = inner_join(
        between_variance,
        within_variance
    ) %>%
        mutate(
            rho = (between_variance / (between_variance + within_variance))
        )
rho
    df = df %>%
        left_join( 
            rho
        )
    df = df %>%
        mutate(
            sim_pr_mean = complement(lit, -0.9), 
            alpha = (calculate_alpha_beta(rho = rho, pi_pr = sim_pr_mean))$alpha,
            beta = (calculate_alpha_beta(rho = rho, pi_pr = sim_pr_mean))$beta, 
            n_y = extraDistr::rbbinom(n = 1, size = round(pop*0.1), alpha = alpha, beta = beta)
        )  %>%
        ungroup() %>%
        mutate(row_id = 1:n())

    return(df)
}

simulate_N_0_vax = function(df, seed, block_size = 100){
    set.seed(seed)
    n_0_vax = extraDistr::rbbinom(
        n = nrow(df),
        size = block_size,
        alpha = df$alpha,
        beta = df$beta
    )
    n = block_size
    row_id = 1:nrow(df)
    return(lst(n, n_0_vax, n_vax = n - n_0_vax, row_id))
}



sample_plan = function(df,
                       total_units_to_sample,
                       units_per_group){
    n_groups = floor(total_units_to_sample / units_per_group)
    n_strat_var = unique(df$strat_id) %>%
        length()
    strat_ids = unique(df$strat_id)
    group_ids = unique(df$group_id)

    if (n_groups < n_strat_var) {
        stop("Not enough groups to sample each stratum")
    }
    n_groups_per_strata =  floor(n_groups / n_strat_var)
    # n_extra_groups = n_groups %% n_strat_var 

    sampled_group_var = df %>%
        group_by(strat_id) %>% 
        select(group_id, strat_id) %>%
        unique() %>%
        sample_n(n_groups_per_strata) %>%
        ungroup() %>%
        select(group_id) %>%
        pull()
    
    available_df = df %>%
        filter( group_id %in% sampled_group_var) %>%
        group_by(group_id) %>%
        sample_n(pmin(units_per_group, n()))  %>%
        ungroup() %>%
        select(row_id, group_id, strat_id)

    n_sampled_so_far = nrow(available_df)

    extra_required = total_units_to_sample - n_sampled_so_far
    extra_n_per_strata = floor(extra_required / n_strat_var)

    selected_row_ids = unique(available_df$row_id)

    extra_available_df = df %>%
        filter(!(row_id %in% selected_row_ids)) %>%
        group_by(strat_id) %>%
        sample_n(extra_n_per_strata) %>%
        select(row_id, group_id, strat_id)



    final_n_extra = total_units_to_sample - nrow(available_df) - nrow(extra_available_df)



    final_df = bind_rows(
        available_df,
        extra_available_df,
        df %>%
            filter(!(row_id %in% extra_available_df$row_id)) %>%
            filter(!(row_id %in% available_df)) %>%
            sample_n(final_n_extra) %>%
            select(row_id, group_id, strat_id)
    )
    
    if (nrow(final_df) != total_units_to_sample) {
        stop("Too many or few individuals")
    }
    return(final_df)
}

create_indiv_data = function(sim_data){
    subset_sim_data = sim_data
    outcome_matrix = matrix(NA, sum(subset_sim_data$n), ncol = 3)

    u_start = 1
    for (i in 1:nrow(subset_sim_data)){
        outcome = c(rep(1, subset_sim_data[[i, "n_0_vax"]]), rep(0, subset_sim_data[[i, "n_vax"]])) 
        row_id = rep(subset_sim_data[[i, "row_id"]], subset_sim_data[[i, "n"]])
        group_id = rep(subset_sim_data[[i, "group_id"]], subset_sim_data[[i, "n"]])
        length(outcome)
        length(row_id)
        u_end = u_start + length(outcome) - 1 
        outcome_matrix[u_start:u_end, 1] = outcome
        outcome_matrix[u_start:u_end, 2] = row_id
        outcome_matrix[u_start:u_end, 3] = group_id
        u_start = u_end + 1
    }
    colnames(outcome_matrix) = c("outcome", "row_id", "group_id")
    outcome_df = as_tibble(outcome_matrix)
    return(outcome_df)
}

calculate_mean_and_se = function(sim_data_draw){
    if (var(sim_data_draw$outcome) == 0) {
        return(tibble(singulat = TRUE))
    }
    tidy_fit = broom::tidy(fixest::feols(data = sim_data_draw, outcome ~ 1, cluster = ~row_id, nthreads = 1), conf.int = TRUE)
    return(tidy_fit)
}


sim_function = function(seed,
                        df, 
                        block_size,
                        total_units_to_sample,
                        units_per_group,
                        fit_model = TRUE
                        ){
    sim_data = as_tibble(simulate_N_0_vax(df, seed, block_size))
    sim_plan = sample_plan(
        df, 
        total_units_to_sample = total_units_to_sample,
        units_per_group = units_per_group) %>%
        mutate(in_sample = TRUE)
    sim_data_plan = left_join(
        sim_data, 
        sim_plan, 
        by = "row_id" 
    ) %>%
    filter(in_sample == TRUE) 
    
    if (fit_model == FALSE) {
        return(sim_data_plan)
    } 
    indiv_data = create_indiv_data(sim_data_plan)
    
    realised_n_circles_per_charge = sim_plan %>%
        group_by(group_id) %>%
        summarise(realised_n_circles_per_charge = n()) %>%
        summarise(realised_n_circles_per_charge = mean(realised_n_circles_per_charge)) %>%
        pull()
    fit = calculate_mean_and_se(indiv_data) %>%
        mutate(
            n_unique_group_var = length(unique(sim_plan %>% select(group_id) %>% pull())),
            n_unique_units = length(unique(sim_plan$row_id)),
            realised_n_circles_per_charge = realised_n_circles_per_charge,
            n_unique_individuals = nrow(indiv_data), 
            mean_0_vax = mean(indiv_data$outcome)
        )
    return(fit)
}


prep_rural_kambar_df = prep_data(
    rural_kambar_df
) %>%
    mutate(
        strat_id = taluka_id,
        group_id = stc_id
    )

prep_karachi_east_df = prep_data(
    karachi_east_df
) %>%
    mutate(strat_id = subdivision_id, group_id = charge_id)

p_kambar_imp = prep_rural_kambar_df %>%
    ggplot(aes( 
        x = lit, 
        y = sim_pr_mean
    )) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs( 
        title = "Imputed Probability of 0 Vax vs Literacy", 
        subtitle = "kambar rural"
    )

p_karachi_imp = prep_karachi_east_df %>%
    ggplot(aes( 
        x = lit, 
        y = sim_pr_mean
    )) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs( 
        title = "Imputed Probability of 0 Vax vs Literacy",
        subtitle = "karachi east"
    )
prep_karachi_east_df %>%
    group_by(
        subdivision_id
    ) %>%
    summarise( 
        frac_charges = n_distinct(charge_id),
        n_circles = n_distinct(circle_id), 
        n_pop = sum(pop)
    )



poss_sim_function = possibly(sim_function, otherwise = tibble(FAIL = TRUE))
N_sim_function = function(N, params, df) {
    sim_df = future_map_dfr(
        1:N, 
        ~sim_function(
            seed = .x,
            df = df,
            block_size = params$block_size,
            total_units_to_sample = params$total_units_to_sample,
            units_per_group = params$units_per_group, 
            fit_model = TRUE
        ) %>%
        mutate(
            block_size = params$block_size,
            total_units_to_sample = params$total_units_to_sample,
            units_per_group = params$units_per_group
        ),
        .options = furrr_options(seed = TRUE)
    )
    return(sim_df)
} 


p_bar_karachi = prep_karachi_east_df %>%
    group_by(charge_id) %>%
    summarise( 
        n_circles = n_distinct(circle_id)
    ) %>%
    ggplot(aes( 
        x = n_circles
    )) +
    geom_bar() +
    scale_x_discrete(
        breaks = 1:10, 
        labels = 1:10, 
        limits = 1:10
    ) +
    theme_bw()
 
p_bar_rural_kambar = prep_rural_kambar_df %>%
    group_by(group_id) %>%
    summarise( 
        n_villages = n()
    ) %>%
    ggplot(aes( 
        x = n_villages
    )) +
    geom_bar() +
    theme_bw()


plan(
    list(
        tweak(
            multisession, workers = 2
        ),
        tweak(multisession, workers = 4)
    )
)

param_df = expand.grid( 
    units_per_group = 1:5, 
    total_units_to_sample = 10:30 
) %>%
    mutate( 
        min_cutoff = 4*units_per_group
    ) %>%
    as_tibble() %>%
    filter(min_cutoff <= total_units_to_sample)
param_df

karachi_east_sim_draws = future_pmap_dfr(
    list( 
        param_df$units_per_group,
        param_df$total_units_to_sample
    ),
    ~N_sim_function(
        100, 
        df = prep_karachi_east_df, 
        params = list(
            block_size = 100,
            units_per_group = ..1,
            total_units_to_sample = ..2
            )), 
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
)

# rural_kambar_sim_draws = future_map_dfr(
#     c(1, 2, 3, 4),
#     ~N_sim_function(
#         500, 
#         df = prep_rural_kambar_df, 
#         params = list(
#             block_size = 100,
#             units_per_group = .x,
#             total_units_to_sample = 30
#             )), 
#     .progress = TRUE,
#     .options = furrr_options(seed = TRUE)
# )

plan(sequential)
stop()

# old_sim_draws = sim_draws
sim_draws = bind_rows(
    karachi_east_sim_draws %>% mutate(type = "karachi"),
    # rural_kambar_sim_draws %>% mutate(type = "rural kambar")
)
karachi_east_sim_draws %>%
    filter(n_unique_individuals != 3000) 
# write_csv(
#     sim_draws,
#     "variable-sim-draws.csv"
# )

sim_draws = sim_draws %>%
    mutate( 
        coverage = conf.low < 0.05 & 0.05 < conf.high, 
        signif = p.value < 0.05 
    )
# bind_rows(
# rural_kambar_df  %>% 
# mutate(pla = "kambar"),
# karachi_east_df %>%
#     mutate(pla = "karachi")
# ) %>%
#     ggplot(aes( 
#         x = pop, 
#         fill = pla
#     )) +
#     geom_histogram(bins = 60) +
#     scale_x_log10()



sim_draws %>%
    select(n_unique_individuals) %>%
    unique()







# Why does this look like this
summ_sim_draws = sim_draws %>%
    group_by(type, units_per_group, total_units_to_sample) %>%
    summarise(
        rp = mean(coverage),
        mean_se = median(std.error),
        sd_se = sd(std.error),
        pr_signif = mean(signif),
        realised_n_units_per_group = mean(n_unique_units/n_unique_group_var),
        n_unique_group_var = mean(n_unique_group_var),
        n_unique_units = mean(n_unique_units),
        n_unique_individuals = mean(n_unique_individuals), 
        mean_0_vax = mean(mean_0_vax), 
        n_draws = n())  %>%
    mutate(
        sd = sqrt(pr_signif*(1-pr_signif)/n_draws)
    )


summ_sim_draws

summ_sim_draws %>%
    ggplot(aes( 
        x = total_units_to_sample, 
        y = mean_se,
        colour = factor(units_per_group)
    )) +
    geom_point(
        size = 5
    ) +
    theme_bw() +
    labs( 
        title = "Estimator Variance", 
        x = "Circles Per Charge", 
        y = "Mean Standard Error"
    ) +
    scale_x_discrete(
        breaks = 1:6, 
        labels = 1:6, 
        limits = 1:6
    ) +
    geom_hline(yintercept = 0.0175)

summ_sim_draws %>%
    ggplot(aes( 
        x = units_per_group, 
        y = mean_se,
        colour = factor(total_units_to_sample)
    )) +
    geom_point(
        size = 5
    ) +
    theme_bw() +
    labs( 
        title = "Estimator Variance", 
        x = "Circles Per Charge", 
        y = "Mean Standard Error"
    ) +
    scale_x_discrete(
        breaks = 1:6, 
        labels = 1:6, 
        limits = 1:6
    ) +
    geom_hline(yintercept = 0.0175)


summ_sim_draws %>%
    ggplot(aes( 
        x = total_units_to_sample, 
        y = mean_se,
        ymin = mean_se - 1.96*sd_se,
        ymax = mean_se + 1.96*sd_se,
        colour = factor(units_per_group)
    )) +
    geom_pointrange(
        # position = position_dodge(0.5)
    ) +
    theme_bw() +
    labs( 
        title = "Estimator Variance", 
        x = "N Blocks to Sample", 
        y = "Mean Standard Error",
        colour =  "Min Blocks per Charge",
        caption = "Datapoints missing where it's impossible to sample at least 5 blocks per charge
         across all 4 towns with only 15 total blocks for instance."
    ) +
    facet_wrap(~units_per_group)

summ_sim_draws %>%
    ggplot(aes( 
        x = units_per_group, 
        y = mean_se,
        ymin = mean_se - 1.96*sd_se,
        ymax = mean_se + 1.96*sd_se,
        colour = factor(total_units_to_sample)
    )) +
    geom_pointrange(
        position = position_dodge(0.5)
    ) +
    theme_bw() +
    labs( 
        title = "Estimator Variance", 
        x = "Min Blocks Per Charge", 
        y = "Mean Standard Error",
        colour =  "Total Blocks"
    )




summ_sim_draws %>%
    ggplot(aes( 
        x = units_per_group, 
        y = mean_se,
        ymin = mean_se - 1.96*sd_se,
        ymax = mean_se + 1.96*sd_se,
        colour = factor(total_units_to_sample)
    )) +
    geom_pointrange(
        position = position_dodge(0.4)
    ) +
    theme_bw() +
    labs( 
        title = "Estimator Variance", 
        x = "Circles Per Charge", 
        y = "Mean Standard Error"
    )

summ_sim_draws %>%
    ggplot(aes( 
        x = params_n_circles, 
        y = pr_signif,
        ymin = pr_signif - 1.96*sd, 
        ymax = pr_signif + 1.96*sd, 
        colour = type
    )) +
    geom_pointrange() +
    theme_bw() +
    labs( 
        title = "Powerh"
    )


summ_sim_draws %>%
    ggplot(aes( 
        x = n_circles_per_charge, 
        y = rp,
        ymin = rp - 1.96*sd, 
        ymax = rp + 1.96*sd
    )) +
    geom_pointrange() +
    theme_bw() +
    labs( 
        title = "Powerh"
    )


summ_sim_draws %>%
    ggplot(aes( 
        x = params_n_circles, 
        y = n_unique_circles
    )) +
    geom_point()

summ_sim_draws %>%
    ggplot(aes(
        x = params_n_circles, 
        y = pct_signif
    )) +
    geom_point() +
    theme_bw() +
    labs(title = "Power Vs Circles Per Charge")



sim_draws %>%  
    lm(
        data = .,
        p.value ~ params_n_circles  + n_unique_individuals
    ) %>%
    tidy()




sim_draws %>%
    lm(
        data = .,
        p.value ~ params_frac_charges   + n_unique_individuals
    ) %>%
    tidy()


sim_draws %>%  
    lm(
        data = .,
        p.value ~ factor(n_circles_per_charge)  + n_unique_individuals
    ) %>%
    tidy()



