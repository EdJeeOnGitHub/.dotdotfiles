
library(tidyverse)
library(furrr)

# circles only
raw_rural_df = read_csv("power_rural.csv") %>%
    janitor::clean_names() %>%
    rename(locality = blocks) %>%
    mutate(pop = str_remove_all(pop, ",")) %>%
    mutate(
        across(c(pop, lit), as.numeric),
    ) %>%
    filter(!is.na(pop)) %>%
    filter(!is.na(lit)) %>%
    select(
        district,
        locality,
        pop,
        lit
    ) %>%
    filter(!(district ==  "09523"))

# has non-circles
raw_urban_df = read_csv("power_urban.csv") %>%
    janitor::clean_names() %>%
    rename(locality = charges) %>%
    filter(str_detect(locality, "CHARGE|CIRCLE")) %>%
    mutate(charge_present = str_detect(locality, "CHARGE")) %>%
    mutate(
        charge_id = cumsum(charge_present) 
    ) %>%
    filter(str_detect(locality, "CIRCLE")) %>%
    select(
        district,
        locality,
        pop,
        lit,
        charge_id
    )

joint_df = bind_rows(
    raw_rural_df %>% select(district, locality, pop, lit) %>% mutate(place = "rural"),
    raw_urban_df %>% select(district, locality, pop, lit, charge_id) %>% mutate(place = "urban")
) %>%
    mutate(
        unique_id = 1:n(), 
        lit = lit/100,
        lit = pmin(lit + 0.3, 1)
        ) %>%
    mutate(
        locality_var = lit*(1 - lit)*pop
    ) %>%
    # remove this when we have rural charges
    filter(place == "urban")

mean_literacy = joint_df %>%
    summarise(mean_lit = mean(lit)) %>%
    pull(mean_lit)


joint_df %>%
    select(district, place) %>%
    unique()


between_variance = joint_df %>%
    group_by(district, place) %>%
    summarise( 
        between_variance = mean((lit - weighted.mean(lit, w = pop))^2)
    ) 

within_variance = joint_df %>%
    group_by(district, place) %>%
    summarise(within_variance = sum(locality_var)/(sum(pop) - n())) 


rho = inner_join(
    between_variance,
    within_variance
) %>%
    mutate(
        rho = between_variance / (between_variance + within_variance)
    )



joint_df = joint_df %>%
    left_join( 
        rho
    )

complement <- function(y, rho, x) {
  if (missing(x)) x <- rnorm(length(y), mean = 0, sd = 0.05) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  x_tilde = rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
  x = pmin(0, x_tilde - mean(x_tilde) + 0.05)
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

joint_df = joint_df %>%
    mutate(
        sim_pr_mean = complement(lit, 0.93), 
        alpha = (calculate_alpha_beta(rho = rho, pi_pr = sim_pr_mean))$alpha,
        beta = (calculate_alpha_beta(rho = rho, pi_pr = sim_pr_mean))$beta, 
        n_y = extraDistr::rbbinom(n = 1, size = round(pop*0.1), alpha = alpha, beta = beta)
    )  %>%
    ungroup() %>%
    mutate(row_id = 1:n())

simulate_N_0_vax = function(df){
    n_0_vax = extraDistr::rbbinom(
        n = nrow(df),
        size = round(df$pop*0.1),
        alpha = df$alpha,
        beta = df$beta
    )
    n = round(df$pop*0.1)
    row_id = 1:nrow(df)
    return(lst(n, n_0_vax, n_vax = n - n_0_vax, row_id))
}

sample_plan = function(df, prop_charges, prop_circles){

    sampled_charges = sample(df$charge_id, size = round(length(unique(df$charge_id))*prop_charges), replace = FALSE)

    available_df = df %>%
        filter(charge_id %in% sampled_charges)
    
    final_df = available_df %>%
        group_by(charge_id) %>%
        sample_frac(prop_circles)  %>%
        ungroup() %>%
        select(row_id, charge_id)
    return(final_df)
}


create_indiv_data = function(sim_data, sample_frac){
    subset_sim_data = sim_data %>%
        mutate(across(.cols = c(n, n_0_vax, n_vax), ~round(.x*sample_frac))) %>%
        mutate(n = n_0_vax + n_vax) %>%
        filter(n > 0)
    outcome_matrix = matrix(NA, sum(subset_sim_data$n), ncol = 3)

    u_start = 1
    for (i in 1:nrow(subset_sim_data)){
        outcome = c(rep(1, subset_sim_data[[i, "n_0_vax"]]), rep(0, subset_sim_data[[i, "n_vax"]])) 
        row_id = rep(subset_sim_data[[i, "row_id"]], subset_sim_data[[i, "n"]])
        charge_id = rep(subset_sim_data[[i, "charge_id"]], subset_sim_data[[i, "n"]])
        length(outcome)
        length(row_id)
        u_end = u_start + length(outcome) - 1 
        outcome_matrix[u_start:u_end, 1] = outcome
        outcome_matrix[u_start:u_end, 2] = row_id
        outcome_matrix[u_start:u_end, 3] = charge_id
        u_start = u_end + 1
    }
    colnames(outcome_matrix) = c("outcome", "row_id", "charge_id")
    outcome_df = as_tibble(outcome_matrix)
    return(outcome_df)
}

calculate_mean_and_se = function(sim_data_draw){
    tidy_fit = broom::tidy(fixest::feols(data = sim_data_draw, outcome ~ 1, cluster = ~row_id, nthreads = 1))
    return(tidy_fit)
}


sim_function = function(prop_charges, prop_circles, prop_sample){
    sim_data = as_tibble(simulate_N_0_vax(joint_df))
    sim_plan = sample_plan(
        joint_df, 
        prop_charges = prop_charges, 
        prop_circles = prop_circles) %>%
        mutate(in_sample = TRUE)
    sim_data_plan = left_join(
        sim_data, 
        sim_plan, 
        by = "row_id"
    ) %>%
    filter(in_sample == TRUE) %>%
    select(-in_sample)
    
    indiv_data = create_indiv_data(sim_data_plan, sample_frac = prop_sample)
    fit = calculate_mean_and_se(indiv_data) %>%
        mutate(
            n_unique_charges = length(unique(sim_plan$charge_id)),
            n_unique_circles = length(unique(sim_plan$row_id)),
            n_unique_individuals = nrow(indiv_data), 
            mean_0_vax = mean(indiv_data$outcome)
        )
    return(fit)
}


N_sim_function = function(N, params) {
    sim_df =future_map_dfr(
        1:N, 
        ~sim_function(
            prop_charges = params$prop_charges, 
            prop_circles = params$prop_circles,
            prop_sample = params$prop_sample
        ) %>%
        mutate(
            prop_charges = params$prop_charges, 
            prop_circles = params$prop_circles,
            prop_sample = params$prop_sample 
        ),
        .options = furrr_options(seed = TRUE)
    )
    return(sim_df)
} 

charge_prop_seq = seq(from = 0.1, to = 0.25, length.out = 5)
circle_prop_seq = seq(from = 0.1, to = 0.25, length.out = 5)
prop_sample = 0.1
param_grid = expand.grid(
    charge = charge_prop_seq,
    circle = circle_prop_seq,
    prop_sample = 0.1
) %>%
    as_tibble()
param_grid

plan(
    list(
        tweak(
            multisession, workers = 2
        ),
        tweak(multisession, workers = 2)
    )
)
sim_draws = future_pmap_dfr(
    list(param_grid$charge, param_grid$circle, param_grid$prop_sample),
    ~N_sim_function(10, params = list(prop_charges = ..1, prop_circles = ..2, prop_sample = ..3)), 
    .progress = TRUE
)

summ_sim_draws = sim_draws %>%
    group_by(prop_circles, prop_charges, prop_sample) %>%
    summarise(
        pct_signif = mean(p.value < 0.05), 
        mean_p = mean(p.value),
        n_unique_charges = mean(n_unique_charges),
        n_unique_circles = mean(n_unique_circles),
        n_unique_individuals = mean(n_unique_individuals), 
        mean_0_vax = mean(mean_0_vax)) 
summ_sim_draws %>%
    select(contains("n_"), )

ed = N_sim_function(10, params = list(prop_charges = 0.5, prop_circles = 0.5, prop_sample = 0.8))
ed


example = create_indiv_data(sim_data %>% filter(draw == 1), 0.1 ) 
example %>%
    calculate_mean_and_se()

sd(example$outcome)/sqrt(nrow(example))

example %>%
    summarise(mean(outcome))

sim_data %>%
    mutate( 
        sample_frac = 0.2
    )



sim_data %>%
    filter(in_sample == TRUE) %>%
    mutate(across(.cols = c(n, n_0_vax, n_vax), ~round(./2))) %>%
    group_by(draw) %>%
    summarise( 
        n_0_vax = sum(n_0_vax),
        n_total = sum(n),
        pr = n_0_vax/n_total, 
        var = n_total*(pr)*(1 - pr),
        sd = sqrt(var)
    )


    sim_data


    group_by(draw)
joint_df







simulate_N_0_vax(joint_df)

    joint_df %>%
        select(alpha, beta)
        select(pop)
extraDistr::rbbinom(n = 1000, size = round(3793*0.1), alpha = 0.264, beta = 10.7)

joint_df
joint_df %>%
    select(n_y) %>%
    unique()

joint_df %>%
    ggplot(aes(x = n_y)) +
    geom_histogram()

extraDistr::rbbinom(1, size = pop*0.1, alpha = joint_df[[1, "alpha"]], joint_df[[1, "beta"]])





#####################################




alpha_beta_c = map(pi_p_c, ~calculate_alpha_beta(rho = rho, .x))
        # number of event by cluster
        n_y_c = map2_dbl(alpha_beta_c, n_c, ~rbbinom(n = 1, size = .y, alpha = .x$alpha, beta = .x$beta ))
        n_y_c
        # generate indiv level outcomes by cluster
        if (any(is.na(n_y_c))) {
            stop("NAs in the number of successes in a cluster.")
        }
        y_indiv = map2(n_c, n_y_c, ~c(rep(1, .y), rep(0, .x - .y))) 

