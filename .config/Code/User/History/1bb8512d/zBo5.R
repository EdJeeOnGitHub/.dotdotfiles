
library(tidyverse)
library(furrr)
library(broom)
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
    filter(place == "urban" & district == "KARACHI WEST DISTRICT")
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

joint_df = joint_df %>%
    mutate(
        sim_pr_mean = complement(lit, 0.9), 
        alpha = (calculate_alpha_beta(rho = rho, pi_pr = sim_pr_mean))$alpha,
        beta = (calculate_alpha_beta(rho = rho, pi_pr = sim_pr_mean))$beta, 
        n_y = extraDistr::rbbinom(n = 1, size = round(pop*0.1), alpha = alpha, beta = beta)
    )  %>%
    ungroup() %>%
    mutate(row_id = 1:n())
joint_df %>%
select(sim_pr_mean) %>%
summarise(mean(sim_pr_mean))

joint_df %>%
    ggplot(aes( 
        x = lit, 
        y = sim_pr_mean
    )) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs( 
        title = "Imputed 0 Vax Mean vs Literacy"
    )

joint_df %>%
    summarise( 
        cor = cor(lit, sim_pr_mean)
    )

joint_df %>%
    select(sim_pr_mean) %>%
    filter(sim_pr_mean < 0)

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

sample_plan = function(df, n_charges, n_circles_per_charge){
    sampled_charges = sample(df$charge_id, size = n_charges, replace = FALSE)

    available_df = df %>%
        filter(charge_id %in% sampled_charges)
    
    final_df = available_df %>%
        group_by(charge_id) %>%
        sample_n(pmin(n_circles_per_charge, n()))  %>%
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


sim_function = function(n_charges, n_circles_per_charge, prop_sample){
    sim_data = as_tibble(simulate_N_0_vax(joint_df))
    sim_plan = sample_plan(
        joint_df, 
        n_charges = n_charges, 
        n_circles_per_charge = n_circles_per_charge) %>%
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
            n_circles_per_charge = n_circles_per_charge,
            n_unique_individuals = nrow(indiv_data), 
            mean_0_vax = mean(indiv_data$outcome)
        )
    return(fit)
}

poss_sim_function = possibly(sim_function, otherwise = tibble(FAIL = TRUE))
N_sim_function = function(N, params) {
    sim_df = future_map_dfr(
        1:N, 
        ~poss_sim_function(
            n_charges = params$n_charges, 
            n_circles = params$n_circles,
            prop_sample = params$prop_sample
        ) %>%
        mutate(
            n_charges = params$n_charges, 
            n_circles = params$n_circles,
            prop_sample = params$prop_sample 
        ),
        .options = furrr_options(seed = TRUE)
    )
    return(sim_df)
} 

n_charges_total = joint_df %>%
    select(charge_id) %>%
    unique() %>%
    nrow()

n_circles_total = joint_df %>%
    select(row_id) %>%
    unique() %>%
    nrow()
n_charges_total
n_circles_total

target_n_circles = 20
circles_per_charge = c(1, 2, 4, 5)
n_charges = round(target_n_circles/circles_per_charge)





param_grid = tibble(
    circles = circles_per_charge, 
    charges = n_charges, 
    prop_sample = 0.05
)

plan(
    list(
        tweak(
            multisession, workers = 2
        ),
        tweak(multisession, workers = 2)
    )
)

sim_draws = future_pmap_dfr(
    list(param_grid$charges, param_grid$circles, param_grid$prop_sample),
    ~N_sim_function(200, params = list(n_charges = ..1, n_circles = ..2, prop_sample = ..3)), 
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
)

sim_draws %>%
    filter(n_circles_per_charge > 4) %>%
    select(contains("n"))

sim_draws %>%
    filter(n_unique_circles == n_charges*n_circles_per_charge) %>%
    ggplot(aes( 
        x = n_unique_individuals
    )) +
    geom_histogram()

sim_draws %>%
    filter(n_unique_circles == n_charges*n_circles_per_charge) %>%
    ggplot(aes(x = n_circles_per_charge, y = p.value)) +
    geom_point()

summ_sim_draws = sim_draws %>%
    filter(n_unique_circles == n_charges*n_circles_per_charge) %>%
    group_by(n_circles_per_charge, n_charges, prop_sample) %>%
    summarise(
        pct_signif = mean(p.value < 0.05), 
        mean_p = mean(p.value),
        n_unique_charges = mean(n_unique_charges),
        n_unique_circles = mean(n_unique_circles),
        realised_n_circles_per_charge = mean(n_unique_circles/n_unique_charges),
        n_unique_individuals = mean(n_unique_individuals), 
        mean_0_vax = mean(mean_0_vax)) 

summ_sim_draws


sim_draws %>%
 select(
    n_unique_charges,
    n_unique_circles
 ) %>%
 tail()

summ_sim_draws %>%
    ggplot(aes(
        x = n_circles_per_charge, 
        y = pct_signif
    )) +
    geom_point()

sim_draws %>%
select( 
    n_charges, 
    n_circles, 
    n_circles_per_charge
) %>%
filter(n_circles_per_charge != n_circles)    


sim_draws %>%  
    lm(
        data = .,
        p.value ~ n_circles_per_charge  + n_unique_individuals
    ) %>%
    tidy()


sim_draws %>%
    lm(
        data = .,
        p.value ~ n_charges   + n_unique_individuals
    ) %>%
    tidy()



sim_draws %>%  
    lm(
        data = .,
        p.value ~ factor(n_circles_per_charge)  + n_unique_individuals
    ) %>%
    tidy()
