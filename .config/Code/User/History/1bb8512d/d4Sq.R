
library(tidyverse)

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
    )

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
  if (missing(x)) x <- rnorm(length(y), mean = 0.05, sd = 0.1) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
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
        select(row_id)
    return(final_df)
}
joint_df %>%
    select(district, locality)
sim_data = map_dfr(1:10, 
               ~simulate_N_0_vax(joint_df) %>% as_tibble() %>% mutate(draw = .x))
sim_data
sim_sample_plan = map_dfr(
    1:10,
    ~sample_plan(joint_df, 0.5, 0.5) %>% mutate(draw = .x, in_sample = TRUE)
)

sim_sample_plan
sim

joint_df  %>%
    filter(str_detect(district, "EAST")) %>%
    summarise(sum(pop))
sim_data  = left_join(sim_data, sim_sample_plan)
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

