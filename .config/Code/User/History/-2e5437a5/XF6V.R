library(tidyverse)

# circles only
raw_rural_df = read_csv("power_rural.csv") %>%
    janitor::clean_names() %>%
    rename(locality = charges) %>%
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
    filter(str_detect(locality, "CIRCLE")) %>%
    select(
        district,
        locality,
        pop,
        lit
    )

joint_df = bind_rows(
    raw_rural_df %>% select(locality, pop, lit),
    raw_urban_df %>% select(locality, pop, lit)
) %>%
    mutate(
        unique_id = 1:n(), 
        lit = lit/100
        ) %>%
    mutate(
        locality_var = lit*(1 - lit)*pop
    )



between_variance = joint_df %>%
    summarise( 
        between_variance = mean((lit - weighted.mean(lit, w = pop))^2)
    ) %>%
    pull()
between_variance

within_variance = joint_df %>%
    summarise(within_variance = sum(locality_var)/(sum(pop) - n())) %>%
    pull()

rho = between_variance / (between_variance + within_variance)


## MDE_k

# MDE_k = sqrt(1 + rho(m-1)) * (t_{1-k}, + t_{\alpha/2})(1/p_c + 1/p_t)^0.5 * \sigma/\sqrt(N)
# MDE_k = sqrt(1 + rho(m(CV^2 + 1) - 1)) * (t_{1-k}, + t_{\alpha/2})(1/p_c + 1/p_t)^0.5 * \sigma/\sqrt(N)


urban_mean_sujawal_block_size = 325.15
urban_mean_karachi_west_block_size = 3531

urban_CV_sujawal = 0.71
urban_CV_karachi_west = 1.62



calculate_simple_MDE = function(rho, 
                                m,
                                kappa,
                                alpha,
                                p_c,
                                p_t,
                                sigma,
                                N){
    DE = sqrt(1 + rho*(m - 1))
    t_bit = 



}
