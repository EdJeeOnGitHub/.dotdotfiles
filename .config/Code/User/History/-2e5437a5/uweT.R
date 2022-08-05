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

rho

within_variance
total_variance = joint_df %>%
    summarise(total_variance = var(literacy_all))

joint_df %>%
    mutate(p = literacy_all / 100, 
           n = round(rnorm(n(), mean = 1000, sd = 500))) 
 

raw_urban_df %>%
    select(literacy_all)

raw_urban_df %>%
    select(literacy_all) %>%
    mutate(lit = as.numeric(literacy_all)) %>%
    filter(is.na(lit))
raw_rural_df %>%
    mutate(lit = as.numeric(literacy_all))  %>%
    filter(is.na(lit)) %>%
    View()
joint_df

raw_rural_df %>%
    filter(district == "DADU DISTRICT")


