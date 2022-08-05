library(tidyverse)
library(cmdstanr)
options(mc.cores = 4)


all_df = read_csv("mortality_counts/individual_data.csv", guess_max = 10000)


cleaned_studies = c("Dupas et al. 2021", "Luby et al. 2006", "Peletz et al. 2012")

df = all_df %>%
    filter(study %in% cleaned_studies)

df %>%
    select(wtreatment)
df %>% 
    colnames()
clean_df = df %>% 
    select(study, event_time, time_to_event, wtreatment, follow_up_months, t, death) %>%
    mutate(
        left_interval = time_to_event,
        right_interval = time_to_event + 1,
        left_interval = if_else(death == 0, follow_up_months, left_interval), 
        right_interval = if_else(death == 0, Inf, right_interval)
    )  %>%
    group_by(study) %>%
    mutate(site_id = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(death)) %>%
    mutate(censoring = if_else(death == 1, 0, 1))


weibull_model = cmdstan_model("stan/hierarchical-weibull.stan")
# exp_model = cmdstan_model("stan/simple-weibull.stan")

stan_data = list(
    interval_left = clean_df$left_interval/100,
    interval_right = clean_df$right_interval/100,
    N = nrow(clean_df),
    J = length(unique(clean_df$study)),
    nc = 2,
    nsc = 100,
    site = clean_df$site_id, 
    X = clean_df %>% mutate(const = 1) %>% select(const, wtreatment) %>% as.matrix(),
    censoring = clean_df$censoring,
    beta_mean = c(0, 0),
    beta_sigma = c(100, 100)
)

weibull_fit = weibull_model$sample(
    stan_data
)



as_tibble(
    anon_f_data$modeled_data,
   ) %>%
   bind_cols(
       anon_f_data$data$censoring
   )


clean_df %>%
    select(left_interval, right_interval, censoring)

weibull_fit = weibull_model$sample(
    stan_data,
    chains = 1
)
