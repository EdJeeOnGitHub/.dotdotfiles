# Load libraries
library(tidyverse)
library(cmdstanr)
library(ggstance)
library(lubridate)
library(survival)
library(flexsurv)
library(tidybayes)
library(tidyr)
library(bayesplot)
library(ggplot2)
library(rstanarm)
library(simsurv)
source("helpers.R")
options(mc.cores = 4)

# Load and clean data
all_df = read_csv("data/individual_data_full.csv", guess_max = 10000)

subset_df = all_df %>% filter(!str_detect(study, "Quick"))  # No events in Quick et al.

clean_df = subset_df %>%
  select(
    study, 
    event_time_lb,
    event_time_ub,
    follow_up_time, 
    treatment_time, 
    death, 
    wtreatment,
    age_month, 
    precision_evt
  ) %>%
  mutate(
    cens_event_time_lb = if_else(
      death == 0,
      follow_up_time,
      event_time_lb
    ),
    cens_event_time_ub = if_else(
      death == 0,
      follow_up_time,
      event_time_ub
    ),
    left_interval = interval(treatment_time, cens_event_time_lb) %/% days(1), 
    right_interval = interval(treatment_time, cens_event_time_ub) %/% days(1),
  ) %>%
  group_by(study) %>%
  mutate(site_id = cur_group_id()) %>%
  ungroup() %>%
  filter(!is.na(death)) %>%
  mutate(censoring = if_else(death == 0, 0, 
                             ifelse(left_interval == right_interval, 1, 3)), 
         right_interval = right_interval + ifelse(left_interval == 0, 1, 0),
         left_interval = left_interval + ifelse(left_interval == 0, 1, 0)) %>%
  filter(!is.na(left_interval) & !is.na(right_interval)) 

# Fit survival models
site_name = clean_df %>%
  select(study, site_id) %>%
  unique()

surv_model = cmdstan_model("survival-stan/hierarchical-survival-old.stan")

stan_draws = list()
for (i in 1:nrow(site_name)) {
  i = 1
  stan_df = clean_df  %>%
    mutate(left_interval = left_interval / 1000, 
           right_interval = right_interval / 1000)
  
  stan_data = list(
    N = nrow(stan_df),
    J = length(unique(stan_df$study)),
    nc = 2,
    site = stan_df$site_id, 
    X = stan_df %>% 
      mutate(const = 1) %>% 
      select(const, wtreatment) %>% 
      as.matrix(),
    censoring = stan_df$censoring,
    interval_left = stan_df$left_interval,
    interval_right = stan_df$right_interval,
    model_type = 2
  )
  
  surv_fit = surv_model$sample(stan_data) 
  # assuming 'fit' is from CmdStanR
stanfit <- rstan::read_stan_csv(surv_fit$output_files())
shinystan::launch_shinystan(stanfit)
  stan_draws[[i]] = surv_fit %>% gather_draws(beta[site_id, k],
    hazard_ratio[site_id, k], shape[k]) 
}
