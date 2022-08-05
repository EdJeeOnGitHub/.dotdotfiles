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

# Fit Bayesian models with age covariate
stan_df = clean_df %>% sample_frac(0.1) # to run faster
prep_stan_df = stan_df %>% 
    mutate(const = 1) %>% 
    select(const, wtreatment, age_month, left_interval, right_interval, study, site_id, censoring) %>% 
    mutate(age_month = (age_month - mean(age_month, na.rm = TRUE))/sd(age_month, na.rm = TRUE)) %>%
    # mutate(age_month = ifelse(age_month <= 12, 1, 0)) %>%
    na.omit() %>%
    group_by(site_id) %>%
    mutate(reindex_site_id = cur_group_id()) %>%
    ungroup()
surv_model = cmdstan_model("survival/hierarchical-survival.stan")

stan_data = list(
  interval_left = prep_stan_df$left_interval,
  interval_right = prep_stan_df$right_interval,
  N = nrow(prep_stan_df),
  J = length(unique(prep_stan_df$study)),
  nc = 3,
  site = prep_stan_df$reindex_site_id, 
  X = prep_stan_df %>% 
    select(const, wtreatment, age_month) %>%
    as.matrix(),
  censoring = prep_stan_df$censoring,
  model_type = 1  # Just exponential for now
)

surv_fit = surv_model$sample(stan_data, chains = 1, refresh = 20, show_message = TRUE)
stan_draws = surv_fit %>% gather_draws(beta[site_id, k],
  hazard_ratio[site_id, k])  # y_rep[n]
