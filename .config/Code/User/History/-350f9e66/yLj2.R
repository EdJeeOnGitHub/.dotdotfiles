

library(testthat)
library(rstan)
library(tidyverse)
library(broom)
library(tidybayes)
library(furrr)
options(mc.cores = 4)
source("code/sbc-functions.R")
rstan_options(auto_write = TRUE)



default_stan_model = stan_model("stan/selection-mvn.stan")
heckit_model = stan_model("stan/rw-heckit.stan")

default_data_args = list(N = 60, p = 2, q = 2, cutoff_quantile = 0.5, d = 4)
default_params_args = list(mu_beta = 0, sd_beta = 1)
default_modelled_data_args = list(selection_type = "quantile", rubin = FALSE, selection_d = 2)


first_draw = simulate_draw(
  seed = 2,
  stan_model = default_stan_model,
  gen_data_args = default_data_args,
  gen_params_args = default_params_args,
  gen_modelled_data_args = default_modelled_data_args,
  fit_model = TRUE,
  fit_heckit = TRUE
)
