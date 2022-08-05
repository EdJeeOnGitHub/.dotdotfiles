

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

default_data_args = list(N = 60, p = 2, q = 2, cutoff_quantile = 0.5)
default_params_args = list(mu_beta = 0, sd_beta = 1)
default_modelled_data_args = list(selection_type = "quantile", rubin = FALSE)
