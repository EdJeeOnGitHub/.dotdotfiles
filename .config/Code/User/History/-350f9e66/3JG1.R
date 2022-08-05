

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

first_draw$bayes_fit %>%
    gather_draws(gamma[j,k]) %>%
    median_qi()

first_draw$bayes_fit %>%
    gather_draws(Sigma[j,k]) %>%
    median_qi()
first_draw$params$gamma


first_sbc = create_sbc_output(first_draw$params, first_draw$bayes_fit, first_draw$data$N)
first_tidy = first_draw %>%
    tidy_sim_output()
test_that("Model works", {
  expect_s4_class(first_draw$bayes_fit, "stanfit")
  expect_s3_class(first_sbc, "tbl_df")
  expect_s3_class(first_tidy, "tbl_df")
})

first_tidy %>%
  mutate(error = true_value - estimate) %>%
  arrange(term, j) 




plan(multisession, workers = 8)

simulated_draws = 1:100 %>%
  future_map(
    ~{sim_draw = simulate_draw(
      seed = .x,
      stan_model = default_stan_model,
      gen_data_args = default_data_args,
      gen_params_args = default_params_args,
      gen_modelled_data_args = default_modelled_data_args,
      fit_heckit = TRUE
      )
      
      
      if (is.null(sim_draw$bayes_fit)) {
        return(list(sbc = NULL, tidy_df = NULL, N_observed = NULL))
      } 

      sbc_draw_bayes = create_sbc_output(sim_draw$params, sim_draw$bayes_fit, sim_draw$data$N) %>%
        mutate(draw = .x, model = "bayes") 
      sbc_draw_heckit = create_sbc_output(sim_draw$params, sim_draw$heckit_bayes_fit, sim_draw$data$N) %>%
        mutate(draw = .x, model = "heckit") 
      sbc_draw = bind_rows(
        sbc_draw_bayes,
        sbc_draw_heckit
      )
      N_observed = nobs(sim_draw$ols_fit)
       tidy_output = sim_draw %>%
        tidy_sim_output() %>%
        mutate(draw = .x)
        return(list(sbc = sbc_draw, 
                    tidy_df = tidy_output,
                    N_observed = N_observed))
        },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE, packages = "broom")
  )



tidy_sim_draws = map_dfr(simulated_draws, "tidy_df")
sbc_draws = map_dfr(simulated_draws, "sbc")
calibration_draws = map_dfr(simulated_draws, "calibration_res")
stop()
#### Plots #####
sbc_draws

tidy_sim_draws %>%
    filter()


tidy_sim_draws %>%
  mutate(error = true_value - estimate) %>%
  mutate(coverage = conf.low < true_value & true_value < conf.high) %>% 
  group_by(term, j, k, model) %>%
  summarise( 
    mean_error = mean(error),
    mae = mean(abs(error)), 
    mse = mean(error^2),
    med_ae = median(abs(error)),
    rp = mean(coverage, na.rm = TRUE) 
  ) 
