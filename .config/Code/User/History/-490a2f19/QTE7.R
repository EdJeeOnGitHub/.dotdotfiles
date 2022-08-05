library(testthat)
library(rstan)
library(tidyverse)
library(broom)
library(tidybayes)
library(furrr)
options(mc.cores = 4)
source("code/sbc-functions.R")
rstan_options(auto_write = TRUE)


library(cmdstanr)
cmdstan_model("stan/mvn.stan")
default_stan_model = stan_model("stan/mvn.stan")


default_data_args = list(N = 200, p = 2, q = 2, d = 1, cutoff_quantile = 0.5)
default_params_args = list(mu_beta = 0, sd_beta = 1)
default_modelled_data_args = list(selection_type = "none", rubin = FALSE, selection_d = NA)



first_draw = simulate_draw(
  seed = 2,
  stan_model = default_stan_model,
  gen_data_args = default_data_args,
  gen_params_args = default_params_args,
  gen_modelled_data_args = default_modelled_data_args,
  fit_model = TRUE,
  fit_heckit = FALSE
)

first_sbc = create_sbc_output(first_draw$params, first_draw$bayes_fit, first_draw$data$N)
first_tidy = first_draw %>%
    tidy_sim_output()
test_that("Model works", {
  expect_s4_class(first_draw$bayes_fit, "stanfit")
  expect_s3_class(first_sbc, "tbl_df")
  expect_s3_class(first_tidy, "tbl_df")
})

plan(multisession, workers = 8)

simulated_draws = 1:100 %>%
  future_map(
    ~{sim_draw = simulate_draw(
      seed = .x,
      stan_model = default_stan_model,
      gen_data_args = default_data_args,
      gen_params_args = default_params_args,
      gen_modelled_data_args = default_modelled_data_args
      )
      
      
      if (is.null(sim_draw$bayes_fit)) {
        return(list(sbc = NULL, tidy_df = NULL))
      } 
      sbc_draw = create_sbc_output(
        sim_draw$params,
        sim_draw$bayes_fit,
        sim_draw$data$N
      ) %>%
        mutate(draw = .x)

       tidy_output = sim_draw %>%
        tidy_sim_output() %>%
        mutate(draw = .x)
        return(list(sbc = sbc_draw, tidy_df = tidy_output))
        },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE, packages = "broom")
  )


tidy_sim_draws = map_dfr(simulated_draws, "tidy_df")
sbc_draws = map_dfr(simulated_draws, "sbc")


## Tests ##
unif_test = sbc_draws %>%
  group_by(term, j, k) %>%
  summarise(ks_p = tidy(ks.test(rank_stat, "punif", 0, 1))$p.value, .groups = "drop") %>%
  mutate(ks_p_adj = p.adjust(ks_p, method = "BH")) 



error_df = tidy_sim_draws %>%
  mutate(error = estimate - true_value) %>%
  group_by(term, j, model) %>%
  summarise( 
    mean_error = mean(error),
    mae = mean(abs(error)), 
    mse = mean(error^2), 
    .groups = "drop"
  ) 


test_that("MVN Model Recovers Parameters", {

  expect_equal(
    error_df %>% 
      filter(term == "beta" | term == "gamma") %>%
      filter(mean_error > 0.2) %>%
      nrow(),
      0
  )

  expect_equal(
    unif_test %>%
      filter(ks_p_adj < 0.05) %>%
      nrow(),
      0
  )
})