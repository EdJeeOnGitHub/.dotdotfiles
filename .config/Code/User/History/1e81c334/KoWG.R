


library(testthat)
library(rstan)
library(tidyverse)
library(broom)
library(tidybayes)
library(furrr)
options(mc.cores = 4)
source("code/sbc-functions.R")
rstan_options(auto_write = TRUE)

# library(cmdstanr)

# cmdstan_model("stan/hierarchical-rubin-selection-mvn.stan")
default_stan_model = stan_model("stan/hier-vec.stan")
# heckit_model = cmdstan_model("stan/hier-vec.stan")

default_data_args = list(N = 60, p = 2, q = 2, d = 10, cutoff_quantile = 0.5)
default_params_args = list(mu_beta = 0, sd_beta = 1)
default_modelled_data_args = list(selection_type = "quantile", rubin = TRUE, selection_d = 2)


first_draw = simulate_draw(
  seed = 2,
  stan_model = default_stan_model,
  gen_data_args = default_data_args,
  gen_params_args = default_params_args,
  gen_modelled_data_args = default_modelled_data_args,
  fit_model = TRUE,
  fit_heckit = FALSE
)
first_draw$params

# TODO write hierarchical tidying code
create_hierarchical_comp_df = function(bayes_fit, params) {

  gamma = params$gamma
  splits = split(1:ncol(gamma), f = ceiling(seq_along(1:ncol(gamma))/2))

  true_gamma_tidy = lapply(splits, function(x){gamma[, x]}) %>%
    map(flatten_params) %>%
    imap_dfr(~mutate(., c = as.numeric(.y))) %>%
    rename(true_value = prior_draw) %>%
    mutate(term = "gamma")

  true_beta_tidy = params$beta %>%
    matrix(., ncol = 1) %>%
    flatten_params() %>%
    rename(true_value = prior_draw) %>%
    mutate(term = "beta")

  single_sigma_tidy = params$Sigma %>%
    flatten_params()
  true_sigma_tidy = map_dfr(1:length(splits), ~mutate(single_sigma_tidy, c = .x)) %>%
    mutate(term = "Sigma") %>%
    rename(true_value = prior_draw)  


  tidy_beta_draws = bayes_fit %>%
    gather_draws(beta[k])
  tidy_gamma_draws = bayes_fit %>%
    gather_draws(gamma[c, j, k])
  tidy_sigma_draws = bayes_fit %>%
    gather_draws(Sigma[c, j, k])

  true_df = bind_rows(
    true_beta_tidy,
    true_gamma_tidy,
    true_sigma_tidy
  ) %>%
    mutate(c = replace_na(c, 1))
  tidy_draw_df = bind_rows(
    tidy_beta_draws,
    tidy_gamma_draws,
    tidy_sigma_draws
  ) %>%
    mutate(c = replace_na(c, 1))

  comp_df = inner_join(
      true_df,
      tidy_draw_df,
      by = c("term" = ".variable", 
            "j", "c", "k")
  ) %>%
    as_tibble()

  return(comp_df)
}

first_comp = create_hierarchical_comp_df(first_draw$bayes_fit, first_draw$params)
first_tidy = first_comp %>%
  # select(-true_value) %>%
  group_by(term, c, j, k) %>%
  summarise(
    estimate = median(.value), 
    conf.high = quantile(.value, 0.975),
    conf.low = quantile(.value, 0.025), 
    true_value = unique(true_value), 
    .groups = "drop"
  )
first_sbc = first_comp %>%
  group_by(term, c, j, k) %>%
  summarise(rank_stat = mean(.value < true_value), .groups = "drop")

test_that("Model works", {
  expect_s4_class(first_draw$bayes_fit, "stanfit")
  expect_s3_class(first_sbc, "tbl_df")
  expect_s3_class(first_tidy, "tbl_df")
})

first_tidy %>%
  mutate(error = true_value - estimate) %>%
  arrange(term, j) 




plan(multisession, workers = 8)
# plan(sequential)

simulated_draws = 1:2 %>%
  map(
    ~{sim_draw = simulate_draw(
      seed = .x,
      stan_model = default_stan_model,
      gen_data_args = default_data_args,
      gen_params_args = default_params_args,
      gen_modelled_data_args = default_modelled_data_args,
      fit_heckit = FALSE
      )
      
      
      if (is.null(sim_draw$bayes_fit)) {
        return(list(sbc = NULL, tidy_df = NULL, N_observed = NULL))
      } 
      comp_df = create_hierarchical_comp_df(sim_draw$bayes_fit, sim_draw$params)
      sbc_draw_bayes = comp_df %>%
        group_by(term, c, j, k) %>%
        summarise(rank_stat = mean(.value < true_value), .groups = "drop") %>%
        mutate(model = "bayes")


      sbc_draw = bind_rows(
        sbc_draw_bayes
      )
      N_observed = nobs(sim_draw$ols_fit)

       tidy_output = comp_df %>%
          group_by(term, c, j, k) %>%
          summarise(
            estimate = median(.value), 
            conf.high = quantile(.value, 0.975),
            conf.low = quantile(.value, 0.025), 
            true_value = unique(true_value), 
            .groups = "drop"
          ) %>%
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
  )  %>%
  tail()


sbc_draws %>% 
  filter(model == "bayes") %>%
    ggplot(aes(sample = rank_stat,
           colour = factor(k))) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_wrap(term~j,
             scales = "free") +
  theme_bw() +
  labs(
      x = "Theoretical Quantile", 
      y = "Realised Quantile", 
      title = "Simulation Based Calibration - Simple Selection Model"
  )



sbc_draws
