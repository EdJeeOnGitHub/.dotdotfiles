


stop()


sbc_model = cmdstan_model("stan_models/sbc_takeup_reduced.stan")

  library(furrr)
  anon_f = function(seed){
    sim_draw = simulate_draw(
        seed = seed,
        sim_args = list(fit_model = TRUE),
        gen_data_args = c(default_gen_data_args, list(sbc = 1)),
        gen_params_args = NULL,
        gen_modelled_data_args = NULL,
        sample_from_model_args = default_sample_from_model_args %>% list_modify(stan_model = sbc_model)
    )
    sim_draw = sbc_model$sample(
        data = c(models$REDUCED_FORM_NO_RESTRICT,
                 stan_data)
    )
    rank_draws = sim_draw$summary(variables = "ranks_", "mean")
    rank_draws$seed = seed
    return(rank_draws)
  }
  poss_anon_f = possibly(anon_f, otherwise = tibble(fail = TRUE))
  plan(multisession, workers = 2)
  sim_draws = future_map_dfr(
    1:10,
    anon_f,
    .options = furrr_options(seed = TRUE)
  )
  p = sim_draws %>%
    ggplot(aes( 
      sample = mean,
      colour = variable
    )) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_wrap(~variable,
             scales = "free") +
  theme_bw() +
  guides(colour = "none")  +
  labs(
      x = "Theoretical Quantile", 
      y = "Realised Quantile",
      title = "Simulation Based Calibration - Reduced Form No Age"
  )
  p
  ggsave("temp.png", width = 8, height = 6)