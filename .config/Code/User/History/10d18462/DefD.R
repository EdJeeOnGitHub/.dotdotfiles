


stop()


sbc_model = cmdstan_model("stan_models/sbc_takeup_reduced.stan")

  library(furrr)
  anon_f = function(seed){
    sim_fit = sbc_model$sample(
        data = c(models$REDUCED_FORM_NO_RESTRICT,
                 stan_data)
    )
    rank_draws = sim_fit$summary(variables = "ranks_", "mean")
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