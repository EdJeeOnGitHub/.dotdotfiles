library(tidyverse)
library(cmdstanr)







load("data/stan_analysis_data/dist_prior53.RData")
ls()

files = fs::dir_ls("data/stan_analysis_data", regexp = "dist_prior.*csv$")
files
dist_fit = as_cmdstan_fit(
  files
)
draws = dist_fit$draws()
draw_df = posterior::as_draws_df(draws)

var_names = tibble(draw_df %>%
  colnames())

colnames(var_names) = "var_names"




str(dist_fit)
dist_fit$print(max_rows = 50)
kdist_summ = dist_fit$summary()

kdist_summ 



library(tidyverse)
load("data/sbc_output_data/sbc_test.RData")
rank_stats$REDUCED_FORM_NO_RESTRICT %>%
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

    
sbc_model = cmdstan_model("stan_models/sbc_takeup_reduced.stan")

  library(furrr)
  anon_f = function(seed){
    browser()
    sim_fit = sbc_model$sample(
        data = c(models$REDUCED_FORM_NO_RESTRICT,
                 stan_data,
                 list(
                    sbc = TRUE,
                    num_age_groups = 1,
                    obs_age_group = rep(1, stan_data$num_obs),
                    use_age_group_gp = FALSE,
                    age_group_alpha_sd = array(0, dim = 8),
                    age_group_rho_sd = array(0, dim = 8)
                 )),
        chains = 1
    )
    rank_draws = sim_fit$summary(variables = "ranks_", "mean")
    rank_draws$seed = seed
    return(rank_draws)
  }
  ed = anon_f(1)
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