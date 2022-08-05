library(tidyverse)


rankstat_df = read_csv("data/sbc_output_data/reduced_form_sbc.csv")



rankstat_df %>%
    ggplot(aes(
        sample = rank_stat,
        colour = .variable)) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_wrap(.variable~j,
             scales = "free") +
  theme_bw() +
  guides(colour = "none")  +
  labs(
      x = "Theoretical Quantile", 
      y = "Realised Quantile",
      title = "Simulation Based Calibration - Reduced Form No Age"
  )
ggsave("quantile-sbc.png", width = 8, height = 6)