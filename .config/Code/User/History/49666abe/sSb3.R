library(tidyverse)


rankstat_df = read_csv("data/sbc_output_data/wtp_sbc_test.csv")


rankstat_df %>%
    ggplot(aes(
        sample = mean,
        colour = variable)) +
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
ggsave("quantile-sbc.png", width = 8, height = 6)