library(tidyverse)

rankstat_df = read_csv("data/sbc_output_data/wtp_sbc_wtp_model.csv")
rankstat_df = read_csv("data/sbc_output_data/beliefs_beliefs_sbc.csv")

    select(variable) %>%
    unique()
clean_rankstat_df = rankstat_df %>%
    mutate( 
        j = str_extract(variable, "(?<=\\[)\\d+") %>% as.numeric(),
        k = str_extract(variable, "\\d+(?=\\])") %>% as.numeric(), 
        variable = str_remove(variable, "(?<=ranks_).*")
    )


clean_rankstat_df %>%
    select(j) %>%
    pull() %>%
    max()



clean_rankstat_df %>%
    filter( j < 3) %>%
    ggplot(aes(
        sample = mean,
        colour = factor(k))) +
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
      title = "Simulation Based Calibration - WTP Valuations", 
      subtitle = "WTP Strata Means"
  )
ggsave("quantile-sbc.png", width = 8, height = 6)