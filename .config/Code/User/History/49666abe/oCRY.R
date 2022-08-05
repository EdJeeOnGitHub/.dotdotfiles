library(tidyverse)
library(glue)

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

vars = clean_rankstat_df %>%
    select(variable) %>%
    unique() %>%
    pull()


plots = map(
    vars,
    ~clean_rankstat_df %>%
        filter( j < 3) %>%
        filter(variable == .x) %>%
        ggplot(aes(
            sample = mean, 
            colour = factor(k))) +
    stat_qq(distribution = stats::qunif,
            alpha = 1) +
    stat_qq_line(distribution = stats::qunif,
                colour = "black",linetype = "longdash") +
    facet_grid(k~j,
                scales = "free") +
    theme_bw() +
    labs(
        x = "Theoretical Quantile", 
        y = "Realised Quantile",
        title = "Simulation Based Calibration - Beliefs Model", 
        subtitle = glue("{.x}")
    )
)
iwalk(
    plots,
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/sbc_output_data/plots/sbc_", 
            vars[.y], "plot.png"
        ), 
        width = 8, 
        height = 8
    )
)

