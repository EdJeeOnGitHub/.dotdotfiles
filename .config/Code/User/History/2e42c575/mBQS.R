library(tidyverse)


all_peto_df =   read_csv(
    "publication_bias/Applications/water/or-labels.csv") %>%
    mutate(row = 1:n())

all_peto_df 


files = fs::dir_ls("publication_bias/FiguresandTables/sens-output/", regexp = "SelectionModel")


dirty_tables = map_dfr(files, read.table,  skip = 3, nrow = 2, sep = "&") 
tables = dirty_tables %>%
    rename(
        theta = V1, 
        tau = V2, 
        beta_p = V3
    ) %>%
    mutate(
        beta_p = str_remove_all(beta_p, fixed("\\")),
        type = rep(c("estimate", "std_error"), n()/2), 
        row_missing = rep(1:(n()/2), each = 2)
    )  %>%
    left_join(
        all_peto_df %>%
            rename(study_name = `Name (year)`) %>%
            select(study_name, row), 
        by = c("row_missing" = "row")
    )

long_sens_df =  tables %>%
        pivot_longer( 
            cols = c(theta, tau, beta_p)
        ) %>%
        mutate(value = str_remove_all(value, "\\(|\\)")%>% as.numeric())


long_sens_df %>%
    filter(type == "estimate") %>%
    ggplot(aes(
        x = value, 
        fill = name)) +
    geom_histogram(colour = "black") +
    facet_wrap(~name, scales = "free", ncol = 1) +
    guides(fill = "none") +
    theme_bw() +
    labs( 
        x = "Estimate", 
        title = "LOO Estimates of theta, tau, and beta", 
        caption = "N.B. different x scales."
    )
ggsave("publication_bias/FiguresandTables/sens-output/loo-hist.png", 
width = 8, height = 6)

long_sens_df %>%
    filter(value == 0)

library(data.table)
tables
tables = fread(files[[1]], sep = "&", skip = 3)
