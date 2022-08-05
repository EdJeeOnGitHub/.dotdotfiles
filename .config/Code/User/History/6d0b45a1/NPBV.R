library(tidyverse)
library(lubridate)
library(did)
library(fixest)


cohort_vax_df = read_csv("data/output/clean-preprogramme-metrics.csv")

agg_vax_df = cohort_vax_df %>%
    group_by(vaccine, district, vax_year_mon) %>%
    summarise( 
        n_vaxxed = sum(n_vaxxed), 
        .groups = "drop"
    )


did_vax_df = agg_vax_df %>%
    mutate( 
        t_period = interval(min(vax_year_mon), vax_year_mon) %/% months(1),
    ) %>%
    group_by(district) %>%
    mutate(
        G = sample(t_period, 1)
    ) %>%
    ungroup() %>%
    mutate(district_id = as.numeric(factor(district)))


fit = did_vax_df %>%
    filter(vaccine == "BCG") %>%
    feols( 
        data = ., 
        fml = n_vaxxed ~ 1 | district + vax_year_mon
    )

fixef(fit)

bcg_fit = did_vax_df %>%
    filter(vaccine == "BCG") %>%
    att_gt( 
        data = .,
        yname = "n_vaxxed",
        tname = "t_period", 
        idname = "district_id", 
        gname = "G", 
        control_group = "notyettreated"
    )

# aggregate them into event study plot
did_es <- aggte(bcg_fit, type="dynamic")
did_es %>%
    tidy()
# plot the event study
ggdid(did_es)
did_vax_df

ggdid(bcg_fit)


months(ymd("2020-01-01") - ymd("2020-02-01"))

