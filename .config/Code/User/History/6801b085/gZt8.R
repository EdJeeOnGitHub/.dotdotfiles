script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  scratch.R [ --fake-data --time-aggregation-level=<time-aggregation-level>]
  Options:
  --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: month]
"),
  args = if (interactive()) "--fake-data --time-aggregation-level=month  " else commandArgs(trailingOnly = TRUE)
) 

library(data.table)
library(tidyverse)
library(lubridate)
library(glue)
library(did)

if (script_options$fake_data) {
    town_input_path = paste0(
        "data/output/fake-town-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    district_input_path = "data/output/fake-district-month-vaccination-data.csv"
} else {
    town_input_path = paste0(
        "data/output/town-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    district_input_path = "data/output/district-month-vaccination-data.csv"
}

town_vax_dt = fread(town_input_path)
district_vax_dt = fread(district_input_path)
town_vax_dt

if (script_options$fake_data) {
    town_vax_dt = town_vax_dt[date > "2021-06-01" & date < "2024-01-01"]
    district_vax_dt = district_vax_dt[date > "2021-06-01" & date < "2024-01-01"]
}

districts = district_vax_dt[, unique(district)]
towns = town_vax_dt[, unique(town)]
vaccines = town_vax_dt[, unique(vaccine)]





prepare_data_for_did = function(df, first_treat_var, time_period, unit_var ){
    prep_df = copy(df)
    if (time_period == "day") {

        prep_df[, 
            event_time := interval(min(date), date) %/% days(1)]
    prep_df[, group := interval(min(date), get(first_treat_var)) %/% days(1)]

    } else if (time_period == "week") {

        prep_df[, 
            event_time := interval(min(date), date) %/% weeks(1)]
    prep_df[, group := interval(min(date), get(first_treat_var)) %/% weeks(1)]

    } else {

        prep_df[, 
            event_time := interval(min(date), date) %/% months(1)]
    prep_df[, group := interval(min(date), get(first_treat_var)) %/% months(1)]

    }

    prep_df[, unit_id := .GRP, by = unit_var]

    return(prep_df)
}


prep_town_vax_dt = prepare_data_for_did(
    town_vax_dt, 
    first_treat_var = "town_treat_date", 
    time_period = script_options$time_aggregation_level,
    unit_var = "town"
)
prep_town_vax_dt
prep_district_vax_dt = prepare_data_for_did(
    district_vax_dt, first_treat_var =  "district_treat_year_month"
)

prep_district_vax_dt %>%
    ggplot(aes(
        time_period , 
        y = n_vaccinated, 
        colour = factor(group)
    )) +
    geom_line() +
    facet_wrap(~vaccine) +
    geom_vline(aes(xintercept = group), 
                linetype = "longdash", 
                alpha = 0.2)



prep_town_vax_dt[, 
.N, by = c("unit_id", "time_period")]



simple_town_month_att_gts = map(
    vaccines,
    ~att_gt(
        yname = "n_vaccinated", 
        tname = "time_period",
        idname = "unit_id",
        clustervars = "district",
        panel = TRUE,
        gname = "group",
        data = prep_town_vax_dt[vaccine == .x],
        control_group = "notyettreated"
    )
)

simple_district_month_att_gts = map(
    vaccines,
    ~att_gt(
        yname = "n_vaccinated", 
        tname = "time_period",
        idname = "unit_id",
        clustervars = "district",
        panel = TRUE,
        gname = "group",
        data = prep_district_vax_dt[vaccine == .x],
        control_group = "notyettreated"
    )
)


tidy_town_month_es = simple_town_month_att_gts %>%
    map(aggte, type = "dynamic") %>%
    imap_dfr(~tidy(.x) %>% mutate(vaccine = vaccines[.y])) %>%
    as_tibble()


tidy_town_month_es %>%
    filter(vaccine == "bcg") %>%
    filter(!is.na(conf.low)) %>%
    ggplot(aes( 
        x = event.time, 
        y = estimate,
        ymin = conf.low, 
        ymax = conf.high, 
        colour = event.time >= 0
    )) +
    geom_pointrange() +
    geom_hline( 
        yintercept = 0, 
        linetype = "longdash"
    ) +
    theme_bw() +
    guides( 
        colour = "none"
    )

agg.es <- aggte(example_attgt, type = "dynamic")



example_attgt <- att_gt(yname = "n_vaccinated",
                        tname = "time_period",
                        idname = "unit_id",
                        clustervars = "district",
                        panel = TRUE,
                        gname = "group",
                        data = prep_town_vax_dt[vaccine == "bcg"], 
                        control_group = "notyettreated"
                        )

agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es)

ggdid(agg.es)
