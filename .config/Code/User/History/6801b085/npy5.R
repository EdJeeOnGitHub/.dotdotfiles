script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  scratch.R [ --fake-data --start-date=<start-date> --end-date=<end-date> --time-aggregation-level=<time-aggregation-level>]
  Options:
  --start-date=<start-date>  Date to start panel [default: 2019-01-01]
  --end-date=<end-date>  Date to end panel [default: 2024-12-01]
  --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: month]
"),
  args = if (interactive()) "--fake-data " else commandArgs(trailingOnly = TRUE)
) 

library(data.table)
library(tidyverse)
library(lubridate)
library(glue)

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

town_vax_dt
town_vax_dt[, 
    event_time := interval(town_treat_date, date) %/% months(1)]

district_vax_dt[, 
    event_time := interval(district_treat_date, date) %/% months(1)]


plot_agg_vax_time = function(df, vax, aggregation, time_var, time_period){
    subset_df = df %>%
        filter(vaccine == vax) 
    aggregation_intercept = sym(paste0(ensym(aggregation), "_treat_date"))
    time_var_title = if (ensym(time_var) == "event_time") {
        "event time"
    } else {"calendar time"}
    p = subset_df %>%
            ggplot(aes(
                x = {{ time_var }}, 
                y = n_vaccinated, 
                colour = {{ aggregation }}
            )) +
            geom_line() +
            theme_bw() +
            labs(
                title = glue("N Vaxxed per {ensym(aggregation)} per {time_period}, {time_var_title}."),
                subtitle = glue("{vax}"), 
                x = time_var_title
            )
    if (ensym(time_var) == "date") {
        p = p + 
            geom_vline( 
                aes( 
                    xintercept = {{ aggregation_intercept }}
                ), 
                linetype = "longdash", 
                alpha = 0.2
            )
    } else {
        p = p +
            geom_vline( 
                xintercept = 0,
                linetype = "longdash", 
                alpha = 0.5
            )
    }
    return(p)
}



district_calendar_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        district_vax_dt,
        .x, 
        district, 
        time_var = date,
        time_period = script_options$time_aggregation_level
    )
)
town_calendar_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        town_vax_dt,
        .x, 
        town, 
        time_var = date, 
        time_period = script_options$time_aggregation_level
    )
)
district_calendar_plots[[1]]

iwalk(
    town_calendar_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/fake-town-calendar-", 
            vaccines[.y], "-",
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))
iwalk(
    district_calendar_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/fake-district-calendar-", 
            vaccines[.y], "-",
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))


district_event_time_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        district_vax_dt,
        .x, 
        district, 
        time_var = event_time, 
        time_period = script_options$time_aggregation_level
    )
)
town_event_time_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        town_vax_dt,
        .x, 
        town, 
        time_var = event_time, 
        time_period = script_options$time_aggregation_level
    )
)


iwalk(
    town_event_time_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/fake-town-event-time-", 
            vaccines[.y], "-", 
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))
iwalk(
    district_event_time_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/fake-district-event-time-", 
            vaccines[.y], "-",
            script_options$time_aggregation_level,
             "-plot.png"), 
        width = 8, 
        height = 6
    ))


# library(did)
# stop()


# prepare_data_for_did = function(df, first_treat_var){
#     prep_df = copy(df)
#     prep_df[, time_period := interval(min(vac_year_month), vac_year_month) %/% months(1)]
#     prep_df[, group := interval(min(vac_year_month), get(first_treat_var)) %/% months(1)]
#     prep_df[, unit_id := .GRP, by = town]

#     return(prep_df)
# }


# prep_town_vax_dt = prepare_data_for_did(
#     town_vax_dt, first_treat_var =  "town_treat_year_month"
# )

# prep_district_vax_dt = prepare_data_for_did(
#     district_vax_dt, first_treat_var =  "district_treat_year_month"
# )

# prep_district_vax_dt %>%
#     ggplot(aes(
#         time_period , 
#         y = n_vaccinated, 
#         colour = factor(group)
#     )) +
#     geom_line() +
#     facet_wrap(~vaccine) +
#     geom_vline(aes(xintercept = group), 
#                 linetype = "longdash", 
#                 alpha = 0.2)



# prep_town_vax_dt[, 
# .N, by = c("unit_id", "time_period")]



# simple_town_month_att_gts = map(
#     vaccines,
#     ~att_gt(
#         yname = "n_vaccinated", 
#         tname = "time_period",
#         idname = "unit_id",
#         clustervars = "district",
#         panel = TRUE,
#         gname = "group",
#         data = prep_town_vax_dt[vaccine == .x],
#         control_group = "notyettreated"
#     )
# )

# simple_district_month_att_gts = map(
#     vaccines,
#     ~att_gt(
#         yname = "n_vaccinated", 
#         tname = "time_period",
#         idname = "unit_id",
#         clustervars = "district",
#         panel = TRUE,
#         gname = "group",
#         data = prep_district_vax_dt[vaccine == .x],
#         control_group = "notyettreated"
#     )
# )


# tidy_town_month_es = simple_town_month_att_gts %>%
#     map(aggte, type = "dynamic") %>%
#     imap_dfr(~tidy(.x) %>% mutate(vaccine = vaccines[.y])) %>%
#     as_tibble()


# tidy_town_month_es %>%
#     filter(vaccine == "bcg") %>%
#     filter(!is.na(conf.low)) %>%
#     ggplot(aes( 
#         x = event.time, 
#         y = estimate,
#         ymin = conf.low, 
#         ymax = conf.high, 
#         colour = event.time >= 0
#     )) +
#     geom_pointrange() +
#     geom_hline( 
#         yintercept = 0, 
#         linetype = "longdash"
#     ) +
#     theme_bw() +
#     guides( 
#         colour = "none"
#     )

# agg.es <- aggte(example_attgt, type = "dynamic")



# example_attgt <- att_gt(yname = "n_vaccinated",
#                         tname = "time_period",
#                         idname = "unit_id",
#                         clustervars = "district",
#                         panel = TRUE,
#                         gname = "group",
#                         data = prep_town_vax_dt[vaccine == "bcg"], 
#                         control_group = "notyettreated"
#                         )

# agg.es <- aggte(example_attgt, type = "dynamic")
# summary(agg.es)

# ggdid(agg.es)
