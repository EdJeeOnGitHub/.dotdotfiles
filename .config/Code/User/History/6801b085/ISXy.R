
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  scratch.R [ --fake-data --start-date=<start-date> --end-date=<end-date> ]
  Options:
  --start-date=<start-date>  Date to start panel [default: 2019-01-01]
  --end-date=<end-date>  Date to end panel [default: 2024-12-01]
"),

  args = if (interactive()) "--fake-data " else commandArgs(trailingOnly = TRUE)
) 
library(data.table)
library(tidyverse)
library(lubridate)
library(glue)

if (script_options$fake_data) {
    town_input_path = "data/output/fake-town-month-vaccination-data.csv"
    district_input_path = "data/output/fake-district-month-vaccination-data.csv"
} else {
    town_input_path = "data/output/town-month-vaccination-data.csv"
    district_input_path = "data/output/district-month-vaccination-data.csv"
}

town_vax_dt = fread(town_input_path)
district_vax_dt = fread(district_input_path)


if (script_options$fake_data) {
    town_vax_dt = town_vax_dt[vac_year_month > "2022-01-01" & vac_year_month < "2024-01-01"]
    district_vax_dt = district_vax_dt[vac_year_month > "2022-01-01" & vac_year_month < "2024-01-01"]
}

districts = district_vax_dt[, unique(district)]
towns = town_vax_dt[, unique(town)]
vaccines = town_vax_dt[, unique(vaccine)]

town_vax_dt
town_vax_dt[, 
    event_time := interval(town_treat_year_month, vac_year_month) %/% months(1)]

district_vax_dt[, 
    event_time := interval(district_treat_year_month, vac_year_month) %/% months(1)]


plot_agg_vax_time = function(df, vax, aggregation, time_var){
    subset_df = df %>%
        filter(vaccine == vax) 
    aggregation_intercept = sym(paste0(ensym(aggregation), "_treat_year_month"))
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
                title = glue("N Vaxxed per {ensym(aggregation)} per month, {time_var_title}."),
                subtitle = glue("{vax}"), 
                x = time_var_title
            )
    if (ensym(time_var) == "vac_year_month") {
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
        time_var = vac_year_month
    )
)
town_calendar_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        town_vax_dt,
        .x, 
        town, 
        time_var = vac_year_month
    )
)
district_calendar_plots[[1]]
stop(0)
iwalk(
    town_calendar_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0("data/output/plots/fake-town-calendar-", vaccines[.y], "-plot.png"), 
        width = 8, 
        height = 6
    ))
iwalk(
    district_calendar_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0("data/output/plots/fake-district-calendar-", vaccines[.y], "-plot.png"), 
        width = 8, 
        height = 6
    ))


district_event_time_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        district_vax_df,
        .x, 
        district, 
        time_var = event_time
    )
)
town_event_time_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        town_vac_df,
        .x, 
        town, 
        time_var = event_time
    )
)


iwalk(
    town_event_time_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0("data/output/plots/fake-town-event-time-", vaccines[.y], "-plot.png"), 
        width = 8, 
        height = 6
    ))
iwalk(
    district_event_time_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0("data/output/plots/fake-district-event-time-", vaccines[.y], "-plot.png"), 
        width = 8, 
        height = 6
    ))


library(did)
stop()


indiv_vac_df[, time_period := interva]

town_vac_df[, time_period := interval(min(vac_year_month), vac_year_month) %/% months(1)]
town_vac_df[, group := interval(min(vac_year_month), town_treat_year_month) %/% months(1)]
town_vac_df[, town_id := .GRP, by = town]
town_vac_df

example_attgt <- att_gt(yname = "n_vaccinated",
                        tname = "time_period",
                        idname = "town_id",
                        clustervars = "district",
                        panel = FALSE,
                        gname = "group",
                        data = town_vac_df, 
                        control_group = "notyettreated"
                        )
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es)

ggdid(agg.es)
