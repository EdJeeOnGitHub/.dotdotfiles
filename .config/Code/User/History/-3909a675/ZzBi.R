
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  create-raw-data.R [ --fake-data --time-aggregation-level=<time-aggregation-level>]
  Options:
  --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: month]
"),
  args = if (interactive()) "--fake-data --time-aggregation-level=day  " else commandArgs(trailingOnly = TRUE)
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
    fake_output_name = "fake-"
} else {
    town_input_path = paste0(
        "data/output/town-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    district_input_path = "data/output/district-month-vaccination-data.csv"
    fake_output_name = ""
}

town_vax_dt = fread(town_input_path)
district_vax_dt = fread(district_input_path)

if (script_options$fake_data) {
    town_vax_dt = town_vax_dt[date > "2021-06-01" & date < "2024-01-01"]
    district_vax_dt = district_vax_dt[date > "2021-06-01" & date < "2024-01-01"]
}

districts = district_vax_dt[, unique(district)]
towns = town_vax_dt[, unique(town)]
vaccines = town_vax_dt[, unique(vaccine)]

if (script_options$time_aggregation_level == "day") {

    town_vax_dt[, 
        event_time := interval(town_treat_date, date) %/% days(1)]

} else if (script_options$time_aggregation_level == "week") {

    town_vax_dt[, 
        event_time := interval(town_treat_date, date) %/% weeks(1)]

} else {

    town_vax_dt[, 
        event_time := interval(town_treat_date, date) %/% months(1)]
    district_vax_dt[, 
        event_time := interval(district_treat_date, date) %/% months(1)]

}



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

stop()

town_vax_dt %>%
    ggplot(aes( 
        x = n_vaccinated, 
        fill = town
    )) +
    geom_histogram()


shadow_hist_plot = function(plot_data, x_var, subset_var, bins = 30) {
    subset_df = plot_data %>%
        select(-{{ subset_var }})
    
        plot_data %>% 
            ggplot(aes(x = {{ x_var }})) +
            geom_histogram(data = subset_df, fill = "grey", bins = bins) +
            geom_histogram(aes(fill = {{ subset_var }}), color = "black", bins = bins) +
            facet_wrap(vars({{ subset_var }})) +
            theme_bw() +
            guides(fill = "none") 
}

district_shadow_hists = map(
    vaccines, 
    ~shadow_hist_plot(
        plot_data = district_vax_dt %>%
            filter(vaccine == .x),
        x_var = n_vaccinated, 
        subset_var = district, 
        bins = 20
    ) + 
    labs( 
        title = glue("Histogram of {.x} vaccines administered per {script_options$time_aggregation_level}")
    )
)

iwalk(
    district_shadow_hists, 
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


# Only run district plots at month level of time aggregation
if (script_options$time_aggregation_level == "month") {
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

    iwalk(
        district_calendar_plots, 
        ~ggsave( 
            plot = .x, 
            filename = paste0(
                "data/output/plots/", fake_output_name, "district-calendar-", 
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
                "data/output/plots/" fake_output_name, "district-event-time-", 
                vaccines[.y], "-",
                script_options$time_aggregation_level,
                "-plot.png"), 
            width = 8, 
            height = 6
        ))

}

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



iwalk(
    town_calendar_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/", fake_output_name, "town-calendar-", 
            vaccines[.y], "-",
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))
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
            "data/output/plots/", fake_output_name, "town-event-time-", 
            vaccines[.y], "-", 
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))

generic_plot_path = paste0(
    "data/output/plots/town-event-time-vaccine-", 
    script_options$time_aggregation_level, "-plot.png"
)

cat(glue("\n Plotting event and calendar time finished. 
        Outputs saved to {generic_plot_path}"))
