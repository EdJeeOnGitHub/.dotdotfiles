
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  create-raw-data.R [options] [--fake-data]

  Options:
    --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: month]
    --start-date=<start-date>  First date to plot vaccination time series from. [default: 2019/01/01]
    --end-date=<end-date>  Last date to plot vaccination time series till. [default: 2025/01/01]
"),
  args = if (interactive()) "--fake-data --time-aggregation-level=month --start-date=2019/01/01 --end-date=2020/02/01  " else commandArgs(trailingOnly = TRUE)
) 

library(data.table)
library(tidyverse)
library(lubridate)
library(glue)

script_options = script_options %>%
    modify_at( 
        vars(contains("date")), ymd
    )

if (script_options$fake_data) {
    town_input_path = paste0(
        "data/output/fake-town-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    district_input_path = paste0(
        "data/output/fake-district-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    fake_output_name = "fake-"
} else {
    town_input_path = paste0(
        "data/output/town-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    district_input_path = paste0(
        "data/output/district-", 
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
    fake_output_name = ""
}

town_vax_dt = fread(town_input_path)
district_vax_dt = fread(district_input_path)
town_vax_dt[, town := factor(town)]
town_vax_dt[, town := fct_reorder(town, town_treat_date)]
district_vax_dt[, district := factor(district)]
district_vax_dt[, district := fct_reorder(district, district_treat_date)]

town_vax_dt = town_vax_dt[date > script_options$start_date & date < script_options$end_date]
district_vax_dt = district_vax_dt[date > script_options$start_date & date < script_options$end_date]

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
            "data/output/plots/raw-data-plots/", fake_output_name, "district-histogram-", 
            vaccines[.y], "-",
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))


p_town_hist = town_vax_dt %>%
    ggplot(aes( 
        x = n_vaccinated, 
        fill = town
    )) +
    geom_histogram() +
    facet_wrap(~vaccine) +
    guides(fill = "none") +
    theme_bw()  +
    labs( 
        title = glue("Histogram of N Vaccinated per town per {script_options$time_aggregation_level}")
    )

ggsave(
    plot = p_town_hist, 
    filename = paste0(
        "data/output/plots/raw-data-plots/", fake_output_name, "town-histogram-", 
        script_options$time_aggregation_level, ".png"
    ), 
    width = 8, height = 6
)


plot_district_town_breakdown = function(data, vax, time_var){

    p = data %>%
        mutate(town = as.character(town)) %>%
        filter(vaccine == vax) %>%
        ggplot(aes( 
            x = {{ time_var }}, 
            y = n_vaccinated,
            colour = town
        ))    +
        facet_wrap(~district) +
        geom_line(alpha = 0.7) +
        guides( 
            colour = "none"
        ) +
        theme_bw() +
        labs(
            title = glue("N {vax} vaccinations per town, by {script_options$time_aggregation_level}.")
        )

    if (ensym(time_var) == "date") {
        p = p + 
            geom_vline( 
                aes( 
                    xintercept = district_treat_date 
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


district_vax_plot = district_vax_dt %>%
    ggplot(aes(
        x = date, 
        y = n_vaccinated, 
        colour = vaccine
    )) +
    geom_line() +
    facet_wrap(~district) +
    theme_bw() +
    geom_vline( 
        aes( 
            xintercept = district_treat_date
        ), 
        linetype = "longdash"
    ) +
    labs( 
        title = glue("N vaccinated per {script_options$time_aggregation_level} by district")
    )

ggsave(
    plot = district_vax_plot, 
    filename = paste0(
        "data/output/plots/raw-data-plots/", 
        fake_output_name,
        "district-vaccine-calendar-", 
        script_options$time_aggregation_level, 
        ".png"), 
    width = 8, 
    height = 6
)


district_town_plots = map( 
    vaccines, 
    ~plot_district_town_breakdown(
        town_vax_dt, 
        vax = .x, 
        time_var = date
    )
)

iwalk(
    district_town_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/raw-data-plots/", fake_output_name, "district-town-", 
            vaccines[.y], "-calendar-", 
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))


district_town_event_plots = map( 
    vaccines, 
    ~plot_district_town_breakdown(
        town_vax_dt, 
        vax = .x, 
        time_var = event_time
    )
)

iwalk(
    district_town_event_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/raw-data-plots/", fake_output_name, "district-town-", 
            vaccines[.y], "-event-time-", 
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))

plot_district_town_breakdown(
    town_vax_dt, 
    vax = "bcg", 
    time_var = event_time
)


p_district_indiv_vaccine_plots = map(
    vaccines,
    ~district_vax_dt %>%
        filter(vaccine == .x) %>%
    ggplot(aes( 
        x = date, 
        y = n_vaccinated
    ))   +
    geom_line() +
    facet_wrap(~district) +
    geom_vline(
        aes(xintercept = district_treat_date), 
        linetype = "longdash") +
    theme_bw()  +
    labs( 
        title = glue("Number of {.x} vaccinations per {script_options$time_aggregation_level}")
    )
)

iwalk(
    p_district_indiv_vaccine_plots, 
    ~ggsave( 
        plot = .x, 
        filename = paste0(
            "data/output/plots/raw-data-plots/", fake_output_name, "district-calendar-facet-", 
            vaccines[.y], "-",
            script_options$time_aggregation_level,
            "-plot.png"), 
        width = 8, 
        height = 6
    ))


p_district_vaccine_plot = district_vax_dt %>%
    ggplot(aes( 
        x = date, 
        y = n_vaccinated, 
        colour = vaccine
    ))   +
    geom_line() +
    facet_wrap(~district) +
    geom_vline(
        aes(xintercept = district_treat_date), 
        linetype = "longdash") +
    theme_bw()  +
    labs( 
        title = glue("Number of vaccinations per {script_options$time_aggregation_level}")
    )

ggsave( 
    p_district_vaccine_plot, 
    filename = paste0(
        "data/output/plots/raw-data-plots/", 
        fake_output_name, 
        "district-vaccine-", 
        script_options$time_aggregation_level, 
        "-plot.png"), 
    width = 8, 
    height = 6
)    

# Only run district time plots at month level of time aggregation
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
                "data/output/plots/raw-data-plots/", fake_output_name, "district-calendar-", 
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
                "data/output/plots/raw-data-plots/", fake_output_name, "district-event-time-", 
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
            "data/output/plots/raw-data-plots/", fake_output_name, "town-calendar-", 
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
            "data/output/plots/raw-data-plots/", fake_output_name, "town-event-time-", 
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
