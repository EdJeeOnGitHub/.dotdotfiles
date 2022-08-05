
library(data.table)
library(tidyverse)
library(lubridate)
library(glue)



town_vax_dt = fread("data/output/clean-town-randomisation-data.csv")
district_vax_dt = fread("data/output/large-fake-long-vaccination-data.csv")




missing_towns = setdiff(town_treatment_dt[, unique(town)], long_dt[, unique(town)])
if (length(missing_towns) > 0) {
    stop("Some towns present in treatment data missing in ZM data.")
}

districts = town_treatment_dt[, unique(district)]
towns = town_treatment_dt[, unique(town)]
vaccines = long_dt[, unique(vaccine)]
town_treatment_dt %>%
    colnames()
long_dt %>%
    colnames()

long_treat_dt = merge( 
    long_dt, 
    town_treatment_dt, 
    all.x = TRUE,
    by = c("district", "town")
)

long_treat_dt[, 
    `:=`(vac_year_month = floor_date(vac_date, "month"), 
    town_treat_year_month = floor_date(town_roll_in_date_randomised, "month"), 
    district_treat_year_month = floor_date(district_roll_in_date, "month")
    )]

town_vac_df = long_treat_dt[, 
    .(n_vaccinated = sum(vaccinated), town_treat_year_month = unique(town_treat_year_month), district_treat_year_month = unique(district_treat_year_month)), 
    by = c("vaccine", "district", "town", "vac_year_month")] 

district_vac_df = long_treat_dt[, 
    .(n_vaccinated = sum(vaccinated), town_treat_year_month = unique(town_treat_year_month), district_treat_year_month = unique(district_treat_year_month)), 
    by = c("vaccine", "district", "vac_year_month")] 


town_vac_df[, 
    event_time := interval(town_treat_year_month, vac_year_month) %/% months(1)]

district_vac_df[, 
    event_time := interval(district_treat_year_month, vac_year_month) %/% months(1)]
district_vac_df


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
        district_vac_df,
        .x, 
        district, 
        time_var = vac_year_month
    )
)
town_calendar_plots = map(
    vaccines, 
    ~plot_agg_vax_time(
        town_vac_df,
        .x, 
        town, 
        time_var = vac_year_month
    )
)

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
        district_vac_df,
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
