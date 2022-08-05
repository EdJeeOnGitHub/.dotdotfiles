library(data.table)
library(tidyverse)
library(lubridate)
library(glue)

town_treatment_dt = fread("data/output/clean-town-randomisation-data.csv")
long_dt = fread("data/output/large-fake-long-vaccination-data.csv")




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




district_vac_df


plot_agg_vax_calendar = function(df, vax, aggregation){
    subset_df = df %>%
        filter(vaccine == vax) 
    aggregation_intercept = sym(paste0(ensym(aggregation), "_treat_year_month"))
    p = subset_df %>%
            ggplot(aes(
                x = vac_year_month, 
                y = n_vaccinated, 
                colour = {{ aggregation }}
            )) +
            geom_line() +
            geom_vline( 
                aes( 
                    xintercept = {{ aggregation_intercept }}
                ), 
                linetype = "longdash", 
                alpha = 0.2
            ) +
            theme_bw() +
            labs(
                title = glue("N Vaxxed per {ensym(aggregation)} per month"),
                subtitle = glue("{vax}")
            )
    return(p)
}



district_calendar_plots = map(
    vaccines, 
    ~plot_agg_vax_calendar(
        district_vac_df,
        .x, 
        district
    )
)
town_calendar_plots = map(
    vaccines, 
    ~plot_agg_vax_calendar(
        town_vac_df,
        .x, 
        town
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


district_vac_df %>%
    filter(vaccine == "bcg") %>%
    ggplot(aes( 
        x = vac_year_month, 
        y = n_vaccinated,
        colour = district
    )) + 
    geom_line() +
    geom_vline(
        aes(xintercept = district_treat_year_month), 
        linetype = "longdash", 
        alpha = 0.2) +
    facet_wrap(~vaccine) +
    theme_bw()


town_vac_df %>%
    filter(vaccine == "bcg") %>%
    ggplot(aes( 
        x = vac_year_month, 
        y = n_vaccinated,
        colour = town
    )) + 
    geom_line() +
    geom_vline(
        aes(xintercept = town_treat_year_month), 
        linetype = "longdash", 
        alpha = 0.2) +
    facet_wrap(~vaccine) +
    theme_bw()

