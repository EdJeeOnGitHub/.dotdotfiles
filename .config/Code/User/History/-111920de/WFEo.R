
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  merge-zm-town-data.R [ --fake-data --start-date=<start-date> --end-date=<end-date> ]
  Options:
  --start-date=<start-date>  Date to start panel [default: 2019-01-01]
  --end-date=<end-date>  Date to end panel [default: 2024-12-01]
"),

  args = if (interactive()) "--fake-data " else commandArgs(trailingOnly = TRUE)
) 
library(data.table)
library(tidyverse)
library(lubridate)

script_options = script_options %>%
    modify_at(vars(contains("date")), ymd)

if (script_options$fake_data) {
    input_path = "data/output/large-fake-long-vaccination-data.csv"
    town_output_path = "data/output/fake-town-month-vaccination-data.csv"
    district_output_path = "data/output/fake-district-month-vaccination-data.csv"
} else {
    input_path = "data/output/long-vaccination-data.csv"
    town_output_path = "data/output/town-month-vaccination-data.csv"
    district_output_path = "data/output/district-month-vaccination-data.csv"
}


town_treatment_dt = fread("data/output/clean-town-randomisation-data.csv")
long_dt = fread(input_path)





missing_towns = setdiff(town_treatment_dt[, unique(town)], long_dt[, unique(town)])
if (length(missing_towns) > 0) {
    stop("Some towns present in treatment data missing in ZM data.")
}

districts = town_treatment_dt[, unique(district)]
towns = town_treatment_dt[, unique(town)]
vaccines = long_dt[, unique(vaccine)]


if (max(long_dt$vac_date) > script_options$end_date) {
    stop("There is a date in the data after our panel end date.")
}
if (min(long_dt$vac_date) < script_options$start_date) {
    stop("There is a date in the data before our panel start date.")
}

create_aggregrate_data = function(vax_data,
                                  treatment_data,
                                  unit_aggregation_level, 
                                  time_aggregation_level) {
    dates = seq(
        from = ymd(script_options$start_date), 
        to = ymd(script_options$end_date), 
        by = time_aggregation_level
    )

    empty_panel_df = CJ(
        date = dates,
        vaccine = vaccines,
        town_districts = paste0(town_treatment_dt$district, "_", town_treatment_dt$town)     
    )

    empty_panel_df[, c("district", "town") := tstrsplit(town_districts, "_", fixed=TRUE)]
    empty_panel_df[, town_districts := NULL]

    empty_panel_df = merge(
        empty_panel_df,
        treatment_data, 
        all.x = TRUE,
        by = c("district", "town")
    )


    empty_panel_df[, 
        `:=`(town_treat_date = floor_date(town_roll_in_date_randomised, time_aggregation_level), 
        district_treat_date = floor_date(district_roll_in_date, time_aggregation_level))
    ]
    vax_data[, date := floor_date(vac_date, time_aggregation_level)]
    collapsed_dt = vax_data[, 
        .(n_vaccinated = sum(vaccinated),
          n_timely = sum(vaccinated_on_time, na.rm = TRUE)), 
        by = c("vaccine", unit_aggregation_level, "date")] 

    if (any(is.na(collapsed_dt$n_vaccinated)) | any(is.na(collapsed_dt$n_timely))) {
        stop("NAs in collapsed vaccination numbers.")
    }

    agg_dt = merge(
        empty_panel_df,
        collapsed_dt,
        all.x = TRUE, 
        by.x = c("date",  "vaccine", unit_aggregation_level), 
        by.y = c("date", "vaccine", unit_aggregation_level)
    )

    agg_dt[is.na(n_vaccinated), n_vaccinated := 0 ]
    agg_dt[is.na(n_timely), n_timely := 0 ]
    return(agg_dt)
}


### Town Level Aggregations ###
town_month_panel_dt = create_aggregrate_data(
    long_dt,
    town_treatment_dt,
    unit_aggregation_level = c("district", "town"), 
    time_aggregation_level = "months"
)

town_week_panel_dt = create_aggregrate_data(
    long_dt,
    town_treatment_dt,
    unit_aggregation_level = c("district", "town"), 
    time_aggregation_level = "weeks"
)

town_day_panel_dt = create_aggregrate_data(
    long_dt,
    town_treatment_dt,
    unit_aggregation_level = c("district", "town"), 
    time_aggregation_level = "days"
)

### District Level Aggregations ###
district_month_panel_dt = create_aggregrate_data(
    long_dt, 
    town_treatment_dt,
    unit_aggregation_level = "district", 
    time_aggregation_level = "months"
)
stop()
## Indiv data we do manually
dates = seq(
    from = ymd(script_options$start_date), 
    to = ymd(script_options$end_date), 
    by = "days"
)

empty_panel_df = CJ(
    date = dates,
    vaccine = vaccines,
    childid = unique(long_dt$childid)
)

indiv_panel_dt = merge(
    empty_panel_df, 
    long_dt, 
    all.x = TRUE,
    by.x = c("date", "vaccine", "childid"),
    by.y = c("vac_date", "vaccine", "childid")
)

indiv_day_dt = create_aggregrate_data()



town_month_dt
town_week_dt
town_days_dt





write_csv(
    full_town_vac_df,
    town_output_path
)

write_csv(
    full_district_vac_df,
    district_output_path
)
