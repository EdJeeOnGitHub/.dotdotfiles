
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

long_treat_dt = merge( 
    long_dt, 
    town_treatment_dt, 
    all.x = TRUE,
    by = c("district", "town")
)



long_treat_dt[, 
    `:=`(
        vac_year_month = floor_date(vac_date, "month"), 
        town_treat_year_month = floor_date(town_roll_in_date_randomised, "month"), 
        district_treat_year_month = floor_date(district_roll_in_date, "month")
    )]

if (max(long_treat_dt$vac_date) > script_options$end_date) {
    stop("There is a date in the data after our panel end date.")
}
if (min(long_treat_dt$vac_date) < script_options$start_date) {
    stop("There is a date in the data before our panel start date.")
}

dates = seq(from = ymd(script_options$start_date), to = ymd(script_options$end_date), by = "months")
town_treatment_dt
empty_panel_df = CJ(
    vac_year_month = dates,
    vaccine = vaccines,
    town_districts = paste0(town_treatment_dt$district, "_", town_treatment_dt$town)     
)
empty_panel_df[, c("district", "town") := tstrsplit(town_districts, "_", fixed=TRUE)]
empty_panel_df[, town_districts := NULL]
empty_panel_df = merge(
    empty_panel_df,
    town_treatment_dt, 
    all.x = TRUE,
    by = c("district", "town")
)

empty_panel_df[, 
    `:=`(town_treat_year_month = floor_date(town_roll_in_date_randomised, "months"), 
      district_treat_year_month = floor_date(district_roll_in_date, "months"))
]
town_vac_df = long_treat_dt[, 
    .(n_vaccinated = sum(vaccinated),
      n_timely = sum(vaccinated_on_time, na.rm = TRUE)), 
    by = c("vaccine", "district", "town", "vac_year_month")] 

if (any(is.na(town_vac_df$n_vaccinated)) | any(is.na(town_vac_df$n_timely))) {
    stop("NAs in town vaccination numbers.")
}


full_town_vac_df = merge(
    empty_panel_df,
    town_vac_df,
    all.x = TRUE, 
    by.x = c("vac_year_month",  "vaccine", "district", "town"), 
    by.y = c("vac_year_month", "vaccine", "district", "town")
)

stop()

full_town_vac_df[is.na(n_vaccinated), n_vaccinated := 0 ]
full_town_vac_df[is.na(n_timely), n_timely := 0 ]


district_vac_df = long_treat_dt[, 
    .(n_vaccinated = sum(vaccinated), 
      n_timely = sum(vaccinated_on_time, na.rm = TRUE)), 
    by = c("vaccine", "district", "vac_year_month")] 

if (any(is.na(district_vac_df$n_vaccinated)) | any(is.na(district_vac_df$n_timely))) {
    stop("NAs in district vaccination numbers.")
}

full_district_vac_df = merge(
    empty_panel_df,
    district_vac_df,
    all.x = TRUE, 
    by.x = c("vac_year_month",  "vaccine", "district"), 
    by.y = c("vac_year_month", "vaccine", "district")
)


full_district_vac_df[is.na(n_vaccinated), n_vaccinated := 0 ]
full_district_vac_df[is.na(n_timely), n_timely := 0 ]


write_csv(
    full_town_vac_df,
    town_output_path
)

write_csv(
    full_district_vac_df,
    district_output_path
)
