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
