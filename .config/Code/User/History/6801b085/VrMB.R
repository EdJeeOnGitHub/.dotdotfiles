library(data.table)
library(tidyverse)

town_treatment_dt = fread("data/output/clean-town-randomisation-data.csv")
long_dt = fread("data/output/fake-long-vaccination-data.csv")


long_dt[, unique(town)]
long_dt[town == "North karachi"] %>%
    View()

missing_towns = setdiff(town_treatment_dt[, unique(town)], long_dt[, unique(town)])

if (length(missing_towns) > 0) {
    stop("Some towns present in treatment data missing in ZM data.")
}


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


long_treat_dt

long_dt %>%
    colnames()
