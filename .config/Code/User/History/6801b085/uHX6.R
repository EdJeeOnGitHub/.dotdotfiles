library(data.table)
library(tidyverse)

town_treatment_dt = fread("data/output/clean-town-randomisation-data.csv")
long_dt = fread("data/output/fake-long-vaccination-data.csv")


long_dt[, unique(town)]

town_treatment_dt

long_treat_dt = merge( 
    long_dt, 
    town_treatment_dt, 
    all.x = TRUE,


)


long_dt %>%
    colnames()
