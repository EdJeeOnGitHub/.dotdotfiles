library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

town_randomisation = read_xlsx(
    "data/input/Name of Towns in 7mCCT District - randomised and roll out date.xlsx", 
    sheet = "Randomised order", 
    skip = 1) %>%
    mutate(
        randomised = if_else( 
            row_number() < 4,
            FALSE,
            TRUE
        )
    ) %>%
    clean_names() %>%
    filter(district_order_randomised != "District (order randomised)" | is.na(district_order_randomised)) %>%
    rename( 
        district_roll_in_date = roll_in_date, 
        district = district_order_randomised
    )

wip_town_randomisation = town_randomisation %>%
fill( 
    district, .direction = "down"
)

east_gadap = wip_town_randomisation %>%
    filter(
        town_order == "East - Gadap"
    )  %>%
    mutate( 
        town_roll_in_date = ymd("2022/02/23")
    )



clean_town_randomisation = wip_town_randomisation %>%
    filter(
        town_order != "East - Gadap"
    ) %>%
    mutate( 
        town_roll_in_date = as.Date(as.numeric(roll_out_date), origin = "1899-12-30")
    ) %>%
    bind_rows(
        east_gadap
    ) %>%
    select(
        town_order,
        district_roll_in_date,
        town_roll_in_date, 
        randomised,
        district ) %>%
    arrange(town_roll_in_date) %>%
    group_by(district) %>%
    mutate(
        district_roll_in_date = unique(na.omit(district_roll_in_date)), 
        district_roll_in_date = dmy(district_roll_in_date), 
        town_roll_in_date_randomised = if_else( 
            randomised == FALSE, 
            district_roll_in_date, 
            town_roll_in_date
        )
        )  %>%
    ungroup() %>%
    rename( 
        town = town_order, 
        roll_in_randomised = randomised
    )

if (nrow(clean_town_randomisation) != 29) {
    stop("Number of town observations incorrect")
}

write_csv( 
    clean_town_randomisation, 
    "data/output/clean-town-randomisation-data.csv"
)


