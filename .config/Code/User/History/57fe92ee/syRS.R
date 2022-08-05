library(tidyverse)
library(data.table)
library(lubridate)



fake_df = fread("data/output/fake-long-vaccination-data.csv")
town_df = fread("data/output/clean-town-randomisation-data.csv")

fake_df[, 
 `:=`(
    vaccinationdate = ymd(vaccinationdate),
    vaccinationduedate = ymd(vaccinationduedate), 
    vac_date = ymd(vac_date)
 )]
town_df[, town_roll_in_date_randomised := ymd(town_roll_in_date_randomised)]
town_df %>%
    colnames()
df
fake_df[, ymd(vac_date)]

dates = seq(
    from = ymd("2022-01-01"), 
    to = ymd("2022-08-01"), 
    by = 1
)
dates

create_fake_data = function(template_df, new_district, new_town){
    new_df = template_df[, `:=`(district = new_district, town = new_town)]
    new_df = merge(
        new_df, 
        town_df[, .(district, town, town_roll_in_date_randomised)], 
        by = c("district", "town"), 
        all.x = TRUE
    )
    new_df[, sampled_date := sample(dates, 1), by = c("vaccine", "childid")]
    new_df[, `:=`(vac_date = sampled_date, vaccinationdate = sampled_date, due_date = sampled_date + round(runif(.N, min = -5, max = 30)))]
    new_df[, sampled_date := NULL]
    new_df[vac_date < town_roll_in_date_randomised, move_date := runif(.N)  < 0.5]
    new_df[move_date == TRUE, vac_date := town_roll_in_date_randomised + (town_roll_in_date_randomised - vac_date)]
    new_df[move_date == TRUE, vaccinationdate := town_roll_in_date_randomised + (town_roll_in_date_randomised - vaccinationdate)]
    new_df[move_date == TRUE, vaccinationduedate := town_roll_in_date_randomised + (town_roll_in_date_randomised - vaccinationduedate)]
    new_df[, move_date := NULL]
    new_df[, childid := paste0("fake_", new_district, "_", new_town, childid)]
    new_df[, town_roll_in_date_randomised := NULL]
    return(new_df)
}


large_fake_df = map2(
    town_df[, district], 
    town_df[, town], 
    ~create_fake_data(
        template_df = fake_df, 
        new_district = .x, 
        new_town = .y
    )
) %>%
    rbindlist()

write_csv(
    large_fake_df,
    "data/output/large-fake-long-vaccination-data.csv"
)
