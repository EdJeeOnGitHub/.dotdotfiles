
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  merge-zm-town-data.R [ --fake-data  ]
  Options:
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
    town_output_path = "data/output/fake-town-"
    district_output_path = "data/output/fake-district-month-vaccination-data.csv"
} else {
    input_path = "data/output/long-vaccination-data.csv"
    town_output_path = "data/output/town-"
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


create_aggregrate_data = function(vax_data,
                                  treatment_data,
                                  unit_aggregation_level, 
                                  time_aggregation_level) {
    vax_data[, date := floor_date(vac_date, time_aggregation_level)]
    dates = seq(
        from = ymd(min(vax_data$date)), 
        to = ymd(max(vax_data$date)), 
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
    if (!any(str_detect(unit_aggregation_level, "town"))) {
        empty_panel_df = empty_panel_df[, .(district_treat_date = unique(district_treat_date)),
                                        by = c("vaccine", "district", "date")]
    }
    vax_data[, date := floor_date(vac_date, time_aggregation_level)]
    collapsed_dt = vax_data[, 
        .(n_vaccinated = sum(vaccinated),
          n_timely = sum(vaccinated_on_time, na.rm = TRUE)), 
        by = c("vaccine", unit_aggregation_level, "date")] 
    if (any(is.na(collapsed_dt$n_vaccinated)) | any(is.na(collapsed_dt$n_timely))) {
        stop("NAs in collapsed vaccination numbers.")
    }
    
    empty_panel_dates = empty_panel_df[, date]
    data_dates = collapsed_dt[, date]
    
    if (length(setdiff(data_dates, empty_panel_dates)) > 0) {
        stop("Date merging error, dates present in data but not in 
              balanced panel.")
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

district_week_panel_dt = create_aggregrate_data(
    long_dt, 
    town_treatment_dt,
    unit_aggregation_level = "district", 
    time_aggregation_level = "weeks"
)

district_day_panel_dt = create_aggregrate_data(
    long_dt, 
    town_treatment_dt,
    unit_aggregation_level = "district", 
    time_aggregation_level = "days"
)






check_balanced_panel = function(data, unit_aggregation_level) {
    balanced_panel = data[, 
        .N, 
        by = c("vaccine", unit_aggregation_level, "date")][, all(N == 1)]
    return(balanced_panel)
}

town_balance_panel_checks = map(
    list(
        town_day_panel_dt, 
        town_week_panel_dt,
        town_month_panel_dt
    ),
    check_balanced_panel,
    unit_aggregation_level = "town" 
) %>% unlist()


if (!all(town_balance_panel_checks)) {
    stop("Not all town-date panels are balanced.")
}

district_panel_checks = map(
    list( 
        district_day_panel_dt, 
        district_week_panel_dt, 
        district_month_panel_dt
    ), 
    check_balanced_panel, 
    unit_aggregation_level = "district"
)



if (!all(district_panel_checks)) {
    stop("Not all district-date panels are balanced.")
}

town_output_paths = map(
    c("month", "week", "day"),
    ~paste0(town_output_path, .x, "-vaccination-data.csv")
)


map2(
    list(
        town_month_panel_dt,
        town_week_panel_dt,
        town_day_panel_dt
    ),
    town_output_paths,
    ~write_csv(
        .x,
        file = .y
    )
)

district_output_paths = map(
    c("month", "week", "day"),
    ~paste0(district_output_path, .x, "-vaccination-data.csv")
)


map2(
    list(
        district_month_panel_dt,
        district_week_panel_dt,
        district_day_panel_dt
    ),
    district_output_paths,
    ~write_csv(
        .x,
        file = .y
    )
)

