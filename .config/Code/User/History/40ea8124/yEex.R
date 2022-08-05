
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
    estimate-att-gt.R [options] 
    
    Options:
        --fake-data  Whether to run model on fake data or not.
        --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: week]
        --unit-aggregation-level=<unit-aggregation-level>  Level of unit to aggregate to. [default: town]
        --num-cores=<num-cores>  NOT WORKING Number of cores to pass to did function. [default: 1]
        --additional-districts  NOT IMPLEMENTED YET
        --propensity-weight  NOT IMPLEMENTED YET
        --num-bootstrap-draws  NOT IMPLEMENTED YET
"),
  args = if (interactive()) "--fake-data --time-aggregation-level=week --num-cores=6" else commandArgs(trailingOnly = TRUE)
) 

script_options
library(data.table)
library(tidyverse)
library(lubridate)
library(glue)
library(did)

script_options = script_options %>%
    modify_at(vars(contains("num")), as.integer)



if (script_options$fake_data) {
    input_path = paste0(
        "data/output/fake-", 
        script_options$unit_aggregation_level,
        "-",
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
} else {
    input_path = paste0(
        "data/output/", 
        script_options$unit_aggregation_level,
        "-",
        script_options$time_aggregation_level,
        "-vaccination-data.csv")
}

vax_dt = fread(input_path)

if (script_options$fake_data) {
    vax_dt = vax_dt[date > "2021-06-01" & date < "2024-01-01"]
}

vaccines = vax_dt[, unique(vaccine)]





prepare_data_for_did = function(df, first_treat_var, time_period, unit_var ){
    prep_df = copy(df)
    if (time_period == "day") {

        prep_df[, 
            time_id := interval(min(date), date) %/% days(1)]
    prep_df[, group_id := interval(min(date), get(first_treat_var)) %/% days(1)]

    } else if (time_period == "week") {

        prep_df[, 
            time_id := interval(min(date), date) %/% weeks(1)]
    prep_df[, group_id := interval(min(date), get(first_treat_var)) %/% weeks(1)]

    } else {

        prep_df[, 
            time_id := interval(min(date), date) %/% months(1)]
    prep_df[, group_id := interval(min(date), get(first_treat_var)) %/% months(1)]

    }

    prep_df[, unit_id := .GRP, by = unit_var]

    return(prep_df)
}

first_treat_var_name = paste0(script_options$unit_aggregation_level, "_treat_date")

prep_vax_dt = prepare_data_for_did(
    vax_dt, 
    first_treat_var = first_treat_var_name, 
    time_period = script_options$time_aggregation_level,
    unit_var = script_options$unit_aggregation_level
)

simple_att_gts = map(
    vaccines,
    ~att_gt(
        yname = "n_vaccinated", 
        tname = "time_id",
        idname = "unit_id",
        clustervars = "district",
        panel = TRUE,
        gname = "group_id",
        data = prep_vax_dt[vaccine == .x],
        control_group = "notyettreated"
    )
)



fake_output_name = if (script_options$fake_data) {
    "fake-"
} else {
    ""
}

saveRDS(
    prep_vax_dt, 
    paste0(
        "data/output/", 
        fake_output_name, 
        script_options$unit_aggregation_level, 
        "-", 
        script_options$time_aggregation_level, 
        "-prep-att-gt-data.csv"
    )
)


iwalk(
    simple_att_gts, 
    ~saveRDS(
        .x, 
        file = file.path(
            "data/output/", 
            paste0(
                    fake_output_name,
                   script_options$unit_aggregation_level,
                   "-",
                   script_options$time_aggregation_level,
                   "-", 
                   vaccines[.y], 
                   "-attgt.rds")) )
)

