
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  generate-es-estimates.R [ --fake-data --time-aggregation-level=<time-aggregation-level> --vaccine=<vaccine> --unit-aggregation-level=<unit-aggregation-level>]

  Options:
  --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: month]
  --unit-aggregation-level=<unit-aggregation-level>  Level of unit to aggregate to. [default: town]
  --vaccine=<vaccine>  Which vaccine to analyse effects for.
  --start-event-time=<start-event-time>  Earliest time period to show event time plots.  [default: 104]
  --end-event-time=<end-event-time>  Latest time period to show event time plots.  [default: Inf]
"),
  args = if (interactive()) "--fake-data --time-aggregation-level=week --unit-aggregation-level=town  " else commandArgs(trailingOnly = TRUE)
) 
library(tidyverse)
library(lubridate)
library(glue)
library(did)
library(broom)
library(Matrix)

script_options = script_options %>%
    modify_at(c("start_event_time", "end_event_time"), as.numeric)
if (is.null(script_options$vaccine)) {
    script_options$vaccine = c(
        "penta1",
        "penta2",
        "penta3",
        "measles1",
        "measles2",
        "bcg"
    )
}

if (script_options$fake_data) {
    fake_output_name = "fake-"
} else {
    fake_output_name = ""
}
input_filename = fake_output_name 
output_filename = fake_output_name 
prep_data_filepath = fake_output_name 

input_filename = input_filename %>%
    str_c(., script_options$unit_aggregation_level) %>%
    str_c(., "-", script_options$time_aggregation_level) %>%
    str_c(., "-", script_options$vaccine) %>%
    map(~str_c(.x, "-attgt.rds" ))
output_filename = output_filename %>%
    str_c(., script_options$unit_aggregation_level) %>%
    str_c(., "-", script_options$time_aggregation_level) %>%
    str_c(., "-", script_options$vaccine) %>%
    map(~str_c(.x, "-es-plot.png" ))

prep_data_filepath = str_c(
    "data/output/",
    prep_data_filepath, 
    script_options$unit_aggregation_level, "-",
    script_options$time_aggregation_level, 
    "-prep-att-gt-data.csv"
    )
input_file_path = file.path("data/output", input_filename)
output_file_path = file.path("data/output/plots", output_filename)

att_gts = map(input_file_path, readRDS)
prep_unit_vax_dt = readRDS(prep_data_filepath)



tidy_es = att_gts %>%
    map(~aggte(
        MP = .x, 
        type = "dynamic",
        min_e = script_options$start_event_time*-1,
        max_e = script_options$end_event_time
        )) %>%
    imap(~tidy(.x) %>% mutate(vaccine = script_options$vaccine[.y])) %>%
    map(as_tibble)

plot_event_study = function(tidy_es, vax) {

    p = tidy_es %>%
        filter(vaccine == vax) %>%
        filter(!is.na(conf.low)) %>%
        ggplot(aes( 
            x = event.time, 
            y = estimate,
            ymin = point.conf.low, 
            ymax = point.conf.high, 
            colour = event.time >= 0
        )) +
        geom_pointrange() +
        geom_hline( 
            yintercept = 0, 
            linetype = "longdash"
        ) +
        theme_bw() +
        guides( 
            colour = "none"
        ) +
        labs(
            title = glue("Event Study: Change in number of {vax} vaccinations."),
            subtitle = glue("{str_to_title(script_options$unit_aggregation_level)} level estimates measured in {script_options$time_aggregation_level}s")
        )
        return(p)
}


es_plots = imap(
    tidy_es, 
    ~plot_event_study(.x,
                      vax = script_options$vaccine[.y])
)

iwalk( 
    es_plots, 
    ~ggsave(
        plot = .x, 
        filename = output_file_path[.y], 
        width = 8, 
        height = 6
    )
)



prep_unit_vax_dt[, group_id := as.character(group_id)]
tidy_group_estimates = att_gts %>%
    map(~aggte(MP = .x, type = "group")) %>%
    imap(~tidy(.x) %>% mutate(vaccine = script_options$vaccine[.y])) %>%
    map(as_tibble) %>%
    map( 
        ~left_join(
            .x,
            prep_unit_vax_dt[, .(district = unique(district), town = unique(town)), by = "group_id"], 
            by = c("group" = "group_id")
        )
    ) %>%
    map( 
        ~mutate(
            .x, 
            town = replace_na(town, "Average"), 
            district = replace_na(district, "Average"))
    )


plot_group_estimates = function(data, vax){
    p = data %>%
            ggplot(aes( 
                x = estimate, 
                xmin = conf.low, 
                xmax = conf.high, 
                y = reorder(!!rlang::sym(script_options$unit_aggregation_level), as.numeric(group)), 
                colour = reorder(district, -as.numeric(group))
            )) +
            geom_pointrange() +
            labs( 
                y = str_to_title(script_options$unit_aggregation_level), 
                x = "Estimate", 
                title = str_glue("Group level mCCT estimates of {vax} vaccinations "), 
                colour = "District"
            ) +
            theme_bw()
    return(p)
}


group_plots = imap( 
    tidy_group_estimates, 
    ~plot_group_estimates(
        data = .x, 
        vax = script_options$vaccine[.y]
    )
)


iwalk(
    group_plots, 
    ~ggsave(
        plot = .x, 
        filename = paste0(
            "data/output/plots/", 
            fake_output_name, 
            "group-attgt-estimates-",
            script_options$unit_aggregation_level,
            "-",
            script_options$time_aggregation_level,
            "-",
            script_options$vaccine[.y],
            "-plot.png"
        ),
        width = 8, 
        height = 6
    )
)
