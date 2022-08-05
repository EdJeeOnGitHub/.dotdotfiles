
script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  generate-es-estimates.R [ --fake-data --time-aggregation-level=<time-aggregation-level> --vaccine=<vaccine> --unit-aggregation-level=<unit-aggregation-level>]

  Options:
  --time-aggregation-level=<time-aggregation-level>  Level of time to aggregate to. [default: month]
  --unit-aggregation-level=<unit-aggregation-level>  Level of unit to aggregate to. [default: town]
  --vaccine=<vaccine>  Which vaccine to analyse effects for.
"),
  args = if (interactive()) "--fake-data --time-aggregation-level=week --unit-aggregation-level=town  " else commandArgs(trailingOnly = TRUE)
) 
library(tidyverse)
library(lubridate)
library(glue)
library(did)
library(broom)
library(Matrix)


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
    input_filename = "fake-"
    output_filename = "fake-"
    prep_data_filepath = "fake-"
} else {
    input_filename = ""
    output_filename = ""
    prep_data_filepath = ""
}

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

prep_data_filepaths = str_c(
    "data/output/",
    prep_data_filepath, 
    script_options$unit_aggregation_level, "-",
    script_options$time_aggregation_level, 
    "-prep-att-gt-data.csv"
    )
input_file_path = file.path("data/output", input_filename)
output_file_path = file.path("data/output/plots", output_filename)

att_gts = map(input_file_path, readRDS)
prep_data_dfs = map(prep_data_filepaths, readRDS)

prep_town_vax_dt = prep_data_dfs[[1]]
prep_district_vax_dt = prep_data_dfs[[2]]
rm(prep_data_dfs)



tidy_es = att_gts %>%
    map(~aggte(MP = .x, type = "dynamic")) %>%
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

stop()

att_gts[[1]] %>%
    aggte(type = "group")

tidy_group_estimates = att_gts %>%
    map(~aggte(MP = .x, type = "group")) %>%
    imap(~tidy(.x) %>% mutate(vaccine = script_options$vaccine[.y])) %>%
    map(as_tibble)

prep_town_vax_dt

tidy_group_estimates[[1]]

prepend()
tidy_group_estimates[[1]] %>%
    ggplot(aes( 
        y = group,
        x = estimate, 
        xmin = conf.low, 
        xmax = conf.high
    )) +
    geom_pointrange()


tidy_es = att_gts %>%
    map(~aggte(MP = .x, type = "dynamic")) %>%
    imap(~tidy(.x) %>% mutate(vaccine = script_options$vaccine[.y])) %>%
    map(as_tibble)