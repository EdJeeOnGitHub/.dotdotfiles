library(tidyverse)
library(ggridges)
library(lubridate)
library(ggstance)
realised_RCT_sample = c(
    "Hyderabad",
    "Jacobabad",
    "Kambar",
    "Karachi Central",
    "Karachi East",
    "Karachi West",
    "Sujawal"
)
# # First table from 
# # https://docs.google.com/spreadsheets/d/1tXZOeZsU4D8dKIOwv9ROuSu13JwFXd7z?rtpof=true&authuser=tamtrinh%40uchicago.edu&usp=drive_fs
# aug_df = read_csv("data/init-vax-rates-aug.csv")
# jul_df = read_csv("data/init-vax-rates-jul.csv")


# # Create long data and add 20th percentile

# create_long_data = function(df){
#     long_df = df %>%
#         pivot_longer(
#             cols = `BCG^^`:`Measles-2^`,
#             names_to = "metric"
#         ) %>%
#         mutate(
#             metric = str_remove_all(metric, "\\^")
#         ) 
#     return(long_df)
# }


# jul_long_df = jul_df %>%
#     create_long_data() %>%
#     group_by(metric) %>%
#     mutate(percentile_20 = quantile(value, 0.2)) %>%
#     ungroup() %>%
#     mutate(date = "jul")

# aug_long_df = aug_df %>%
#     create_long_data() %>%
#     group_by(metric) %>%
#     mutate(percentile_20 = quantile(value, 0.2)) %>%
#     ungroup() %>%
#     mutate(date = "aug")


# # verify 20th percentile procedure recreates realised RCT sample:
# recreate_RCT_sample = aug_long_df %>%
#     mutate(
#         accept_criteria = if_else(
#             metric %in% c("Penta-3", "Measles-1") & value < percentile_20, 
#             TRUE,
#             FALSE
#         )
#     ) %>%
#     filter(accept_criteria == TRUE) %>%
#     select(district) %>%
#     pull() %>%
#     unique()

# if (identical(realised_RCT_sample, recreate_RCT_sample) == FALSE) {
#     stop("Recreating assignment doesn't replicate realised sample.")
# }

cohort_vax_df = read_csv("data/output/clean-preprogramme-metrics.csv")

vax_2020_df = cohort_vax_df %>%
    filter(birth_year == 2020) 


vax_2020_df = vax_2020_df %>%
    group_by(vaccine, vax_year_mon) %>%
    mutate(percentile_20 = quantile(pct_vaxxed_all, 0.2)) %>%
    ungroup() 

agg_vax_df = cohort_vax_df %>%
    group_by(district, vaccine, vax_year_mon) %>%
    summarise(n_vaxxed = sum(n_vaxxed), .groups = "drop")

library(ggstance)
agg_vax_df %>%
    group_by(
        district, vaccine
    ) %>%
    mutate( 
        diff = n_vaxxed - lag(n_vaxxed)
    ) %>%
    nest() %>%
    mutate( 
        model = map(data, ~lm(diff ~ n_vaxxed, data = .)), 
        tidy_model = map(model, broom::tidy, conf.int = TRUE) 
    ) %>%
    unnest(tidy_model) %>%
    filter(term == "n_vaxxed") %>%
    select(-data, -model) %>%
    ggplot(aes( 
        x = estimate, 
        xmin = conf.low, 
        xmax = conf.high, 
        colour = vaccine, 
        y = district
    )) +
    geom_pointrange(position = position_dodge2v(height = 0.5)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    theme_bw()

agg_vax_df %>%
    ggplot(aes(x = vax_year_mon, y = n_vaxxed, colour = district)) +
    geom_point() +
    geom_line() +
    facet_wrap(~vaccine, ncol = 1)

agg_vax_df %>%
    filter(!(vaccine %in% c("Measles-1", "Measles-2"))) %>%
    group_by(
        district, vaccine
    ) %>%
    mutate(n_vaxxed = log1p(n_vaxxed)) %>%
    mutate( 
        diff = n_vaxxed - lag(n_vaxxed)
    ) %>%
    group_by(vaccine) %>%
    nest() %>%
    mutate( 
        model = map(data, ~lm(diff ~ n_vaxxed, data = .)), 
        tidy_model = map(model, broom::tidy, conf.int = TRUE) 
    ) %>%
    unnest(tidy_model) %>%
    filter(term == "n_vaxxed") %>%
    select(-data, -model) 

create_acceptance_criteria = function(data, vax_list, value) {
    percentile_data = data %>%
        group_by(vaccine, vax_year_mon) %>%
        mutate(percentile_20 = quantile({{ value }}, 0.58)) %>%
        ungroup()

    percentile_data = percentile_data %>%
        mutate(
            accept_criteria = if_else(
                vaccine %in% vax_list &  {{ value }} < percentile_20,
                TRUE,
                FALSE)
        )  %>%
        group_by(district, vax_year_mon) %>%
        mutate(accept_criteria = any(accept_criteria)) %>%
        ungroup()
    return(percentile_data)
}


alternative_metric_choices_p = expand.grid(
    choice_1 = c("BCG", "Penta-1", "Penta-2", "Penta-3", "Measles-1", "Measles-2"),
    choice_2 = c("BCG", "Penta-1", "Penta-2", "Penta-3", "Measles-1", "Measles-2"),
    date = c("aug", "jul")) %>%
    as_tibble() %>%
    # Remove cases choice 1 and 2 the same
    filter(choice_1 != choice_2)  %>%
    filter(!(str_detect(choice_1, "Penta") & str_detect(choice_2, "Penta"))) %>%
    filter(!(str_detect(choice_1, "Measles") & str_detect(choice_2, "Measles")))
alternative_metric_choices = alternative_metric_choices_p[!duplicated(t(apply(alternative_metric_choices_p, 1, sort))),]

alternative_metric_choices = alternative_metric_choices %>%
    mutate(metric_number = paste0("metric_", 1:n()))
alternative_metric_choices_no_date = alternative_metric_choices %>%
    select(choice_1, choice_2) %>%
    unique()
pr_assign = map2_dfr(
    alternative_metric_choices_no_date$choice_1,
    alternative_metric_choices_no_date$choice_2,
    ~create_acceptance_criteria(
        data = vax_2020_df,
        vax_list = c(.x, .y),
        value = pct_vaxxed_poss
    ) %>%
    group_by(district, vax_year_mon) %>%
    summarise(accepted = unique(accept_criteria), .groups = "drop")
)

pr_assign_table = pr_assign %>%
    group_by(district) %>%
    summarise(
        pr_accept = mean(accepted), 
        sd_accept = sd(accepted),
        se_accept = sd_accept/sqrt(nrow(pr_assign))
    ) %>%
    mutate(
        in_rct = district %in% realised_RCT_sample
    )
pr_assign_table %>%
    write_csv("data/output/incorrect-prop-score.csv")

pr_assign_table %>%
    mutate(district = factor(district)) %>%
    mutate(district = fct_reorder(district, pr_accept)) %>%
    ggplot(aes( 
        y = district, 
        x = pr_accept,
        xmin = pr_accept - 1.96*se_accept, 
        xmax = pr_accept + 1.96*se_accept, 
        colour  = in_rct
    )) +
    geom_pointrangeh() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = "Experimental Propensity Scores", 
         colour = "Experimental Sample", 
         x = "Probability in RCT", 
         caption = "These are using a slightly different metric and so are a little off.")
ggsave("data/output/incorrect-prop-score-plot.png", 
        width = 8,
        height = 6)

vax_2020_df %>%
    create_acceptance_criteria(
        vax_list = c("Penta-3", "Measles-1"), 
        value = pct_vaxxed_poss
    ) %>%
    group_by(district, vax_year_mon) %>%
    summarise(accepted = unique(accept_criteria), .groups = "drop") %>%
    group_by(district) %>%
    summarise( 
        pr_accept = mean(accepted)
    )




vax_2020_df %>%
    create_acceptance_criteria(
        vax_list = c("Penta-3", "Measles-1"), 
        value = pct_vaxxed_poss
    ) %>%
    select(
        district, 
        vaccine, 
        vax_year_mon, 
        pct_vaxxed_poss,
        accept_criteria
    ) %>%
    pivot_wider(
        names_from = c(vaccine), 
        values_from = pct_vaxxed_poss
   ) %>%
    filter(vax_year_mon == ymd("2021-08-01"))
    



vax_2020_df %>%

    mutate(accept_criteria = if_else(
        vaccine %in% c("Penta-3", "Measles-1") &  pct_vaxxed_all < percentile_20,
        TRUE,
        FALSE
    )) %>%
    group_by(district) %>%
    summarise(mean_accept = mean(accept_criteria))
    select(district,
           vax_year_mon,
           vaccine, 
           accept_criteria)

    ggplot(aes( 
        x = vax_year_mon, 
        colour = district, 
        y = accept_criteria
    )) +
    geom_line()


vax_2020_df


vax_2020_df %>%
    filter(
        month(vax_year_mon) == 8
    ) %>%
    select(
        district, 
        vaccine, 
        vax_year_mon,
        pct_vaxxed_all, 
        pct_vaxxed_poss)



vax_2020_df %>%
    ggplot(aes( 
        x = vax_year_mon,
        y = pct_vaxxed_all, 
        colour = district
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~vaccine)
vax_2020_df %>%
    ggplot(aes( 
        x = vax_year_mon,
        y = n_vaxxed, 
        colour = district
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~vaccine)

vax_df %>%
    filter(month(vax_year_mon) == 8 & year == "")

jul_df %>%
    filter(district %in% c("Jacobabad", "Hyderabad", "Kambar"))

## Some quick plots
p_vax_metrics = aug_long_df %>%
    ggplot(
        aes(x = value,
            fill = metric,
            y = metric,
            height = stat(density)),
            alpha = 0.5
    ) +
    stat_density_ridges( 
                        quantile_lines = TRUE,
                        quantiles = 0.2, 
                        alpha = 0.5,
                        draw_baseline = FALSE) +
    guides(fill = "none") +
    theme_ridges() + 
    labs(title = "Distribution of Vaccination Metrics",
         caption = "20th percentile highlighted.")
ggsave("man/vax-metrics-density.png", width = 8, height = 6)
p_vax_metrics


p_rct_vax_metrics = aug_long_df %>%
    mutate(in_sample = district %in% realised_RCT_sample, 
           metric = if_else(metric %in% c("Penta-3", "Measles-1"), 
                            paste0(metric, "***"), 
                            metric)) %>%
    ggplot(
        aes(x = value,
            fill = in_sample,
            y = metric,
            height = stat(density)),
            alpha = 0.5, 
    ) +
    geom_density_ridges(stat = "binline",
                        bins = 60, 
                        alpha = 0.5,
                        draw_baseline = FALSE) +
    guides(fill = "none") +
    theme_ridges() + 
    labs(title = "Distribution of Vaccination Metrics",
         caption = "RCT sample districts in blue.")
p_rct_vax_metrics
ggsave("man/rct-vax-metrics-density.png", width = 8, height = 6)



# Create alternative metrics
alternative_metric_choices_p = expand.grid(
    choice_1 = c("BCG", "Penta-1", "Penta-2", "Penta-3", "Measles-1", "Measles-2"),
    choice_2 = c("BCG", "Penta-1", "Penta-2", "Penta-3", "Measles-1", "Measles-2"),
    date = c("aug", "jul")) %>%
    as_tibble() %>%
    # Remove cases choice 1 and 2 the same
    filter(choice_1 != choice_2)  %>%
    filter(!(str_detect(choice_1, "Penta") & str_detect(choice_2, "Penta"))) %>%
    filter(!(str_detect(choice_1, "Measles") & str_detect(choice_2, "Measles")))
alternative_metric_choices = alternative_metric_choices_p[!duplicated(t(apply(alternative_metric_choices_p, 1, sort))),]

alternative_metric_choices = alternative_metric_choices %>%
    mutate(metric_number = paste0("metric_", 1:n()))
alternative_metric_choices
all_metrics_df = bind_rows(
    aug_long_df,
    jul_long_df
)

create_assignment = function(metric_df, metric_1, metric_2, date_val, bw = 1){

    assigned_metric_df = metric_df %>%
        mutate(
            accept_criteria = if_else(
                metric %in% c(metric_1, metric_2) & date == date_val & value < percentile_20*bw, 
                TRUE,
                FALSE
            )
        ) 

    counterfactual_draws = assigned_metric_df %>%
        filter(accept_criteria == TRUE) %>%
        select(district) %>%
        pull() %>%
        unique()
    return(counterfactual_draws)
}


every_district = unique(all_metrics_df$district)
check_district_present = function(experimental_list, all_districts){
    all_district_df = tibble(district = all_districts) %>%
        mutate(present := district %in% experimental_list)
    return(all_district_df)

}

alternative_assignment_df = pmap_dfr(
    list(
        alternative_metric_choices$choice_1,
        alternative_metric_choices$choice_2,
        alternative_metric_choices$date,
        alternative_metric_choices$metric_number
    ), 
    ~check_district_present(
        create_assignment(all_metrics_df, ..1, ..2, ..3, bw = 1),
        every_district
    ) %>%
    mutate(metric_n = ..4)
)




alternative_assignment_RD_df = pmap_dfr(
    list(
        alternative_metric_choices$choice_1,
        alternative_metric_choices$choice_2,
        alternative_metric_choices$date,
        alternative_metric_choices$metric_number
    ), 
    ~check_district_present(
        create_assignment(all_metrics_df, ..1, ..2, ..3, bw = 1.05),
        every_district
    ) %>%
    mutate(metric_n = ..4)
)


alternative_assignment_df  = alternative_assignment_df %>%
    group_by(district) %>%
    mutate(var_present = var(present), 
           always_in = var_present == 0 & unique(present) == TRUE) %>% 
    ungroup()
alternative_assignment_RD_df  = alternative_assignment_RD_df %>%
    group_by(district) %>%
    mutate(var_present = var(present), 
           always_in = var_present == 0 & unique(present) == TRUE) %>% 
    ungroup()



alternative_assignment_df %>%
    ggplot(aes( 
        x = metric_n, 
        y = district,
        fill = present
    )) +
    geom_tile(color = "black") +
    theme_bw() +
    theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave("man/all-metric-shuffling.png", width = 8, height = 6)


alternative_assignment_df %>%
    group_by(district) %>%
    mutate(var_present = var(present), 
           always_in = var_present == 0 & unique(present) == TRUE) %>%
    filter(var_present != 0 | always_in) %>%
    ggplot(aes( 
        x = metric_n, 
        y = district,
        fill = present
    )) +
    geom_tile(colour = "black") +
    theme_bw() +
    theme(
        panel.grid.minor = element_line(colour="black", size=0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave("man/subset-metric-shuffling.png", width = 8, height = 6)



all_metrics_df = all_metrics_df %>%
    mutate(lte_20 = as.numeric(value < percentile_20))

rd_df = all_metrics_df %>%
    filter(date == "aug", metric == "Penta-3")


all_metrics_df

alternative_assignment_df 


# alternative_assignment_RD_df %>%
#     group_by(district) %>%
#     mutate(var_present = var(present), 
#            always_in = var_present == 0 & unique(present) == TRUE) %>%
#     filter(var_present != 0 | always_in) %>%
#     ggplot(aes( 
#         x = metric_n, 
#         y = district,
#         fill = present
#     )) +
#     geom_tile(colour = "black") +
#     theme_bw() +
#     theme(
#         panel.grid.minor = element_line(colour="black", size=0.5),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank())



pop_df = read_csv("data/misc-pop-data-back-of-envelope.csv") %>%
    rename(pop = `Total Population 2020`)
pop_df
alt_districts = alternative_assignment_df %>%   
    filter(var_present != 0) %>%
    select(district) %>%
    unique() %>%
    pull()
new_alt_regions = setdiff(alt_districts, realised_RCT_sample)



length(new_alt_regions)/length(realised_RCT_sample) * 100


orig_pop = pop_df %>%
    filter(District %in% realised_RCT_sample) %>%
    summarise(total_pop = sum(pop)) %>%
    pull()


extra_pop = pop_df %>%
    filter(District %in% new_alt_regions) %>%
    summarise(total_pop = sum(pop)) %>%
    pull()
extra_pop / orig_pop * 100


calculate_within_RD_bw = function(df, bw){
    bw_df = df %>%
        group_by(metric, date) %>%
        mutate(std_score_bw = bw*sd(value),
               upper_cutoff = percentile_20 + std_score_bw, 
               lower_cutoff = percentile_20 - std_score_bw,
               within_bw = value < upper_cutoff & value > lower_cutoff)  %>%
        ungroup()
    return(bw_df)
}

extract_bw_districts = function(bw_df, metrics){
    districts = bw_df %>%
    filter(within_bw) %>%
    filter(metric %in% metrics ) %>%
    select(district) %>%
    unique() %>%
    pull()
    return(districts)
}


jul_aug_015 = all_metrics_df %>%
    calculate_within_RD_bw(bw = 0.15) %>%
    extract_bw_districts(c("Measles-1", "Penta-3"))
jul_aug_01 = all_metrics_df %>%
    calculate_within_RD_bw(bw = 0.1) %>%
    extract_bw_districts(c("Measles-1", "Penta-3"))
jul_aug_005 = all_metrics_df %>%
    calculate_within_RD_bw(bw = 0.05) %>%
    extract_bw_districts(c("Measles-1", "Penta-3"))



all_vax_metric_names = unique(all_metrics_df$metric)
jul_aug_015_all = all_metrics_df %>%
    calculate_within_RD_bw(bw = 0.15) %>%
    extract_bw_districts(all_vax_metric_names)
jul_aug_01_all = all_metrics_df %>%
    calculate_within_RD_bw(bw = 0.1) %>%
    extract_bw_districts(all_vax_metric_names)
jul_aug_005_all = all_metrics_df %>%
    calculate_within_RD_bw(bw = 0.05) %>%
    extract_bw_districts(all_vax_metric_names)


setdiff(jul_aug_015, realised_RCT_sample)
setdiff(jul_aug_01, realised_RCT_sample)
setdiff(jul_aug_005, realised_RCT_sample)

setdiff(jul_aug_015_all, realised_RCT_sample)
setdiff(jul_aug_01_all, realised_RCT_sample)
setdiff(jul_aug_005_all, realised_RCT_sample)

new_alt_regions

RD_simple_districts = aug_long_df %>%
    group_by(metric) %>%
    mutate(std_score_bw = 0.15*sd(value),
           upper_cutoff = percentile_20 + std_score_bw, 
           lower_cutoff = percentile_20 - std_score_bw) %>%
    filter(value < upper_cutoff) %>%
    filter(metric %in% c("Measles-1", "Penta-3")) %>%
    ungroup() %>%
    select(district) %>%
    unique() %>%
    pull()


setdiff(RD_simple_districts, realised_RCT_sample)



aug_long_df %>%
    group_by(metric) %>%
    summarise(st)


plot_bw = function(df, bw){
    p = df %>%
        group_by(metric) %>%
        mutate(
            metric = if_else(metric %in% c("Penta-3", "Measles-1"), 
                                paste0(metric, "***"), 
                                metric), 
                std_score_bw =bw*sd(value)) %>%
        ggplot(aes(
            x = value,
            y = value > percentile_20,
            color = value < percentile_20
        )) +
        geom_point() +
        geom_vline(aes(xintercept = percentile_20), 
                linetype = "dashed") +
        geom_vline(aes(xintercept = percentile_20-std_score_bw), 
                linetype = "dotted") +
        geom_vline(aes(xintercept = percentile_20+std_score_bw), 
                linetype = "dotted") +
        facet_wrap(~metric,
                    ncol = 1, 
                    scales = "free_x")  +
        guides(color = "none") +
        theme_bw()  +
        labs(title = paste0("Bandwidth: ", bw, " standard deviation"), 
             y = "In RCT")
    return(p)
}

aug_long_df %>%
    plot_bw(0.15)
ggsave("bw-015-sd.png", width = 8, height = 6)
aug_long_df %>%
    plot_bw(0.1)
ggsave("bw-010-sd.png", width = 8, height = 6)
aug_long_df %>%
    plot_bw(0.05)
ggsave("bw-005-sd.png", width = 8, height = 6)







ggsave("man/RD-metrics.png", width = 8, height = 6)







