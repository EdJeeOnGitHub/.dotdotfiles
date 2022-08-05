library(tidyverse)
library(lubridate)




shared_with_us_coverage = read_csv("data/input/ed-epi-copy.csv")

long_shared_with_us_coverage = shared_with_us_coverage %>%
    gather(variable, value, -district) %>%
    rename(selection_value = value, vaccine = variable)
long_shared_with_us_coverage

clean_df = read_csv("data/output/clean-preprogramme-metrics.csv")



clean_df %>%
    filter(vaccine == "BCG") %>%
    group_by(
        district, 
        birth_year
    ) %>%
    filter(year(vax_year_mon) == 2020) %>%
    summarise( 
        total_pct = unique(total_pct)
    ) %>%
    filter(birth_year == 2020) 




clean_df %>%
    filter( 
        vaccine == "BCG"
    ) %>%
    filter(birth_year == 2020) %>%
    filter(vax_year_mon <= ymd("2021-09-01") & vax_year_mon >= ymd("2020-01-01")) %>%
    group_by(district) %>%
    summarise( 
        n_vaxxed = sum(n_vaxxed)
    )

clean_df %>%
    group_by(district, birth_year) %>%
    summarise(total_pop = unique(monthly_births*12))
calculate_coverage = function(data, birth_cohort, cutoff_date) {
    cov_data = data %>%
        filter(birth_year == birth_cohort) %>%
        filter(vax_year_mon < ymd(cutoff_date)) %>%
        group_by(
            district, vaccine
        ) %>%
        summarise( 
            n_vaxxed = sum(n_vaxxed), 
            annual_births = mean(monthly_births*12)
        ) %>%
        mutate( 
            coverage = (n_vaxxed / annual_births)*100
        )

    return(cov_data)
}


aug_our_coverage = calculate_coverage(clean_df, 2020, "2021-09-01")


comp_df = inner_join( 
    aug_our_coverage, 
    long_shared_with_us_coverage, 
    by = c("district", "vaccine")
)

comp_df %>%
    ggplot(aes( 
        x = selection_value, 
        y = coverage, 
        colour = vaccine
    )) +
    geom_point() +
    geom_abline( 
        linetype = "longdash"
    ) +
    theme_bw() +
    labs( 
        title = "Comparing our aggregate coverage estimates with IRD selection estimates",
        y = "Current coverage estimates", 
        x = "Selection coverage estimates"
    )



ggsave( 
    filename = "data/output/agg-comp-plot.png", 
    width = 8, 
    height = 6
)





potential_cutoffs = seq(
    from = ymd("2020-01-01"), 
    to = ymd("2021-09-01"), 
    by = "months"
)




all_coverage_dfs = map_dfr(
    potential_cutoffs, 
    ~calculate_coverage(
        data = clean_df, 
        birth_cohort = 2020,
        cutoff_date = .x
    ) %>% mutate(cutoff_date = .x)
)

all_coverage_dfs %>%
    ungroup() %>%
    select(vaccine) %>%
    unique()
p = all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    filter(vaccine == "Measles-1") %>%
    ggplot(aes( 
        x = rank, 
        fill = district
    )) +
    geom_bar() +
    facet_wrap(~district) +
    geom_vline(xintercept = 7, linetype = "longdash")
all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district) %>%
    summarise(
        pct_times_in = mean(rank <= 7),
        sd_times_in = sd(rank <= 7)
    )

all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district) %>%
    summarise(
        pct_times_mCCT = mean(rank <= 7)*100
    ) %>%
    arrange(pct_times_mCCT) 

all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district) %>%
    filter(year(cutoff_date) == 2021) %>%
    summarise(
        pct_times_mCCT = mean(rank <= 7)*100
    ) %>%
    arrange(pct_times_mCCT) 


realised_mCCT = c(
    "Hyderabad",
    "Jacobabad",
    "Kambar",
    "Karachi Central",
    "Karachi East",
    "Karachi West",
    "Sujawal"
)
all_coverage_dfs %>%
    mutate( 
        district = if_else( 
            district %in% realised_mCCT, 
            paste0("**", district, "**"), 
            district
        ) 
    ) %>%
    ungroup() %>%
    select(district) %>%
    unique()


library(glue)
library(ggtext)
highlight = function(x, color="black", family="") {
  ifelse(x %in% c(realised_mCCT, "Measles-1", "Penta-3"), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

count_in_mCCT_2021 = all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district, vaccine) %>%
    filter(year(cutoff_date) == 2021) %>%
    summarise(
        n_times_in_mCCT = sum(rank < 7)
    )

all_coverage_dfs %>%
    group_by(district, vaccine) %>%
    filter(year(cutoff_date) == 2021) %>%
    summarise(n = n()) %>%
    filter(n != 9)

count_in_mCCT_2021 %>%
    ungroup() %>%
    ggplot(aes( 
        y = reorder(district, n_times_in_mCCT), 
        x = vaccine, 
        fill = n_times_in_mCCT
    )) + 
    geom_tile(colour = "black") +
    scale_fill_viridis_c(option = "inferno") +
    scale_y_discrete(labels= function(x) highlight(x, "red")) +
    scale_x_discrete(labels= function(x) highlight(x, "red")) +
    labs(
        y = "District", 
        x = "Vaccine", 
        title = "Heatmap of mCCT inclusion",
        subtitle = "If coverage calculated at end of each month: Jan 2021-Sep 2021",
        fill = "N Times in mCCT", 
        caption = "Red labels indicate actual mCCT districts and vaccines used."
    ) +
  theme(
    axis.text.y=element_markdown(size = 15), 
    axis.text.x=element_markdown()
  )

ggsave( 
    "data/output/rough-mCCT-inclusion.png", 
    width = 8, 
    height = 8
)

all_coverage_dfs %>%
    filter( 
        cutoff_date == "2021-09-01"
    ) %>%
    filter(vaccine == "Measles-1" | vaccine == "Penta-3") %>%
    group_by(vaccine) %>%
    mutate(rank = rank(coverage)) %>%
    filter(rank < 7) %>%
    ungroup() %>%
    select(district) %>%
    unique()


long_shared_with_us_coverage %>%
    group_by(vaccine) %>%
    mutate(rank = rank(selection_value)) %>%
    ungroup() %>%
    filter(vaccine %in% c("Measles-1", "Penta-3")) %>%
    filter(rank < 7) %>%
    select(district) %>%
    unique()


all_coverage_dfs %>%
    filter(cutoff_date == "2021-09-01") %>%
    filter(vaccine == "Penta-3") %>%
    ungroup() %>%
    mutate(rank = rank(coverage)) %>%
    arrange(rank)


ggsave("")
all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district) %>%
    filter(year(cutoff_date) == 2021) %>%
    summarise(
        pct_times_mCCT = mean(rank <= 7)*100
    ) %>%
    arrange(pct_times_mCCT)  %>%
    mutate(district = factor(district), 
           district = fct_reorder(district, pct_times_mCCT)) %>%
    ggplot(aes( 
        y = district, 
        x = 1, 
        fill = pct_times_mCCT
    )) + 
    geom_tile(colour = "black") +
    scale_fill_viridis_c(option = "magma")

all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage)) %>%
    mutate(in_mCCT = rank <= 7) %>%
    ggplot(aes(x = vaccine, y = district, fill = )) +
    geom_tile
