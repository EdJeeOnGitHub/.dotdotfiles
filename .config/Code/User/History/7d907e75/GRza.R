library(tidyverse)
library(lubridate)




shared_with_us_bcg_coverage = read_csv("data/input/ed-epi-copy.csv")
shared_with_us_bcg_coverage = shared_with_us_bcg_coverage %>%
    mutate(pct = (n_vaccinated/n_total)*100)



clean_df = read_csv("data/output/clean-preprogramme-metrics.csv")


clean_df %>%
    colnames()
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
            annual_births = unique(monthly_births*12)
        ) %>%
        mutate( 
            coverage = (n_vaxxed / annual_births)*100
        )

    return(cov_data)
}



aug_bcg_coverage = calculate_coverage(clean_df, 2020, "2021-09-01") %>%
    filter(vaccine == "BCG")


aug_bcg_coverage %>%
    select(district, n_vaxxed, annual_births, coverage)



p_comp_agg_data = inner_join( 
    aug_bcg_coverage, 
    shared_with_us_bcg_coverage %>% 
        rename(
            n_total_shared = n_total, 
            n_vaccinated_shared = n_vaccinated, 
            pct_shared = pct 
        )
) %>%
    ggplot(aes( 
        x = n_vaxxed, 
        y = n_vaccinated_shared
    )) +
    geom_point() +
    geom_abline(linetype = "longdash") +
    theme_bw() +
    labs(
        x = "Aggregate data shared with UChicago", 
        y = "Selection data shared with UChicago", 
        title = "Slight discrepancy between selection data shared with us
and other aggregate data shared with us", 
caption = "N vaccinated for BCG in 2020 cohort"
    )


ggsave( 
    p_comp_agg_data, 
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

install.packages("ggtext")
library(ggtext)
all_coverage_dfs %>%
    mutate( 
        district = if_else( 
            district %in% realised_mCCT, 
            paste0("**", district, "**"), 
            district
        ) 
    ) %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district, vaccine) %>%
    filter(year(cutoff_date) == 2021) %>%
    summarise(
        pct_times_mCCT = mean(rank <= 7)*100
    ) %>%
    ungroup() %>%
    mutate(district = factor(district), 
           district = fct_reorder(district, pct_times_mCCT)) %>%
    ggplot(aes( 
        y = district, 
        x = vaccine, 
        fill = pct_times_mCCT
    )) + 
    geom_tile(colour = "black") +
    scale_fill_viridis_c(option = "inferno")

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
