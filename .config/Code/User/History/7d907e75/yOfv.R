library(tidyverse)
library(lubridate)


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



potential_cutoffs = seq(
    from = ymd("2020-01-01"), 
    to = ymd("2021-09-01"), 
    by = "months"
)


calculate_coverage(clean_df, 2020, "2021-09-01")




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

all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district) %>%
    filter(year(cutoff_date) == 2021) %>%
    summarise(
        pct_times_mCCT = mean(rank <= 7),
    ) %>%
    arrange(pct_times_in) %>%
    ggplot(aes( 
        x = pct_times_in, 
        y = sd_times_in
    )) +
    geom_point()

all_coverage_dfs %>%
    group_by(cutoff_date, vaccine) %>%
    mutate(rank = rank(coverage))  %>%
    ungroup() %>%
    group_by(district) %>%
    summarise(
        pct_times_in = mean(rank <= 7),
        sd_times_in = sd(rank <= 7)
    ) %>%
    mutate(type = "subset") %>%
    arrange(pct_times_in) %>%
    ggplot(aes( 
        x = pct_times_in, 
        y = sd_times_in
    )) +
    geom_point()
all_coverage_dfs %>%
    ungroup() %>%
    select(district) %>%
    unique()

}
clean_df %>%
    colnames()
