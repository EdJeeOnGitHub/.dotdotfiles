library(tidyverse)
library(cmdstanr)
library(ggstance)
library(lubridate)
options(mc.cores = 4)


all_df = read_csv("data/individual_data.csv", guess_max = 10000)

all_df

cleaned_studies = c("Dupas et al. 2021", "Luby et al. 2006", "Peletz et al. 2012")
subset_df = all_df %>%
    filter(!str_detect(study, "Dupas")) %>%
    filter(!str_detect(study, "Kremer"))


clean_df = subset_df %>%
    select(
        study, 
        event_time_lb,
        event_time_ub,
        follow_up_time, 
        treatment_time, 
        death, 
        wtreatment,
        age_month, 
        precision_evt
    ) %>%
    mutate(
        cens_event_time_lb = if_else(
            death == 0,
            follow_up_time,
            event_time_lb
        ),
        cens_event_time_ub = if_else(
          death == 0,
          follow_up_time,
          event_time_ub
        ),
        left_interval = interval(treatment_time, cens_event_time_lb) %/% days(1), 
        right_interval = interval(treatment_time, cens_event_time_ub) %/% days(1),
    ) %>%
    group_by(study) %>%
    mutate(site_id = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(death)) %>%
    mutate(censoring = if_else(death == 1, 0, 1)) %>%
    filter(!is.na(left_interval) & !is.na(right_interval)) 


# split by groups
df_list = clean_df %>%
    select(site_id, left_interval, wtreatment) %>%
    group_by(site_id) %>%
    group_split()
df_list

# map over separate dfs and fit linear model
fit_list = df_list %>%
    map(~lm(data = .x, left_interval ~ wtreatment))
fit_list
# map over list of fits and tidy them up.
# imap creates a variable .y that acts as a counter for each list object 
# _dfr says combine the final results into a dataframe by row    
tidy_fits = fit_list %>%
    imap_dfr(~broom::tidy(.x) %>% mutate(model = .y, type = "exp"))

clean_df %>%
    filter(
        left_interval < 0
    )
clean_df %>%
    skimr::skim()

exp_model = cmdstan_model("survival-stan/hierarchical-exponential.stan")

stan_data = list(
    interval_left = clean_df$left_interval,
    interval_right = clean_df$right_interval,
    N = nrow(clean_df),
    J = length(unique(clean_df$study)),
    nc = 2,
    nsc = 100,
    site = clean_df$site_id, 
    X = clean_df %>% mutate(const = 1) %>% select(const, wtreatment) %>% as.matrix(),
    censoring = clean_df$censoring,
    beta_mean = c(0, 0),
    beta_sigma = c(1, 1)
)

clean_df %>%
    select(wtreatment) %>%
    unique()

clean_df  %>%
    select(left_interval, right_interval) %>%
    filter(right_interval < left_interval)

clean_df %>%
    nrow()





weibull_model = cmdstan_model("survival-stan/hierarchical-weibull.stan")

weibull_fit = weibull_model$sample(
    stan_data, 
    chains = 1
)



library(tidybayes)


stan_draws = weibull_fit %>%
    gather_draws(beta[j, k])

site_name = clean_df %>%
    select(study, site_id) %>%
    unique()
stan_draws %>%
    median_qi() %>%
    filter(k == 2) %>%
    to_broom_names() %>%
    left_join(site_name, by = c("j" = "site_id")) %>%
    ggplot(aes( 
        x = estimate, 
        xmin = conf.low, 
        xmax = conf.high, 
        y = study,
        colour = factor(j)
    )) +
    geom_pointrangeh() +
    geom_vline(xintercept =  0, linetype = "longdash") +
    theme_bw()




stanfit <- rstan::read_stan_csv(weibull_fit$output_files())
shinystan::launch_shinystan(stanfit)
weibull_fit
