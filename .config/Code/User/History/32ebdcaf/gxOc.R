library(tidyverse)
library(kableExtra)


## Beta Setup

set.seed(3)
N_arms = 4
J_outcomes = 3


N_sim = 100


sim_util_weights = map(1:N_sim, ~rnorm(J_outcomes))
sim_beta = map(1:N_sim, ~matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes))


sim_param_files = fs::dir_ls("data/output/simulations/", regexp = "params")
sim_result_files = fs::dir_ls("data/output/simulations/", regexp = "results")



sim_param_df = map_dfr(sim_param_files, ~read_csv(.x) %>% mutate(file_name = .x))
sim_result_df = map_dfr(sim_result_files, ~read_csv(.x) %>% mutate(file_name = .x))

sim_result_df = sim_result_df %>%
    mutate(datetime_run = str_extract(file_name, "(?<=sim-results-)\\d+") %>% lubridate::ymd_hm()) %>%
    filter(
        datetime_run != lubridate::ymd_hms("2022-05-02 14:17:00")
    )

sim_param_df = sim_param_df %>%
    mutate(datetime_run = str_extract(file_name, "(?<=sim-params-)\\d+") %>% lubridate::ymd_hm()) %>%
    filter(
        datetime_run != lubridate::ymd_hms("2022-05-02 14:17:00")
    )
sim_param_df


clean_sim_result_df = left_join(
    sim_result_df,
    sim_param_df %>% select(-file_name),
    by = "datetime_run"
) %>%
    # one draw randomly repeated twice so delete it
    filter(
        !(datetime_run == lubridate::ymd_hms("2022-05-02 22:59:00") & draw == 35 & estimated_optimal_arm == 1)
    )  %>%
    mutate(
        util_weight_sd = replace_na(util_weight_sd, 0)
    )



clean_sim_result_df %>%
    group_by(datetime_run) %>%
    summarise(n = n())
clean_sim_result_df %>%
    group_by(
        tau,
        tau0,
        weight_type, 
        util_weight_sd,
        choice_type
    ) %>%
    summarise( 
        pr_optimal_arm = mean(true_optimal_arm == estimated_optimal_arm)
    )



# clean_sim_result_df %>%
#     filter(draw == 1)


# equal_files = fs::dir_ls("data/output/simulations/", regexp = "equal")
# first_files = fs::dir_ls("data/output/simulations/", regexp = "first")
# estimated_files = fs::dir_ls("data/output/simulations/", regexp = "estimated")
# ra_files = fs::dir_ls("data/output/simulations/", regexp = "random_assignment")

# equal_df = map_dfr(equal_files, read_csv) %>%
#     mutate(weight_type = "equal")
# first_df = map_dfr(first_files, read_csv) %>%
#     mutate(weight_type = "first")
# estimated_df = map_dfr(estimated_files, read_csv) %>%
#     mutate(weight_type = "estimated")
# random_df = map_dfr(ra_files, read_csv) %>%
#     mutate(weight_type = "random_assignment")


# sim_df = bind_rows(
#     equal_df,
#     first_df,
#     estimated_df,
#     random_df
# ) %>%
#     group_by(draw) %>%
#     mutate(
#         rank = (N_arms + 1) - rank(mean_welfare)
#     ) %>%
#     ungroup() %>%
#     mutate(
#         weight_type = factor(weight_type, levels = c(
#             "estimated",
#             "random_assignment",
#             "equal",
#             "first"
#         ))
#     )

# summ_df = sim_df %>%
#     group_by(trial_type = weight_type) %>%
#     summarise( 
#         pr_optimal_arm = mean(true_optimal_arm == estimated_optimal_arm),
#         mean_welfare = mean(mean_welfare)
#     ) 

# summ_df %>%
#     mutate(across(
#         where(is.numeric), 
#         round, 3
#     )) %>%
#     knitr::kable(
#         format = "latex", 
#         booktabs = TRUE
#     ) %>%
#     kable_styling() %>%
#     save_kable("data/output/tables/sim-summ.tex")
    



# sim_df %>%
#     group_by(weight_type, rank) %>%
#     mutate(rank_count = n()) %>%
#     ungroup() %>%
#     ggplot(aes(
#         x = rank, 
#         fill = weight_type
#     )) +
#     geom_bar(
#         position = position_dodge(0.5), 
#         colour = "white"
#     ) +
#     theme_bw() +
#     theme(legend.position = "bottom") +
#     labs(y = "Percent",
#          fill = "Trial Type", 
#          x = "Welfare Rank",
#          title = "Trial Welfare Rankings") +
#      geom_text(
#          data = 
#             sim_df %>%
#                 group_by(weight_type, rank) %>%
#                 summarise(rank_count = n()) %>%
#                 ungroup() %>%
#                 filter(rank == 1),
#          aes(label=rank_count, y = rank_count), 
#          position=position_dodge(width=0.5),
#         vjust=-0.25,
#         size = 6)

# ggsave("data/output/plots/trial-welfare-ranks.png", 
#        width = 8,
#        heigh = 6)

