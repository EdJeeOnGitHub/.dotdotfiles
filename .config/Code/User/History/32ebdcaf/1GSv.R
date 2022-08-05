library(tidyverse)
library(kableExtra)


## Beta Setup

set.seed(3)
N_arms = 4
J_outcomes = 3


N_sim = 100


sim_util_weights = map(1:N_sim, ~rnorm(J_outcomes))
sim_beta = map(1:N_sim, ~matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes))

# ra_welfare = map2_dbl(sim_beta, sim_util_weights, ~matrix(1/N_arms, ncol = N_arms) %*% .x %*% .y)
# ra_welfare_df = enframe(
#     ra_welfare,
#     name = "draw", 
#     value = "mean_welfare"
# ) %>%
#     mutate(weight_type = "random_assignment")
# ra_welfare_df


equal_files = fs::dir_ls("data/output/simulations/", regexp = "equal")
first_files = fs::dir_ls("data/output/simulations/", regexp = "first")
estimated_files = fs::dir_ls("data/output/simulations/", regexp = "estimated")
ra_files = fs::dir_ls("data/output/simulations/", regexp = "random_assignment")

equal_df = map_dfr(equal_files, read_csv) %>%
    mutate(weight_type = "equal")
first_df = map_dfr(first_files, read_csv) %>%
    mutate(weight_type = "first")
estimated_df = map_dfr(estimated_files, read_csv) %>%
    mutate(weight_type = "estimated")
random_df = map_dfr(ra_files, read_csv) %>%
    mutate(weight_type = "random_assignment")


sim_df = bind_rows(
    equal_df,
    first_df,
    estimated_df,
    random_df
) %>%
    group_by(draw) %>%
    mutate(
        rank = (N_arms + 1) - rank(mean_welfare)
    ) %>%
    ungroup() %>%
    mutate(
        weight_type = factor(weight_type, levels = c(
            "estimated",
            "random_assignment",
            "equal",
            "first"
        ))
    )

summ_df = sim_df %>%
    group_by(trial_type = weight_type) %>%
    summarise( 
        pr_optimal_arm = mean(true_optimal_arm == estimated_optimal_arm),
        mean_welfare = mean(mean_welfare)
    ) 

summ_df %>%
    mutate(across(
        where(is.numeric), 
        round, 3
    )) %>%
    knitr::kable(
        format = "latex", 
        booktabs = TRUE
    ) %>%
    kable_styling() %>%
    save_kable("data/output/tables/sim-summ.tex")
    



sim_df %>%
    group_by(weight_type, rank) %>%
    mutate(rank_count = n()) %>%
    ungroup() %>%
    ggplot(aes(
        x = rank, 
        fill = weight_type
    )) +
    geom_bar(
        position = position_dodge(0.5), 
        colour = "white"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Percent",
         fill = "Trial Type", 
         x = "Welfare Rank",
         title = "Trial Welfare Rankings") +
     geom_text(
         data = 
            sim_df %>%
                group_by(weight_type, rank) %>%
                summarise(rank_count = n()) %>%
                ungroup() %>%
                filter(rank == 1),
         aes(label=rank_count, y = rank_count), 
         position=position_dodge(width=0.5),
        vjust=-0.25,
        size = 6)

ggsave("data/output/plots/trial-welfare-ranks.png", 
       width = 8,
       heigh = 6)
library(kableExtra)
