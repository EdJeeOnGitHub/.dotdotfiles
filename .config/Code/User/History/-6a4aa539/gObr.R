
library(tidyverse)


sim_files = c(
    "data/output/spillover-sim-results-202206200607.csv"
    )


sim_dfs = map(sim_files, read_csv)


sim_dfs %>%
    map(select, N, p_adjust_method, dgp) %>%
    map(unique)



sim_dfs[[1]] %>%
    group_by(
        N,
        J, 
        n_clusters, 
        rho, 
        spillover_pct,
        randomisation_level
    ) %>% 
    mutate(draw = 1:n()) %>%
    ggplot(aes( 
        x = draw, 
        y = TE,
        colour = randomisation_level
    )) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    facet_grid(spillover_pct ~ rho) +
    labs( 
        title = "Bandit MDE Samples", 
        x = "Iteration", 
        colour = "rho"
    )

generate_mde = function(data, warmup_draws = 1000, ...){
    summ_df = data %>%
        group_by(
            ...
        ) %>% 
        mutate(draw = 1:n(), signif = as.numeric(signif)) %>%
        filter(draw >= warmup_draws) %>%
        summarise( 
            mde = mean(TE), 
            sd = sd(TE), 
            conf.high = mde + 1.96*sd, 
            conf.low = mde - 1.96*sd, 
            p_adjust_method = unique(p_adjust_method),
            .groups = "drop"
        )
    return(summ_df)
}

summ_sim_dfs = map(
    sim_dfs,
    ~generate_mde(.x, warmup_draws = 1000, N, J, n_clusters, rho, randomisation_level, spillover_pct)) 
summ_sim_dfs

generate_mde(sim_dfs[[1]], warmup_draws = 400, N, J, n_clusters, rho, randomisation_level, spillover_pct) %>%
    arrange(spillover_pct, rho) %>%
    select(randomisation_level, rho, spillover_pct, mde:p_adjust_method)  %>%
    write_csv("data/output/spillover-mde-res.csv")

generate_mde(sim_dfs[[1]], warmup_draws = 400, N, J, n_clusters, rho, randomisation_level, spillover_pct) %>%
    arrange(spillover_pct, rho) %>%
    lm(
        mde ~ rho + spillover_pct + randomisation_level, 
        data = .
    ) %>%
    broom::tidy()

plot_mde_rho = function(summ_df){
    p = summ_df %>% 
            ggplot(aes( 
                x = N, 
                y = mde, 
                ymin = conf.low, 
                ymax = conf.high,
                colour = factor(rho)
            )) +
            geom_pointrange() +
            geom_line() +
            theme_bw() 
    return(p)
}






summ_sim_df = bind_rows(
    summ_sim_dfs
)
summ_second_sim_df = bind_rows(
    summ_second_sim_dfs
)

summ_sim_df %>%
    plot_mde_rho() +
    facet_wrap(~p_adjust_method) +
    labs( 
        colour = "rho",
        title = "MDE - Clustered OLS", 
        subtitle = "H1-H2-H4, comparing Bonferroni-Holm with no adjustments",
        caption = "Baseline uptake 10%."
    )
ggsave("data/output/plots/icc-ols-cluster-comp-mult-H1-H2-H4.png", width = 8, height = 6)



summ_second_sim_df %>%
    plot_mde_rho() +
    facet_wrap(~p_adjust_method) +
    labs( 
        colour = "rho",
        title = "MDE - Clustered OLS", 
        subtitle = "H3, comparing Bonferroni-Holm with no adjustments",
        caption = "Comparing T1 + T2 with T4, one third of sample in T4. Baseline uptake 10%."
    )
ggsave("data/output/plots/icc-ols-cluster-comp-mult-H3.png", width = 8, height = 6)

summ_second_sim_dfs[[1]]
