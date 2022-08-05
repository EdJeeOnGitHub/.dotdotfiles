library(tidyverse)


sim_df = read_csv("data/output/sim-data-initial.csv")
second_sim_df = read_csv("data/output/second-sim-data-initial.csv")




sim_df %>%
    group_by(
        N,
        J, 
        n_clusters, 
        rho
    ) %>% 
    mutate(draw = 1:n()) %>%
    ggplot(aes( 
        x = draw, 
        y = TE,
        colour = factor(rho)
    )) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    facet_wrap(~N) +
    labs( 
        title = "Bandit MDE Samples", 
        x = "Iteration", 
        colour = "rho"
    )
ggsave(
    "data/output/plots/bandit-mde-draws-example.png",
    width = 8,
    height = 6
)

summ_sim_df = sim_df %>%
    group_by(
        N,
        J, 
        n_clusters, 
        rho
    ) %>% 
    mutate(draw = 1:n(), signif = as.numeric(signif)) %>%
    filter(draw > 1000) %>%
    summarise( 
        mde = mean(TE), 
        sd = sd(TE), 
        conf.high = mde + 1.96*sd, 
        conf.low = mde - 1.96*sd, 
        .groups = "drop"
    )
sim_df
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
            .groups = "drop"
        )
    return(summ_df)
}

summ_sim_df = sim_df %>%
    generate_mde(warmup_draws = 1000, N, J, n_clusters, rho)

summ_second_sim_df = second_sim_df %>%
    generate_mde(warmup_draws = 1000, N, J, n_clusters, rho)

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

plot_mde_rho(summ_sim_df) +
            labs( 
                colour = "rho",
                title = "MDE - Clustered OLS", 
                subtitle = "H1 - H2 - H4",
                caption = "Baseline Uptake 10%"
            )
ggsave("data/output/plots/icc-ols-cluster.png", width = 8, height = 6)


plot_mde_rho(summ_second_sim_df) +
            labs( 
                colour = "rho",
                title = "MDE - Clustered OLS", 
                subtitle = "H4",
                caption = "Comparing T1 + T2 with T4, one third of sample in T4. Baseline uptake 10%"
            )


summ_sim_df %>%

summ_sim_df %>%
    ggplot(aes( 
        x = N, 
        y = mde, 
        colour = factor(rho)
    )) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs( 
        colour = "rho",
        title = "MDE - Clustered OLS"
    )

