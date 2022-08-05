library(tidyverse)
library(furrr)


#' Generate "Data" for simulations
#' 
#' Here data really means hyperparameters such as number of observations, 
#' number of covariates,#' 
#' @param N Number of total observations
meta_gen_data_function = function(N = 1000, J = 3){
    function(seed) {
        set.seed(seed + 1e6)
        N = N
        J = J
        pr_arm = rep(1/J, J)
        treatment_arm = sample(1:J, N, pr_arm, replace = TRUE)
        return(list(
            N = N,
            J = J,
            treatment_arm = treatment_arm
        ))
    }
}



#' Generate parameters 
#' 
#' These are the parameters we want to estimate.
meta_gen_params = function(TEs, baseline_mean){
    function(seed,
             data){

        set.seed(seed + 2e6)

        return(list(
            TEs = TEs,
            baseline_mean = baseline_mean
        ))
    
    }

}



#' Generate Modelled Data
#'
#'
#' Using hyper parameters from `gen_data` and parameters from `gen_params`,
#' create our modelled data. That is, create binary outcome data according to the generative model. 
meta_gen_modeled_data_function = function(dgp = "logit"){
    function(seed,
             data,
             params) {
        set.seed(seed + 3e6)

    
        TEs = params$TEs
        treatment_arm = data$treatment_arm
        mu_vec = params$baseline_mean + TEs[treatment_arm]

        if (dgp == "probit") {
            p_vec = pnorm(mu_vec)
        } 
        if (dgp == "logit") {
            p_vec = exp(mu_vec)/(1 + exp(mu_vec))
        }

        y = rbinom(n = data$N, size = 1, prob = p_vec) 

        return(list(
            y = y
        ))
    }

}


simulate_data = function(seed = 1234,
                         hyper_params){ 

    gen_data = meta_gen_data_function(
        N = hyper_params$N,
        J = hyper_params$J
    )
    gen_modeled_data = meta_gen_modeled_data_function(dgp = hyper_params$dgp)
    gen_params = meta_gen_params(TEs = hyper_params$TEs, baseline_mean = hyper_params$baseline_mean)
    data = gen_data(seed)
    params = gen_params(seed,
                        data)
    modeled_data = gen_modeled_data(seed,
                                    data,
                                    params)
    sim_data_list = c(
        data,
        params,
        modeled_data
    )
    return(sim_data_list)
}


fit_estimators = function(sim_data) {
    sim_df = tibble(
        y = sim_data$y,
        treatment_arm = factor(sim_data$treatment_arm)
    )
    tidy_chisq = chisq.test(sim_df$y, sim_df$treatment_arm)  %>%
        broom::tidy()
    mean_df = sim_df %>%
        group_by(treatment_arm) %>%
        summarise(mean = mean(y))
    return(list(
        tidy_chisq = tidy_chisq,
        mean_df = mean_df
    ))
}



default_hyper_params = list(
                N = 2250,
                TEs = c(0, 2),
                J = 2,
                dgp = "logit",
                baseline_mean = 0)

default_hyper_params = function(...){
    hp = list(
        N = 2250,
        TEs = c(0, 2),
        J = 2,
        dgp = "logit", 
        baseline_mean = log(1/9)
    )
    new_vals = list(...)
    hp = modifyList(hp, new_vals)
    return(hp)
}

1:10 %>%
    map(
        ~simulate_data(
            seed = .x, 
            hyper_params = list(
                N = 100,
                TEs = c(0, 2),
                J = 2,
                dgp = "logit",
                baseline_mean = 0
            )) %>%
                fit_estimators()
       
    )


simulate_power = function(S, hyper_params) {
    sim_draws = map(
        1:S,
        ~simulate_data(seed = .x, hyper_params = hyper_params)
    )


    sim_fits = map(
        sim_draws,
        fit_estimators
        )
    chisq_fits = map_dfr(sim_fits, "tidy_chisq")
    mean_dfs = map_dfr(sim_fits, "mean_df")

    pr_signif = mean(chisq_fits$p.value < 0.05)

    treatment_means = mean_dfs %>%
        group_by(treatment_arm) %>%
        summarise(mean = mean(mean)) %>%
        select(mean) %>%
        pull()


    res = c(
        "pr_signif" = pr_signif,
        hyper_params[!str_detect(names(hyper_params), "TEs")],
        "TEs" = hyper_params$TEs,
        "treatment_means" = treatment_means
    ) 
    return(res)
}






TE_grid = seq(from = 0.0, to = 2.0, length.out = 50)

mde_grid = expand.grid(
    TE = TE_grid,
    N = c(400, 600, 800, 1000, 1200, 1400),
    baseline_mean = c(log(1/9), log(3/7), log(5/5), log(7/3))
) %>%
    as_tibble()
plan(multisession, workers = 8)

mde_sims = future_pmap_dfr(
    list(
        mde_grid$TE,
        mde_grid$N,
        mde_grid$baseline_mean
    ),
    ~simulate_power(
        150,
        hyper_params = default_hyper_params(
            N = ..2,
            TEs = c(0, ..1),
            baseline_mean = ..3 
        )
    ),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
)


mde_sims %>%
    group_by(N, baseline_mean) %>%
    filter(pr_signif >= 0.8) %>%
    filter(pr_signif == min(pr_signif)) %>%
    summarise(
        p_1 = exp(baseline_mean)/(1 + exp(baseline_mean)),
        diff = mean(treatment_means2 - treatment_means1)) %>%
    ggplot(aes( 
        x = N,
        y = diff,
        colour = factor(p_1)
    )) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(colour = "p_1", 
         y = "MDE(delta)",
         title = "Replicating Stata's Two Proportions Power")

ggsave("data/output/plots/rep-stata-two-prop.png", width = 8, height = 6)    