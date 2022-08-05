library(tidyverse)
library(data.table)
library(tidybayes)
    library(cmdstanr)




gen_data = function(seed, N, K, N_choices ) {
    set.seed(seed)
    N = 100
    K = 2
    N_choices = 400
    beta = matrix(rnorm(K), ncol = 1)

    # sigma_y = rgamma(1, shape = 2, scale = 2)
    # sigma_h = rgamma(1, shape = 2, scale = 2)
    sigma_y = 1
    sigma_h = 1
    rho =  rbeta(1, 3, 3)
    rho = abs(((-1)^rbernoulli(1, 0.5))*rho)


    Sigma = matrix(c(
      sigma_y, rho*sqrt(sigma_y)*sqrt(sigma_h),
      rho*sqrt(sigma_y)*sqrt(sigma_h), sigma_h
    ), ncol = 2, nrow = 2)
    errors = MASS::mvrnorm(n = N, mu = c(0, 0), Sigma = Sigma)

    X_all = matrix(rnorm(N*K*N_choices), ncol = K*N_choices, nrow = N) + errors[, 1]

    Y = 1 + X_all[, 1:2] %*% beta  + errors[, 2]
    df = data.table(
        X_all,
        Y
    )

    col_names = c(paste0("X_", 1:(K*N_choices)), "Y_1")
    colnames(df) = col_names
    return(list(
        df = df,
        params = beta
    ))
}


fake_data = gen_data(1)
df = fake_data$df
params = fake_data$params
df
lm(
    formula = Y_1 ~ X_1 + X_2,
    data = df
)


params


conventional_fit = function(df, params){

    fit = lm(
        formula = Y_1 ~ X_1 + X_2,
        data = df
    )

    tidy_fit = broom::tidy(fit)
    tidy_fit$true_values = params
    tidy_fit$model = "conventional"
    return(tidy_fit)
}

recentered_fit = function(df, params){
    X_cols_index = str_detect(colnames(df), "X_")
    df[, X_mu := rowMeans(.SD), .SDcols = colnames(df)[X_cols_index] ]
    fit = lm(
        Y_1 ~ X_1 + X_2 + X_mu,
        data = df
    )
    tidy_fit = broom::tidy(fit) %>%
        filter(term != "X_mu")
    tidy_fit$true_values = params
    tidy_fit$model = "recentered"
    return(tidy_fit)

}


weighted_fit = function(df, params){
    splits = split(1:20, f = ceiling(seq_along(1:20)/2))
    df[, paste0("X_", splits[[1]]), with = FALSE]




    tidy_all = map(splits,
        ~lm(
            formula = df$Y_1 ~ df[, paste0("X_", .x), with = FALSE] %>% as.matrix
        )) %>%
    map(broom::tidy) %>%
    map(mutate, term = c("(Intercept)", "X_1", "X_2")) %>%
    bind_rows()


    tidy_fit  = tidy_all %>%
        group_by(term) %>%
        summarise(estimate = mean(estimate))
    tidy_fit$true_values = params
    tidy_fit$model = "reweighted"
    return(tidy_fit)
}


weighted2_fit = function(df, params){
    splits = split(1:20, f = ceiling(seq_along(1:20)/2))

    tidy_all = map(splits,
        ~lm(
            formula = df$Y_1 ~ df[, paste0("X_", .x), with = FALSE] %>% as.matrix
        )) 
    
    X_mats = map(splits,
        ~df[, paste0("X_", .x), with = FALSE] %>% as.matrix) %>%
        map(~cbind(matrix(1, nrow(.x)), .x))

    simple_model = cmdstan_model("stan/bh-stan-model.stan")

    beta_hats = tidy_all %>%
        map(broom::tidy) %>%
        map(mutate, term = c("(Intercept)", "X_1", "X_2"))  %>%
        map("estimate")

    stan_data = list(
        K = 3,
        S = 10,
        beta_hat = beta_hats,
        N = nrow(df),
        X = X_mats
    )
    stan_fit = simple_model$sample(
        stan_data,
        chains = 1,
        cores = 1
    )
    tidy_fit = stan_fit %>%
        gather_draws(beta[j]) %>%
        median_qi() %>%
        to_broom_names() %>%
        mutate(term = c("(Intercept)", "X_1", "X_2"))

    tidy_fit$true_values = params
    tidy_fit$model = "reweighted2"
    return(tidy_fit)
}




sim_function = function(draw) {
    fake_data = gen_data(seed = draw)
    df = fake_data$df
    params = c(1, fake_data$params)
    conventional_res = conventional_fit(df, params)
    recentered_res = recentered_fit(df, params)
    weighted_res = weighted_fit(df, params)
    weighted2_res = weighted2_fit(df, params)

    res = bind_rows(
        conventional_res,
        recentered_res,
        weighted_res,
        weighted2_res
    )
    return(res)
}


sim_function(1)

library(furrr)


plan(multisession, workers = 8)


sim_fits = 1:100 %>%
    future_map_dfr(sim_function, .progress = TRUE, .options = furrr_options(seed = TRUE))


sim_fits %>%
    mutate(error = estimate - true_values) %>%
    ggplot(aes( 
        x = error,
        fill = term
    )) +
    geom_histogram() +
    facet_grid(model~term, scales = "free")

sim_fits %>%
    mutate(error = estimate - true_values) %>%
    group_by(term, model) %>%
    summarise( 
        mean_error = mean(error),
        median_error = median(error)
    )

