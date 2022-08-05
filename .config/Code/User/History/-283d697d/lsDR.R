library(tidyverse)
library(data.table)




gen_data = function(seed, N, K, N_choices ) {
    set.seed(seed)
    N = 10000
    K = 2
    N_choices = 10
    beta = matrix(rnorm(K), ncol = 1)

    sigma_y = rgamma(1, shape = 2, scale = 2)
    sigma_h = rgamma(1, shape = 2, scale = 2)
    rho =  rbeta(1, 3, 3)
    rho = ((-1)^rbernoulli(1, 0.5))*rho


    Sigma = matrix(c(
      sigma_y, rho*sqrt(sigma_y)*sqrt(sigma_h),
      rho*sqrt(sigma_y)*sqrt(sigma_h), sigma_h
    ), ncol = 2, nrow = 2)
    errors = MASS::mvrnorm(n = N, mu = c(0, 0), Sigma = Sigma)

    X_all = matrix(rnorm(N*K*N_choices), ncol = K*N_choices, nrow = N) + errors[, 1]

    Y = 1 + X_all[, 1:2] %*% beta + confounder[, 1:2] %*% matrix(rnorm(K, mean = 2), ncol = 1) + errors[, 2]
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


fake_data = gen_data()
df = fake_data$df
params = fake_data$params
df
lm(
    formula = Y_1 ~ X_1 + X_2,
    data = df
)


params


sim_function = function(draw) {
    fake_data = gen_data(seed = draw)
    df = fake_data$df
    params = c(1, fake_data$params)
    fit = lm(
        formula = Y_1 ~ X_1 + X_2,
        data = df
    )

    tidy_fit = broom::tidy(fit)
    tidy_fit$true_values = params
    return(tidy_fit)
}

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
    facet_wrap(~term)
