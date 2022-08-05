library(tidyverse)

clt_check = function(N, delta, ...) {
    mu = 1
    sd = sqrt(0.5)
    X = rnorm(N, mean = mu, sd = sd)
    X_bar = (1/(N + N^delta)) * sum(X)
    clt = sqrt(N)*(X_bar - mu)/var(X)
    return(clt)
}
sim_func =function(delta, N, S){map_dfr(1:S, ~clt_check(N = N, delta = delta) %>% as_tibble() %>% mutate(N = N, delta = delta))}

library(furrr)
plan(multicore, workers = 8)
sims = seq(from = -2, to = 2, length.out = 50) %>%
    future_map_dfr(
        ~sim_func(delta = .x, N = 500, S = 100),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
        )


sims %>%
    ggplot(aes(x = value, colour = factor(delta) )) +
    geom_density() +
    guides(colour = "none") +
    theme_bw()



sims %>%
    group_by(delta) %>%
    summarise(mean = mean(value)) %>%
    ggplot(aes(x = delta, y = mean)) +
    geom_point() 

sims %>%
    group_by(delta) %>%
    summarise(var = var(value)) %>%
    ggplot(aes(x = delta, y = var)) +
    geom_point() +
    theme_bw() +
    geom_hline(yintercept = 1)

sim_func =function(delta, N, S){map_dfr(1:S, ~clt_check(N = N, delta = delta) %>% as_tibble() %>% mutate(N = N, delta = delta))}



d_func = function(n, sigma) {
    d = 1 + log((1-sigma)/sigma)/log(n)
    return(d)
}
ed = sim_func(N = 100, S = 100, delta = d_func(100, sqrt(0.5)))


ed %>%
    group_by(delta) %>%
    summarise(var = var(value)) %>%
    ggplot(aes(x = delta, y = var)) +
    geom_point() +
    theme_bw() +
    geom_hline(yintercept = 1)
