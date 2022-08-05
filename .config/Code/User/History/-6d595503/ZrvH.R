source("code/ts-functions.R")


set.seed(3)
N_arms = 4
J_outcomes = 3

util_weights_raw = rnorm(J_outcomes)
beta_raw = matrix(rnorm(N_arms*J_outcomes), nrow = N_arms, ncol = J_outcomes)

mu_bar_raw = beta_raw %*% util_weights_raw 
mu_norm = norm(mu_bar_raw)
beta = beta_raw/sqrt(mu_norm)
util_weights = util_weights_raw/sqrt(mu_norm)


mu_bar = beta %*% util_weights


true_weight_df = create_weight_df(util_weights)

N_rounds = 10
N_per_round = 10
print(paste0("N_total: ", N_per_round*N_rounds))



estimated_ts_sim = TS_simulation(
    N_rounds = N_rounds,
    beta = beta,
    util_weights = util_weights,
    K = N_arms,
    N = N_per_round,
    J = J_outcomes
)


estimated_ts_sim

estimated_long_res = estimated_ts_sim$result_dt %>%
    pivot_longer(
        cols = !c("round", "time_period", "weight_type")
    ) 

estimated_long_res

estimated_long_res %>%
    filter(str_detect(name, "arm|J")) %>%
    mutate(name = str_replace(name, "J", "Y")) %>%
    mutate(type = str_extract(name, ".*(?=_)"), 
            number = str_extract(name, "\\d+") %>% as.numeric()) %>%
    ggplot(aes(
        x = round, 
        y = value, 
        colour = name 
    )) +
    facet_wrap(~type, ncol = 1, scales = "free") +
    geom_line() +
    geom_point() +
    geom_hline(
        data = true_weight_df %>% mutate(type = "Y", name = str_replace(name, "J", "Y")),
        aes(
            yintercept = rel_weight, 
            colour = name
        ),
        linetype = "longdash") +
    theme_bw() +
    scale_x_continuous(breaks = 1:20) +
    theme(panel.grid.minor = element_blank()) +
    labs(
        title = "Joint Estimation of Optimal Treatment and MRS", 
        x = "Round", 
        y = "Proportion/MRS", 
        colour = "Arm/Outcome"
    )
er_true
util_weights/util_weights[1]

thompson_sim$posterior_means %>%
    filter(term == "beta") %>%
    ggplot(aes(
        x = round, 
        ymin = conf.low, 
        ymax = conf.high,
        y = estimate, 
        colour = factor(j) 
    )) +
    geom_pointrange() +
    geom_line() +
    facet_grid(k~j, scales = "free") +
    geom_hline(
        data = true_beta_df,
        aes(yintercept = value),
        linetype = "longdash"
    )

# thompson_sim$posterior_means %>%
#     filter(term == "mrs") %>%
#     ggplot(aes(
#         x = round, 
#         ymin = conf.low, 
#         ymax = conf.high,
#         y = estimate,
#         colour = factor(j)
#     )) +
#     geom_pointrange() +
#     geom_line() +
#     geom_hline(
#         data = true_weight_df %>% mutate(j = 1:n()),
#         aes(
#             yintercept = rel_weight
#         ),
#         linetype = "longdash")  +
#         facet_wrap(~j, ncol = 1, scales = "free") +
#         theme_bw()

