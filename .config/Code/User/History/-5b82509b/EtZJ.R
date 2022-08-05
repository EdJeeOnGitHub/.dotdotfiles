
library(testthat)
library(rstan)
library(tidyverse)
library(broom)
library(tidybayes)
library(furrr)
options(mc.cores = 4)
source("code/sbc-functions.R")
rstan_options(auto_write = TRUE)


default_stan_model = stan_model("stan/rubin-selection-mvn.stan")
heckit_model = stan_model("stan/rw-heckit.stan")

default_data_args = list(N = 150, p = 2, q = 2, d= 1, cutoff_quantile = 0.2)
default_params_args = list(mu_beta = 1, sd_beta = 1 )
default_modelled_data_args = list(
  selection_type = "quantile", 
  rubin = TRUE, 
  selection_d = TRUE,
  recentre = FALSE)

first_draw = simulate_draw(
  seed = 2,
  stan_model = default_stan_model,
  gen_data_args = default_data_args,
  gen_params_args = default_params_args,
  gen_modelled_data_args = default_modelled_data_args,
  fit_model = TRUE,
  fit_heckit = FALSE
)


first_sbc = create_sbc_output(first_draw$params, first_draw$bayes_fit, first_draw$data$N)
first_tidy = first_draw %>%
    tidy_sim_output()
test_that("Model works", {
  expect_s4_class(first_draw$bayes_fit, "stanfit")
  expect_s3_class(first_sbc, "tbl_df")
  expect_s3_class(first_tidy, "tbl_df")
})

first_tidy %>%
  mutate(error = true_value - estimate) %>%
  arrange(term, j) 




plan(multisession, workers = 8)

simulated_draws = 1:100 %>%
  future_map(
    ~{sim_draw = simulate_draw(
      seed = .x,
      stan_model = default_stan_model,
      gen_data_args = default_data_args,
      gen_params_args = default_params_args,
      gen_modelled_data_args = default_modelled_data_args,
      fit_heckit = TRUE
      )
      
      
      if (is.null(sim_draw$bayes_fit)) {
        return(list(sbc = NULL, tidy_df = NULL, N_observed = NULL))
      } 

      sbc_draw_bayes = create_sbc_output(sim_draw$params, sim_draw$bayes_fit, sim_draw$data$N) %>%
        mutate(draw = .x, model = "bayes") 
      sbc_draw_heckit = create_sbc_output(sim_draw$params, sim_draw$heckit_bayes_fit, sim_draw$data$N) %>%
        mutate(draw = .x, model = "heckit") 
      sbc_draw = bind_rows(
        sbc_draw_bayes,
        sbc_draw_heckit
      )
      N_observed = nobs(sim_draw$ols_fit)
       tidy_output = sim_draw %>%
        tidy_sim_output() %>%
        mutate(draw = .x)
        return(list(sbc = sbc_draw, 
                    tidy_df = tidy_output,
                    N_observed = N_observed))
        },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE, packages = "broom")
  )



tidy_sim_draws = map_dfr(simulated_draws, "tidy_df")
sbc_draws = map_dfr(simulated_draws, "sbc")
calibration_draws = map_dfr(simulated_draws, "calibration_res")
#### Plots #####
sbc_draws
tidy_sim_draws

tidy_sim_draws %>%
  mutate(error = true_value - estimate) %>%
  mutate(coverage = conf.low < true_value & true_value < conf.high) %>% 
  group_by(term, j, model) %>%
  summarise( 
    mean_error = mean(error),
    mae = mean(abs(error)), 
    mse = mean(error^2),
    med_ae = median(abs(error)),
    rp = mean(coverage, na.rm = TRUE) 
  ) 

unif_test = sbc_draws %>%
  filter(model == "bayes") %>%
  group_by(term, j, k) %>%
  summarise(ks_p = tidy(ks.test(rank_stat, "punif", 0, 1))$p.value, .groups = "drop") %>%
  mutate(ks_p_adj = p.adjust(ks_p, method = "BH")) 
unif_test


test_that("Quantile Selection MVN Model Recovers Parameters", {

  expect_equal(
    unif_test %>%
      filter(ks_p_adj < 0.05) %>%
      nrow(),
      0
  )
})

first_draw$bayes_fit$stan_data



sbc_draws %>%
  group_by(term, j, model) %>%
  summarise(
    cal_50 = mean(0.25 < rank_stat & rank_stat < 0.75),
    cal_90 =  mean(0.05 < rank_stat & rank_stat < 0.95))




sbc_draws %>%
  filter(model == "bayes") %>%
  ggplot(aes(x = rank_stat, fill = term)) +
  geom_histogram(bins = 20) +
  facet_wrap(term ~ j, scales = "free")

sbc_draws %>%
  filter(model == "bayes") %>%
    ggplot(aes(sample = rank_stat,
           colour = term)) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_wrap(term~j,
             scales = "free") +
  theme_bw() +
  guides(colour = "none")  +
  labs(
      x = "Theoretical Quantile", 
      y = "Realised Quantile",
      title = "Simulation Based Calibration - Rubin Selection"
  )
ggsave("rubin-sbc.png", width = 8, height = 6)

tidy_sim_draws %>%
  mutate(coverage = conf.low < true_value & true_value < conf.high) %>% 
  filter(term == "beta") %>%
  group_by(term, j, model) %>%
  summarise(
    mean_err = mean(true_value - estimate), 
    med_err = median(true_value - estimate),
    rp = mean(coverage) 
    )



calculate_freq_coverage = function(ols_draws, interval){
  alpha = (1 - interval)/2
  rp_df = ols_draws %>%
    mutate(
      conf.low = estimate - std.error*qnorm(1 - alpha), 
      conf.high = estimate + std.error*qnorm(1 - alpha), 
      coverage = true_value > conf.low & true_value < conf.high
    ) %>%
    group_by(term, j, k, model) %>%
    summarise(coverage = mean(coverage), interval = interval, .groups = "drop") 
    return(rp_df)
}
calculate_bayes_coverage = function(bayes_draws, interval) {
  alpha = (1 - interval)/2
  c_low = alpha 
  c_hi = 1 - alpha
  bayes_draws %>%
    group_by(term, j, model) %>%
    summarise(
      coverage = mean(c_low < rank_stat & rank_stat < c_hi), interval = interval,
              .groups = "drop")
}
ols_draws = tidy_sim_draws %>%
    filter(str_detect(model, "ols"))

ols_coverage = map_dfr(
  seq(from = 0.01, to = 0.99, length.out = 100),
  ~calculate_freq_coverage(ols_draws = ols_draws, interval = .x)
)
ols_coverage

bayes_coverage = map_dfr(
  seq(from = 0.01, to = 0.99, length.out = 100),
  ~calculate_bayes_coverage(sbc_draws, .x)
)


comp_coverage = bind_rows(
  ols_coverage,
  bayes_coverage
)

comp_coverage %>%
  mutate(coverage = interval - coverage) %>%
filter(term == "beta") %>%
  ggplot(aes( 
    x = interval, 
    y = coverage, 
    colour = model
  )) +
  geom_line(size = 2) +
  # geom_abline(linetype = "longdash") +
  geom_hline(yintercept = 0) +
  facet_wrap(~j, ncol = 1 ) +
  theme_bw()

comp_coverage %>%
  filter(term == "beta") %>%
  ggplot(aes( 
    x = interval, 
    y = coverage, 
    colour = model
  )) +
  geom_line(size = 2) +
  geom_abline(linetype = "longdash") +
  facet_wrap(~j, ncol = 1 ) +
  theme_bw() +
  labs(
    title = "Rubin Selection - Beta Coverage Across Models"
  )

ggsave(
  "data/output/plots/rubin-selection-all-models-beta-coverage.png",
  width = 8,
  height = 6
)


comp_coverage %>%
  mutate(diff_coverage = interval - coverage) %>%
filter(term == "beta") %>%
  ggplot(aes( 
    x = interval, 
    y = diff_coverage, 
    colour = model
  )) +
  geom_line(size = 2) +
  # geom_abline(linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  facet_wrap(~j, ncol = 1, scales = "free" ) +
  theme_bw() +
  labs(
    title = "Rubin Selection - Beta Coverage Difference"
  )
ggsave(
  "data/output/plots/rubin-selection-difference.png",
  width = 8,
  height = 6
)


comp_coverage %>%
  ggplot(aes( 
    x = interval, 
    y = coverage, 
    colour = model
  )) +
  geom_line(size = 2) +
  geom_abline(linetype = "longdash") +
  facet_wrap(term~j ) +
  theme_bw() +
  labs(
    title = "Rubin Selection - Coverage Across Models"
  )
ggsave(
  "data/output/plots/rubin-selection-all-models-coverage.png",
  width = 8,
  height = 6
)
comp_coverage %>%
  ggplot(aes( 
    x = interval, 
    y = coverage, 
    colour = model
  )) +
  geom_point() +
  geom_abline(linetype = "longdash") +
  facet_wrap(term ~j ) +
  theme_bw()
calculate_freq_rp(ols_draws = ols_draws, interval = 0.9)

int = 0.90
qnorm(0.975)


tidy_sim_draws %>%
  filter(term %in% c("rho", "sigma_h", "sigma_y")) %>%
  ggplot(aes( 
    x = true_value, 
    y = estimate,
    colour = model
  )) + 
  geom_point() + 
  facet_grid(~term, scales = "free") +
  geom_abline()

tidy_sim_draws %>%
  ggplot(aes( 
    x = true_value, 
    y = estimate,
    colour = model
  )) + 
  geom_point() + 
  facet_grid(term~j, scales = "free") +
  geom_abline()


tidy_sim_draws %>%
  filter(term == "gamma") %>%
  ggplot(aes( 
    x = true_value, 
    y = estimate,
    colour = model
  )) + 
  geom_point() + 
  facet_wrap(~j, scales = "free", ncol = 1) +
  geom_abline()

tidy_sim_draws %>%
  filter(term == "beta", j == 1) %>%
  filter(model %in% c("bayes", "freq_ols")) %>%
  ggplot(aes( 
    x = true_value, 
    y = estimate,
    colour = model
  )) + 
  geom_point() + 
  facet_wrap(~j, scales = "free", ncol = 1) +
  geom_abline(
    linetype = "longdash"
  ) +
  theme_bw()

tidy_sim_draws %>%
  filter(term == "beta") %>%
  ggplot(aes( 
    x = true_value, 
    y = estimate,
    colour = model
  )) + 
  geom_point() + 
  facet_wrap(~j, scales = "free", ncol = 1) +
  geom_abline(
    linetype = "longdash"
  ) +
  theme_bw()



tidy_sim_draws %>%
  filter(term == "beta") %>%
  filter(j == 1) %>%
  ggplot(aes( 
    x = estimate - true_value, 
    fill = model
  )) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Bias for Beta_1"
  ) +
  theme_bw()
ggsave(
  "data/output/plots/bias-dist-rubin.png",
  width = 8,
  height = 6
)

tidy_sim_draws %>%
  filter(term == "beta") %>%
  filter(j == 1) %>%
  ggplot(aes( 
    x = abs(estimate - true_value), 
    fill = model
  )) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Absolute Bias for Beta_1"
  ) +
  theme_bw()
ggsave(
  "data/output/plots/abs-bias-dist-rubin.png",
  width = 8,
  height = 6
)

tidy_sim_draws %>%
  filter(term == "beta") %>%
  filter(j == 2) %>%
  ggplot(aes( 
    x = true_value - estimate, 
    fill = model
  )) +
  geom_vline(
    data = 
      tidy_sim_draws %>%
        filter(term == "beta") %>%
        filter(j == 2) %>%
        group_by(model) %>%
        summarise(med_error = median(true_value - estimate)),
        aes(xintercept = med_error)
  ) +
  geom_histogram(bins = 60, alpha = 0.2) +
  facet_wrap(~model)


### 

fake_data = simulate_draw(
  seed = 1,
  stan_model = default_stan_model,
  gen_data_args = default_data_args %>% list_modify(N = 150, cutoff_quantile = 0.5),
  gen_params_args = default_params_args %>% list_modify(mu_beta = 0, sd_beta = 0.5),
  gen_modelled_data_args = default_modelled_data_args,
  fit_model = FALSE,
  fit_heckit = FALSE
)





fake_df = bind_cols(
as_tibble(fake_data$modelled_data$X),
as_tibble( fake_data$modelled_data$y),
as_tibble(fake_data$modelled_data$h),
as_tibble(fake_data$modelled_data$D)
)

colnames(fake_df) = c("const", "x_1", "y_1", "h", "d")
fake_df %>%
  mutate(size = d*2 + 1) %>%
  mutate(d = as.logical(d)) %>%
  ggplot(aes( 
    x = x_1, 
    y = y_1,
    colour = factor(d),
    size = size,
    group = d
  )) +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  guides(size = "none") +
  labs( 
    colour = "Observed",
    title = "Selection Problem"
  )  +
  scale_colour_manual(values = c("#ff880081", "#2b6fd6"))
ggsave(
  "data/output/plots/selection-example.png",
  width = 8,
  height = 6
)

full_ols = fake_df %>%
  lm(
    y_1 ~ x_1, data = .
  ) %>%
  augment()

obs_ols =  fake_df %>%
  filter(d == 1) %>%
  lm(
    y_1 ~ x_1, data = .
  ) %>%
  augment()

fake_df %>%
  mutate(size = d*2 + 1) %>%
  mutate(d = as.logical(d)) %>%
  ggplot(aes( 
    x = x_1, 
    y = y_1,
    colour = factor(d),
    size = size,
    group = d
  )) +
  geom_point() +
  geom_line(
    inherit.aes = FALSE, 
    aes(y = .fitted, x = x_1), 
    data = obs_ols, 
  colour = "#2b6fd6",
    size = 2,
    ) +
  geom_line(
    inherit.aes = FALSE, 
    aes(y = .fitted, x = x_1), 
    data = full_ols, 
    size = 2
    ) +
  theme_bw() +
  guides(size = "none") +
  labs( 
    colour = "Observed",
    title = "Selection Problem"
  ) +
  scale_colour_manual(values = c("#ff880081", "#2b6fd6"))

ggsave(
  "data/output/plots/selection-ols-example.png",
  width = 8,
  height = 6
)

example_fit = sample_from_model(
  seed = 2,
  data = fake_data$data,
  params = fake_data$params,
  modelled_data = fake_data$modelled_data,
  stan_model = default_stan_model
)

beta_bayes_example = example_fit %>%
  gather_draws(beta[j]) %>%
  median_qi() %>%
  to_broom_names()  %>%
  mutate(term = paste0(term, "_", j )) %>%
  select(term, estimate) %>%
  spread(term, estimate)


bayes_example = fake_df %>%
  bind_cols(beta_bayes_example) %>%
  mutate(pred = beta_1 + beta_2*x_1)

fake_df %>%
  mutate(size = d*2 + 1) %>%
  mutate(d = as.logical(d)) %>%
  ggplot(aes( 
    x = x_1, 
    y = y_1,
    colour = factor(d),
    size = size,
    group = d
  )) +
  geom_point() +
  geom_line(
    inherit.aes = FALSE, 
    aes(y = .fitted, x = x_1), 
    data = obs_ols, 
  colour = "#2b6fd6",
    size = 2,
    ) +
  geom_line(
    inherit.aes = FALSE, 
    aes(y = pred, x = x_1), 
    data = bayes_example, 
    size = 2,
    colour = "hotpink"
    ) +
  geom_line(
    inherit.aes = FALSE, 
    aes(y = .fitted, x = x_1), 
    data = full_ols, 
    size = 2
    ) +
  theme_bw() +
  guides(size = "none") +
  labs( 
    colour = "Observed",
    title = "Selection Problem + Correction"
  ) +
  scale_colour_manual(values = c("#ff880081", "#2b6fd6"))
ggsave(
  "data/output/plots/selection-correction-ols-example.png",
  width = 8,
  height = 6
)


tidy_sim_draws %>%
  filter(term == "beta") %>%
  filter(j == 2) %>%
  ggplot(aes( 
    x = true_value - estimate, 
    fill = model
  )) +
  geom_density(
    alpha = 0.3
  )


N <- 512
beta <- c(0.5, -0.5)
gamma <- c(0.5, -2.5)
sigma_e <- 1
rho <- 0.3

## simulate correlated covariates and noise
set.seed(3)
X <- cbind(rep(1, N), rnorm(N))
Z <- X
Sigma <- matrix(c(sigma_e^2, rho * sigma_e, rho * sigma_e, 1), ncol = 2)
e <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = Sigma)

## generate responses
D <- 1L * c(Z %*% gamma + e[, 2] > 0)
y <- (X %*% beta + e[, 1])


library(sampleSelection)
X_subset = X[D > 0, 2]
Z_2 = Z[, 2]
length(D)
length(Z_2)
length(y)
length(X_subset)
D_s  
Z_2
y
X_subset
df = tibble(
  y = y[, 1],
  D = D,
  X = X[, 2],
  Z = Z[, 2]
)
df
selection(
  selection = D ~ Z,
  outcome =  y ~ X,
  data = df 
) %>% summary()
sampleSelection()
lm(y[D>0,1] ~ 0 + X[D > 0, ])




library( "mvtnorm" )
nObs <- 1000
sigma <- matrix( c( 1, -0.7, -0.7, 1 ), ncol = 2 )
errorTerms <- rmvnorm( nObs, c( 0, 0 ), sigma )
myData <- data.frame( no = c( 1:nObs ), x1 = rnorm( nObs ), x2 = rnorm( nObs ),
   u1 = errorTerms[ , 1 ], u2 =  errorTerms[ , 2 ] )
myData$y <- 2 + myData$x1 + myData$u1
myData$s <- ( 2 * myData$x1 + myData$x2 + myData$u2 - 0.2 ) > 0
myData$y[ !myData$s ] <- NA
myOls <- lm( y ~ x1, data = myData)
summary( myOls )
myHeckit <- heckit( s ~ x1 + x2, y ~ x1, myData, print.level = 1 )
summary( myHeckit )

myData



