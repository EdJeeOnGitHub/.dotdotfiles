library(rstan)
library(tidyverse)
library(broom)
library(tidybayes)
library(furrr)
options(mc.cores = 4)
source("code/sbc-functions.R")
rstan_options(auto_write = TRUE)



default_stan_model = stan_model("stan/selection-mvn.stan")

default_data_args = list(N = 200, p = 2, q = 2, cutoff_quantile = 0.5)
default_params_args = list(mu_beta = 0, sd_beta = 1)
default_modelled_data_args = list(selection_type = "conventional")



first_draw = simulate_draw(
  seed = 2,
  stan_model = default_stan_model,
  gen_data_args = default_data_args,
  gen_params_args = default_params_args,
  gen_modelled_data_args = default_modelled_data_args,
  fit_model = TRUE
)


first_sbc = create_sbc_output(first_draw)
first_tidy = first_draw %>%
    tidy_sim_output()
first_tidy
plan(multisession, workers = 8)

simulated_draws = 1:100 %>%
  future_map(
    ~{sim_draw = simulate_draw(
      seed = .x,
      stan_model = default_stan_model,
      gen_data_args = default_data_args,
      gen_params_args = default_params_args,
      gen_modelled_data_args = default_modelled_data_args
      )
      
      
      if (is.null(sim_draw$bayes_fit)) {
        return(list(sbc = NULL, tidy_df = NULL))
      } 
      sbc_draw = sim_draw %>% 
        create_sbc_output() %>%
        mutate(draw = .x)

       tidy_output = sim_draw %>%
        tidy_sim_output() %>%
        mutate(draw = .x)
        return(list(sbc = sbc_draw, tidy_df = tidy_output))
        },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE, packages = "broom")
  )


tidy_sim_draws = map_dfr(simulated_draws, "tidy_df")
sbc_draws = map_dfr(simulated_draws, "sbc")


#### Plots #####
sbc_draws

sbc_draws %>%
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
      y = "Realised Quantile"
  )


tidy_sim_draws %>%
  filter(term == "beta") %>%
  group_by(model, term) %>%
  summarise(mean_estim = mean(estimate), med_estim = median(estimate))

tidy_sim_draws %>%
  filter(term == "beta") %>%
  ggplot(aes(x = estimate, fill = model)) +
  geom_histogram(bins = 60) +
  facet_wrap(~j)


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
  filter(term == "beta") %>%
  ggplot(aes( 
    x = true_value, 
    y = estimate,
    colour = model
  )) + 
  geom_point() + 
  facet_wrap(~j) +
  geom_abline()



tidy_sim_draws %>%
  mutate(error = estimate - true_value) %>%
  group_by(term, j, model) %>%
  summarise( 
    mean_error = mean(error),
    mae = mean(abs(error)), 
    mse = mean(error^2)
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



