# THIS IS BASED OFF RTRANGUCCI'S CODE BUT MODIFIED MAY 2021
# https://discourse.mc-stan.org/t/heckman-selection-model-code-simulation/4853/4


library(MASS)
library(rstan)
library(bayesplot)
library(xtable)
S <- 20
n <- 50  # I have checked and got good coverage for N = 100, 50, 20, 15, 7 (!!!)
d <- 2 #governs dimension of X matrix in toto 

calibration_95 <- matrix(1, nrow = S, ncol = 2*d + 2)  # this is bad practice but i am very tired 
calibration_50 <- matrix(1, nrow = S, ncol = 2*d + 2)

for(s in 1:S){
  sig1 <- abs(rnorm(1))
  sig2 <- 1
  rho <- 2*runif(1)-1
  Sigma <- diag(c(sig1,sig2)) %*% matrix(c(1,rho,rho,1),2,2) %*% diag(c(sig1, sig2))
  
  X <- matrix(rnorm(n * 3 * d),n,3 * d)
  X = matrix(mvrnorm(n = n, mu = c(0, 0), Sigma = Sigma), n, 2)
  X1 <- cbind(rep(1, n), X[,1])
  X2 = cbind(rep(1, n), X[, 1])
  
  b1 <- rnorm(d)
  b2 <- rnorm(d)
  
  noise <- mvrnorm(n = n, mu = c(0, 0), Sigma = Sigma)
  
  y2 <- (X2 %*% b2 + noise[,2]) >= 0
  y1 <- (X1 %*% b1 + noise[,1])
  y1_sel <- y1[y2]
  X1_sel <- X1[y2,] 
  
  stan_data <- list(y2 = y2[,1], y1 = y1_sel, X1 = X1_sel, 
                    X2 = X2, N = n, D = d, N_pos = sum(y2[,1]), 
                    N_neg = as.integer(length(y2[,1]) - sum(y2[,1])))
  heck <- stan_model(file='heck.stan')
  fit <- sampling(heck, data = stan_data, iter = 2000,chains=1,cores = 1)
  stan_fit_summary <- summary(fit)
  stan_fit_table <- xtable(stan_fit_summary$summary)
  stan_fit_simple_table <- as.data.frame(stan_fit_table)
  draws <- as.matrix(fit, pars = c('rho','b1','b2','sd1')) 
  true <- c(rho, b1, b2, sig1)
  
  # bayesplot::mcmc_recover_intervals(draws, true)
  
  # throw stop if not converged
  if(length(stan_fit_simple_table$Rhat[stan_fit_simple_table$Rhat > 1.05 ]) > 0 ){stop("Rhat criterion detects nonconvergence")}
  
  
  params_of_interest <- rownames(stan_fit_table)[1:(2*d+2)]
  true <- c(rho, b1, b2, sig1)
  
  bayes_too_big_95 <- which(stan_fit_simple_table$`2.5%`[1:(2*d+2)] > true)
  bayes_too_small_95 <- which(stan_fit_simple_table$`97.5%`[1:(2*d+2)] < true)
  bayes_too_big_50 <- which(stan_fit_simple_table$`25%`[1:(2*d+2)] > true)
  bayes_too_small_50 <- which(stan_fit_simple_table$`75%`[1:(2*d+2)] < true)
  # because empty is not zero blegh 
  if(length(bayes_too_big_95)==0){  bayes_too_big_95 <- 0  }
  if(length(bayes_too_small_95)==0){  bayes_too_small_95 <- 0  }
  if(length(bayes_too_big_50)==0){  bayes_too_big_50 <- 0  }
  if(length(bayes_too_small_50)==0){  bayes_too_small_50 <- 0  }
  
  calibration_50[s,bayes_too_big_50] <- 0
  calibration_50[s,bayes_too_small_50] <- 0
  
  calibration_95[s,bayes_too_big_95] <- 0
  calibration_95[s,bayes_too_small_95] <- 0
} #closes the forloop indexed by s 


mean(calibration_50)
mean(calibration_95)
#saveRDS(list(calibration_50, calibration_95), file = "simulations/output/heckman_modified_mc_n_50.rds")

