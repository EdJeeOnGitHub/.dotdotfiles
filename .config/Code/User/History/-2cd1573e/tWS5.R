library(tidyverse)
library(data.table)
library(cmdstanr)

gen_data = function(){
    N = 1000
    sigma_1 = abs(rnorm(1, 1, 1))
    sigma_2 = abs(rnorm(1, 1, 1))
    rho = runif(1, 0.5, 1)
    Sigma = matrix(
        c(sigma_1^2, sigma_1*sigma_2*rho,
         sigma_1*sigma_2*rho, sigma_2^2),
         ncol = 2,
         nrow = 2
    )
    errors = MASS::mvrnorm(n = N, mu = c(0, 0), Sigma)

    Z = rnorm(n = N)
    D = rbernoulli(n = N, p = pnorm(Z + errors[, 1]))
    X = rnorm(n = N) 
    Y = 1*D + 5*X + errors[, 2]

    df = data.table(
        Y = Y,
        D = D,
        Z = Z,
        X = X
    )
    return(df)
}

fake_data = gen_data()

feedback_model = cmdstan_model("sbc/feedback.stan")
options(mc.cores = 4)
model_fit = feedback_model$sample(
    c(
        fake_data %>% as.list(),
        list(N = nrow(fake_data))

    ),
    chains = 4
)


model_fit$summary()



lm(
    data = fake_data,
    Y ~ D + X
)

library(brms)
install.packages("brms")
