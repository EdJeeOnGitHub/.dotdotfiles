/*
Multivariate Normal Estimation

Hardcoded for two dimensions (i.e. d === 1)
*/
data {
#include sec-data.stan
}
transformed data {
#include sec-transformed-data.stan
}
parameters {
#include sec-parameters.stan
}
transformed parameters {
#include sec-transformed-parameters.stan
}
model {
    beta ~ normal(0, 5);
    gamma ~ normal(0, 5);
    rho ~  normal(0, 1);
    sigma_y ~ normal(0, 5);
    sigma_h ~ normal(0, 5);
    {
        // log-likelihood
        vector[N] Xb = X * beta;
        vector[N] Zg = Z * gamma;
        matrix[N, d+1] mu_vec;
        mu_vec = append_col(Xb, Zg);
        for(i in 1:N) {
            target += multi_normal_lpdf(y_h[i, ] | mu_vec[i, ], Sigma);
        }
    }
}
