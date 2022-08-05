data {
#include sec-data.stan
    array[N] int<lower=0, upper = 1> D;
    array[N] real<lower=0> y_latent_se_hat;
}
transformed data {
#include sec-transformed-data.stan
}
parameters {
#include sec-parameters-lkj.stan

    vector[p] beta;
    matrix[q, d] gamma;

    vector[N] y_latent;

    vector<lower=0, upper=1>[d+1] u;
}
transformed parameters {
#include sec-transformed-parameters-lkj.stan
    cholesky_factor_cov[d] L_h;
    L_h = block(L, 2, 2, d, d);
}
model {
#include sec-model-priors-lkj.stan
    beta ~ normal(0, 5);
    to_vector(gamma) ~ normal(0, 5);
    // y is measured with noise as it's in itself estimated
    y ~ normal(y_latent, y_latent_se_hat)
    {
        // log-likelihood
        vector[N] Xb = X * beta;
        matrix[N, d] Zg = Z * gamma;
        matrix[N, d+1] mu_vec;
        mu_vec = append_col(Xb, Zg);
        for(i in 1:N) {
            if (D[i] == 1) {
                target += multi_normal_cholesky_lpdf(y_h[i, ] | mu_vec[i, ], L);
            }
            if (D[i] == 0) {
                if (d == 1) {
                    y_h[i, 2] ~ normal(Zg[i], sqrt(Sigma[2,2]));
                } else {
                    target += multi_normal_cholesky_lpdf(y_h[i, 2:(d+1)] | mu_vec[i, 2:(d+1)], L_h);
                }
            }
        }
    }
}
generated quantities {
    /* Hardcoded for d = 1 */
   real<lower=-1, upper=1> rho = Sigma[1, 2]/(sqrt(Sigma[1, 1])*sqrt(Sigma[2, 2]));
   real<lower=0> sigma_y = Sigma[1, 1];
   real<lower=0> sigma_h = Sigma[2, 2];
}












































functions {
#include sec-functions-trunc-mvn.stan
}
data {
#include sec-data.stan
    array[N] int<lower=0, upper = 1> D;

    vector[2] b;                // lower or upper bound
    // s[k] ==  0 implies no constraint; otherwise 
    // s[k] == -1 -> b[k] is an upper bound 
    // s[k] == +1 -> b[k] is a lower bound
    vector<lower=-1,upper=1>[2] s;
    array[N] real<lower=0> y_latent_se_hat;
}
parameters {
    vector[p] beta;
    vector[q] gamma;
    cholesky_factor_cov[2, 2] L;
    vector<lower=0, upper=1>[2] u;
    vector[N] y_latent;
}
transformed parameters {
   /* ... declarations ... statements ... */
   cov_matrix[2] Sigma;
   Sigma = L * L';
}
model {
    beta ~ normal(0, 5);
    gamma ~ normal(0, 5);

    // y is measured with noise as it's in itself estimated
    y ~ normal(y_latent, y_latent_se_hat);

    {
        // log-likelihood
        vector[N] Xb = X * beta;
        vector[N] Zg = Z * gamma;
        matrix[N, 2] mu_vec;
        matrix[N, 2] y_h;
        mu_vec = append_col(Xb, Zg);
        y_h = append_col(y_latent, h);
        for(i in 1:N) {
            if (D[i] == 1) {
                target += log(make_stuff(mu_vec[i, ]', L, b, s, u)[2]);
                target += multi_normal_cholesky_lpdf(y_h[i, ] | mu_vec[i, ], L);
            }
            if (D[i] == 0) {
                h[i] ~ normal(Zg[i], sqrt(Sigma[2, 2])) T[ , b[2]];
            }
        }
    }
}
generated quantities {
   /* ... declarations ... statements ... */
   real<lower=-1, upper=1> rho = Sigma[1, 2]/(sqrt(Sigma[1, 1])*sqrt(Sigma[2, 2]));
   real<lower=0> sigma_y = Sigma[1, 1];
   real<lower=0> sigma_h = Sigma[2, 2];
}
