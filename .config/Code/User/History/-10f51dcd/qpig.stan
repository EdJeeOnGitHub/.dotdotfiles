data {
#include sec-data.stan
    array[N] int<lower=0, upper = 1> D;
    array[N] int<lower=0> D_ind; // index that maps 1:N to 1:N_obs for D == 1
    int<lower=1> N_obs;
    array[N_obs] real<lower=0> y_latent_se_hat;

}
parameters {
#include sec-parameters-lkj.stan

    vector[p] beta;
    matrix[q, d] gamma;

    vector[N_obs] y_latent;

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
    {
        // log-likelihood
        vector[N] Xb = X * beta;
        matrix[N, d] Zg = Z * gamma;
        matrix[N, d+1] mu_vec;
        matrix[N, d+1] y_h;
        y_h = append_col(y_latent, h);
        mu_vec = append_col(Xb, Zg);
        for(i in 1:N) {
            if (D[i] == 1) {
                y[i] ~ normal(y_latent[D_ind[i]], y_latent_se_hat);
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
