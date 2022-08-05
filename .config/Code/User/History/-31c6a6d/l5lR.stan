
data {
    int<lower=1> N;
    int<lower=1> p;
    int<lower=1> q;
    int<lower=1> d;
    int<lower=1> N_choices;

    vector[N] y;
    array[N_choices] matrix[N, d] h;

    matrix[N, p] X;
    array[N_choices] matrix[N, q] Z;

    array[N_choices, N] int<lower=0, upper = 1> D;
    array[N_choices, N] int<lower=0> D_ind; // index that maps 1:N to 1:N_obs for D == 1
    int<lower=1> N_obs;
    array[N_obs] real<lower=0> y_latent_se_hat;

}
transformed data {
   /* ... declarations ... statements ... */
   array[N_choices] matrix[N_obs, d] h_obs; 
   {
    for (c in 1:N_choices) {
        int i_obs = 1;
        for (i in 1:N) {
            if (D[c, i] == 1) {
                    h_obs[c, i_obs, ] = h[c, i, ];
                    i_obs += 1;
            }
        }
    }
   } 
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
        matrix[N_obs, d+1] y_h;
        y_h = append_col(y_latent, h_obs);
        mu_vec = append_col(Xb, Zg);



        int i_obs = 1;
        for(i in 1:N) {
            if (D[i] == 1) {
                y[i] ~ normal(y_latent[i_obs], y_latent_se_hat);
                target += multi_normal_cholesky_lpdf(y_h[i_obs, ] | mu_vec[i, ], L);
                i_obs += 1;
            }
            if (D[i] == 0) {

                if (d == 1) {
                    h[i, 1] ~ normal(Zg[i], sqrt(Sigma[2,2]));
                } else {
                    target += multi_normal_cholesky_lpdf(h[i, ] | mu_vec[i, 2:(d+1)], L_h);
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
