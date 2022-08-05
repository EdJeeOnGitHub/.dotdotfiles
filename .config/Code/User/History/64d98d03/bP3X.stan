
data {
#include sec-data.stan
    array[N] int<lower=0, upper = 1> D;
    int<lower=0> N_obs;
}
transformed data {
    int<lower=0> N_unobs;
    matrix[N_obs, d+1] y_h_obs;
    matrix[N_unobs, d] h_unobs; 
    
    {
        int i_obs = 1;
        int i_unobs = 1;
        for (i in 1:N) {
            if (D[i] == 1) {
                y_h_obs[i_obs] = [y[i], h[i]]';
                i_obs += 1;
            }
            if (D[i] != 1) {
                h_unobs[i_unobs] = h[i];
                i_unobs += 1;
            }
        }
    }

}
parameters {
#include sec-parameters-lkj.stan

    vector[p] beta;
    matrix[q, d] gamma;


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
