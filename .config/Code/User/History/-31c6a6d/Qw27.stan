


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

    array[N] int<lower=0, upper = 1> D;
    array[N] int<lower=0> D_ind; // index that maps 1:N to 1:N_obs for D == 1
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
            if (D[i] == 1) {
                    h_obs[c, i_obs, ] = h[c, i, ];
                    i_obs += 1;
            }
        }
    }
   } 
}

parameters {
    real<lower = 0, upper = 1> alpha; // pooling coef
    array[N_choices] cholesky_factor_corr[d+1] L_Omega_diff; // Components for MVN Sigma
    cholesky_factor_corr[d+1] L_Omega_mu; // Components for MVN Sigma
    array[N_choices] vector<lower=0, upper=pi()/2>[d+1] tau_unif; // Scale for MVN Sigma
    vector<lower=0, upper=pi()/2>[d+1] tau_unif_mu; // Scale for MVN Sigma
    array[N_choices] real<lower=0> tau;
    real mu_tau;
    real<lower=0> sigma_tau;
    vector[p] beta;
    array[N_choices] matrix[q, d] gamma;

    vector[N_obs] y_latent;
}
transformed parameters {
    /*
    This is a mess because we work with correlation matrices initially but need 
    to pass the cholesky factor of a covariance matrix to multi normal


    Sigma = diag(tau)Omega diag(tau)
    where Omega is correlation matrix. L_Omega is cholesky factor of Omega, i.e.
    the correlation matrix. L is the cholesky factor of Sigma, the covariance 
    matrix, and L_h is the cholesky factor of the block covariance matrix just 
    for the H terms, excluding y.
    */
    corr_matrix[d+1] Omega_mu = L_Omega_mu * L_Omega_mu';
    array[N_choices] corr_matrix[d+1] Omega_group;
    array[N_choices] cov_matrix[d+1] Sigma;
    array[N_choices] cholesky_factor_cov[d+1] L;
    // array[N_choices] vector<lower=0>[d+1] tau = 2.5*tan(tau_unif);
    array[N_choices] cholesky_factor_cov[d] L_h;
    array[N_choices] cholesky_factor_corr[d+1] L_Omega;

    for (c in 1:N_choices) {

        Omega_group[c] = (1 - alpha)*Omega_mu + 
                        alpha*(L_Omega_diff[c]*L_Omega_diff[c]');
        L_Omega[c] = cholesky_decompose(Omega_group[c]);
        L[c] = diag_pre_multiply(tau[c], L_Omega[c]);
        Sigma[c] = L[c] * L[c]';
        L_h[c] = block(L[c], 2, 2, d, d);
    }
}
model {
    alpha ~ beta(3, 3);
    L_Omega_mu ~ lkj_corr_cholesky(2);
    L_Omega_diff ~ lkj_corr_cholesky(1/alpha);
    mu_tau ~ normal(0, 5);
    sigma_tau ~ normal(0, 5);
    tau ~ cauchy(mu_tau, sigma_tau);


    // tau_unif ~ uniform(0, pi()/2); // not needed but let's be explicit

    beta ~ normal(0, 5);
    to_vector(gamma) ~ normal(0, 5);
    // y is measured with noise as it's in itself estimated
    {
        // log-likelihood
        vector[N] Xb = X * beta;
        array[N_choices] matrix[N, d] Zg;
        array[N_choices] matrix[N, d+1] mu_vec;
        array[N_choices] matrix[N_obs, d+1] y_h;

        for (c in 1:N_choices) {
            Zg[c] = Z[c] * gamma[c];
            y_h[c] = append_col(y_latent, h_obs[c]);
            mu_vec[c] = append_col(Xb, Zg[c]);
            int i_obs = 1;
            for(i in 1:N) {
                if (D[i] == 1) {
                    y[i] ~ normal(y_latent[i_obs], y_latent_se_hat);
                    target += multi_normal_cholesky_lpdf(y_h[c, i_obs, ] | mu_vec[c, i, ], L[c]);
                    i_obs += 1;
                }
                if (D[i] == 0) {

                    if (d == 1) {
                        h[c, i, 1] ~ normal(Zg[c, i], sqrt(Sigma[c, 2,2]));
                    } else {
                        target += multi_normal_cholesky_lpdf(h[c, i, ] | mu_vec[c, i, 2:(d+1)], L_h[c]);
                    }
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
