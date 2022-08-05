



data {
    int<lower=1> N;
    int<lower=1> p;
    int<lower=1> q;
    int<lower=1> d_selection;
    int<lower=1> N_choices;

    vector[N] y;
    array[N_choices] matrix[N, d_selection] h;

    matrix[N, p] X;
    matrix[N, q] Z;

    array[N] int<lower=0, upper = 1> D;
    array[N] int<lower=0> D_ind; // index that maps 1:N to 1:N_obs for D == 1
    int<lower=1> N_obs;
    array[N_obs] real<lower=0> y_latent_se_hat;

}
transformed data {
    int<lower=0> N_unobs = N - N_obs;
    array[N_choices] matrix[N_obs, d_selection] h_obs;
    array[N_choices, N_unobs] row_vector[d_selection] h_unobs; 
    array[N_obs] real y_obs; 
    matrix[N_obs, p] X_obs;
    matrix[N_obs, q] Z_obs;
    matrix[N_unobs, p] X_unobs;
    matrix[N_unobs, q] Z_unobs;
    // Create vectors for unobserved and observed component so we can perform 
    // vectorised operations on these instead of for looping over i 
    {
        for (c in 1:N_choices) {
            int i_obs = 1;
            int i_unobs = 1;
            for (i in 1:N) {
                if (D[i] == 1) {
                    h_obs[c, i_obs, ] = h[c, i];
                    i_obs += 1;
                }
                if (D[i] != 1) {
                    h_unobs[c, i_unobs] = h[c, i];
                    i_unobs += 1;
                }
            }
        }
    }
    profile("td"){
        int i_obs = 1;
        int i_unobs = 1;
        for (i in 1:N) {
            if (D[i] == 1) {
                y_obs[i_obs] = y[i];
                X_obs[i_obs] = X[i];
                Z_obs[i_obs] = Z[i];
                i_obs += 1;
            }
            if (D[i] != 1) {
                X_unobs[i_unobs] = X[i];
                Z_unobs[i_unobs] = Z[i];
                i_unobs += 1;
            }
        }
    }
}

parameters {
    real<lower = 0, upper = 1> alpha; // pooling coef
    array[N_choices] cholesky_factor_corr[d_selection+1] L_Omega_diff; // Components for MVN Sigma
    cholesky_factor_corr[d_selection+1] L_Omega_mu; // Components for MVN Sigma
    array[N_choices] vector<lower=0, upper=pi()/2>[d_selection+1] tau_unif; // Scale for MVN Sigma
    array[N_choices] vector<lower=0>[d_selection+1] tau;
    real mu_tau;
    real<lower=0> sigma_tau;
    vector[p] beta;
    array[N_choices] matrix[q, d_selection] gamma;

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
    corr_matrix[d_selection+1] Omega_mu = L_Omega_mu * L_Omega_mu';
    array[N_choices] corr_matrix[d_selection+1] Omega_group;
    array[N_choices] cov_matrix[d_selection+1] Sigma;
    array[N_choices] cholesky_factor_cov[d_selection+1] L;
    // array[N_choices] vector<lower=0>[d_selection+1] tau = 2.5*tan(tau_unif);
    array[N_choices] cholesky_factor_cov[d_selection] L_h;
    array[N_choices] cholesky_factor_corr[d_selection+1] L_Omega;

    profile("transformed params") {
        for (c in 1:N_choices) {

            Omega_group[c, , ] = (1 - alpha)*Omega_mu + 
                            alpha*(L_Omega_diff[c, , ]*L_Omega_diff[c, , ]');
            L_Omega[c, , ] = cholesky_decompose(Omega_group[c, , ]);
            L[c, , ] = diag_pre_multiply(tau[c, ], L_Omega[c, , ]);
            Sigma[c, , ] = L[c, , ] * L[c, , ]';
            L_h[c, , ] = block(L[c, , ], 2, 2, d_selection, d_selection);
        }
    }
}
model {
    alpha ~ beta(3, 3);
    L_Omega_mu ~ lkj_corr_cholesky(2);
    mu_tau ~ normal(0, 5);
    sigma_tau ~ normal(0, 5);


    // tau_unif ~ uniform(0, pi()/2); // not needed but let's be explicit

    beta ~ normal(0, 5);
    // y is measured with noise as it's in itself estimated
    profile("modelling"){
        // log-likelihood
        array[N_choices] matrix[N_obs, d_selection+1] mu_vec_obs;
        array[N_choices] matrix[N_unobs, d_selection] mu_vec_unobs;
        array[N_choices, N_obs]  row_vector[d_selection + 1] mu_array_obs;
        array[N_choices, N_unobs] row_vector[d_selection] mu_array_unobs;
        array[N_choices] matrix[N_obs, d_selection + 1] y_h_matrix_obs;
        array[N_choices, N_obs] row_vector[d_selection+1] y_h_array_obs;

        for (c in 1:N_choices) {

            mu_vec_obs[c] = append_col(X_obs*beta, Z_obs*gamma[c]);
            mu_vec_unobs[c] = Z_unobs*gamma[c];
            y_h_matrix_obs[c] = append_col(y_latent, h_obs[c]);

            for (i in 1:N_obs) {
                y_h_array_obs[c, i] = y_h_matrix_obs[c, i, ];
                mu_array_obs[c, i] = mu_vec_obs[c, i, ];
            }
            for (i in 1:N_unobs) {
                mu_array_unobs[c, i] = mu_vec_unobs[c, i, ];
            }
            target += multi_normal_cholesky_lpdf(
                y_h_array_obs[c] |
                mu_array_obs[c],
                L[c]
            );

            target += multi_normal_cholesky_lpdf(h_unobs[c] | mu_array_unobs[c], L_h[c]);
        }
            target += normal_lpdf(y_obs | y_latent, y_latent_se_hat);


//////////////////////////
        // vector[N] Xb = X * beta;
        // array[N_choices] matrix[N, d_selection] Zg;
        // array[N_choices] matrix[N, d_selection+1] mu_vec;
        // array[N_choices] matrix[N_obs, d_selection+1] y_h;

        // for (c in 1:N_choices) {
        //     L_Omega_diff[c] ~ lkj_corr_cholesky(1/alpha);
        //     tau[c] ~ cauchy(mu_tau, sigma_tau);
        //     to_vector(gamma[c, ]) ~ normal(0, 5);

        //     Zg[c] = Z * gamma[c];
        //     y_h[c] = append_col(y_latent, h_obs[c]);
        //     mu_vec[c] = append_col(Xb, Zg[c]);
        //     int i_obs = 1;
        //     for(i in 1:N) {
        //         if (D[i] == 1) {
        //             y[i] ~ normal(y_latent[i_obs], y_latent_se_hat);
        //             target += multi_normal_cholesky_lpdf(y_h[c, i_obs, ] | mu_vec[c, i, ], L[c]);
        //             i_obs += 1;
        //         }
        //         if (D[i] == 0) {

        //             if (d_selection == 1) {
        //                 h[c, i, 1] ~ normal(Zg[c, i], sqrt(Sigma[c, 2,2]));
        //             } else {
        //                 target += multi_normal_cholesky_lpdf(h[c, i, ] | mu_vec[c, i, 2:(d_selection+1)], L_h[c]);
        //             }
        //         }
        //     }
        // }

    }
}
generated quantities {
//     /* Hardcoded for d = 1 */
//    real<lower=-1, upper=1> rho = Sigma[1, 2]/(sqrt(Sigma[1, 1])*sqrt(Sigma[2, 2]));
//    real<lower=0> sigma_y = Sigma[1, 1];
//    real<lower=0> sigma_h = Sigma[2, 2];
}
