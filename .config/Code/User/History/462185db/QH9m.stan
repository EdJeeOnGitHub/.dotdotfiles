
data {
#include sec-data.stan
    array[N] int<lower=0, upper = 1> D;
    array[N] int<lower=0> D_ind; // index that maps 1:N to 1:N_obs for D == 1
    int<lower=1> N_obs;
    array[N_obs] real<lower=0> y_latent_se_hat;
    real h_mu;

}
transformed data {
    int<lower=0> N_unobs = N - N_obs;
    matrix[N_obs, d] h_obs;
    array[N_unobs] row_vector[d] h_unobs; 
    array[N_obs] real y_obs; 
    matrix[N_obs, p] X_obs;
    matrix[N_obs, q] Z_obs;
    matrix[N_unobs, p] X_unobs;
    matrix[N_unobs, q] Z_unobs;
    // Create vectors for unobserved and observed component so we can perform 
    // vectorised operations on these instead of for looping over i 
    profile("td"){
        int i_obs = 1;
        int i_unobs = 1;
        for (i in 1:N) {
            if (D[i] == 1) {
                h_obs[i_obs, ] = h[i];
                y_obs[i_obs] = y[i];
                X_obs[i_obs] = X[i];
                Z_obs[i_obs] = Z[i];
                i_obs += 1;
            }
            if (D[i] != 1) {
                h_unobs[i_unobs] = h[i];
                X_unobs[i_unobs] = X[i];
                Z_unobs[i_unobs] = Z[i];
                i_unobs += 1;
            }
        }
    }
}
parameters {
#include sec-parameters-lkj.stan

    vector[p] beta;
    matrix[q, d] gamma;

    vector[N_obs] y_latent;

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
        /*
        Since matrix math is fast but stan gets picky about vectorisation 
        formatting we create matrices through matrix multiplication and then 
        just re-index these using a for loop to create arrays of row vectors.

        AFAIK there's no vectorised function for matrix => array[] row_vector
        so we just have to for loop it but it's not too slow anyway.
        */
        matrix[N_obs, d+1] mu_vec_obs;
        matrix[N_unobs, d] mu_vec_unobs;
        array[N_obs] row_vector[d+1] mu_array_obs;
        array[N_unobs] row_vector[d] mu_array_unobs;
        array[N_obs] row_vector[d+1] y_h_array_obs;
        matrix[N_obs, d+1]  y_h_matrix_obs;
        mu_vec_obs = append_col(X_obs*beta,  Z_obs*gamma);
        mu_vec_unobs = Z_unobs * gamma; 
        y_h_matrix_obs = append_col(y_latent, h_obs);
        for (i in 1:N_obs) {
            y_h_array_obs[i] = y_h_matrix_obs[i, ];
            mu_array_obs[i] = mu_vec_obs[i, ];
        }
        for (i in 1:N_unobs) {
            mu_array_unobs[i] = mu_vec_unobs[i, ];
        }
        // Hooray vectorisation!
        target += normal_lpdf(y_obs | y_latent, y_latent_se_hat);
        target += multi_normal_cholesky_lpdf(y_h_array_obs | mu_array_obs, L);
        
        if (d == 1) {
            target += normal_lpdf(h_unobs[, 1] | mu_array_unobs[, 1], sqrt(Sigma[2,2]));
        } else {
            target += multi_normal_cholesky_lpdf(h_unobs | mu_array_unobs, L_h);
        }


    }
}
generated quantities {
    /* Hardcoded for d = 1 */
   real<lower=-1, upper=1> rho = Sigma[1, 2]/(sqrt(Sigma[1, 1])*sqrt(Sigma[2, 2]));
   real<lower=0> sigma_y = Sigma[1, 1];
   real<lower=0> sigma_h = Sigma[2, 2];
}
