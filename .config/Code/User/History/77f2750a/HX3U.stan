data {
    int<lower=1> S;
    int<lower=1> K;
    int<lower=1> N;
    array[S] row_vector[K] beta_hat;
    array[S] matrix[N, K] X;
}
transformed data {
   /* ... declarations ... statements ... */
   array[S] matrix[K, K] XX_inv;
   for (s in 1:S) {
    XX_inv[s] = inverse_spd(X[s]' * X[s]);
   }
}

parameters {
   /* ... declarations ... */
   row_vector[K] beta;
   real<lower=0> sigma;
   matrix[N, 1] latent_error;
}
model {

    for (s in 1:S) {
        beta_hat[s] = beta + to_row_vector(XX_inv) * (X[s] * latent_error)  
    }
    latent_error[, 1] ~ normal(0, sqrt(sigma));

}
