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
   row_vector beta;
   real<lower=0> sigma;
}
model {
    
    for (s in 1:S) {
        beta_hat[s] ~ multi_normal(beta, XX_inv[s]*sigma);
    }

}