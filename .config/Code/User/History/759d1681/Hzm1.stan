data {
    int<lower=0> N;
    int<lower=0> K;
    int<lower=0> J;
    matrix[N, J] y;
    array[N] int<lower=0,upper=K> treat_arm; 
}
parameters {
    matrix[K, J] beta;
    array[K] corr_matrix[J] Omega;
    array[K] vector<lower=0>[J] tau;
}

model {
    for (k in 1:K) {
        beta[k, :] ~ normal(0, 5);
        tau[k] ~ cauchy(0, 2.5);
        Omega[k] ~ lkj_corr(2);
    }

    for (i in 1:N) {
        y[i,:] ~ multi_normal(beta[treat_arm[i], :], quad_form_diag(Omega[treat_arm[i]], tau[treat_arm[i]]));
    }
}