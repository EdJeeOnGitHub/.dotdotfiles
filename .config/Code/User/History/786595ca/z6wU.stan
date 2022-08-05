data {
    int<lower=0> N;
    vector[N]  Y;
    vector[N]  D;
    vector[N]  X;
    vector[N]  Z;

}

parameters{
    real beta;
    real delta_1;
    real delta_2;
    real gamma;
    cholesky_factor_corr[2] L_Omega;
    vector<lower=0>[2] tau;
} 
transformed parameters {
    cholesky_factor_cov[2] L = diag_pre_multiply(tau, L_Omega);
}


model {
    array[N] vector[2] outcome_vec;
    vector[N]  mu_vec_1; 
    vector[N]  mu_vec_2; 

    L_Omega ~ lkj_corr(2);
    tau ~ normal(0, 5);

    beta ~ normal(0, 5);
    delta_1 ~ normal(0, 5);
    delta_2 ~ normal(0, 5);
    gamma ~ normal(0, 5);


    mu_vec_2 = Z*gamma + X*delta_2;
    mu_vec_1 = (mu_vec_2)*beta + X*delta_1;
    outcome_vec = append_col(Y, D);



    target += multi_normal_cholesky(outcome_vec | [mu_vec_1, mu_vec_2], L);

}