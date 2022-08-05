data {
    int<lower=0> N;
    array[N] real Y;
    array[N] real D;
    array[N] real X;
    array[N] real Z;

}

parameters{
    real beta;
    real delta_1;
    real delta_2;
    real gamma;
    cholesky_factor_cov[2] L;
} 


model {
    array[N] vector[2] outcome_vec;
    array[N] real mu_vec_1; 
    array[N] real mu_vec_2; 

    L ~ lkj_cov(2.0);
    beta ~ normal(0, 5);
    delta_1 ~ normal(0, 5);
    delta_2 ~ normal(0, 5);
    gamma ~ normal(0, 5);


    mu_vec_2 = Z*gamma + X*delta_2;
    mu_vec_1 = (mu_vec_2)*beta + X*delta_1;
    outcome_vec = append_col(Y, D);



    target += multi_normal_cholesky(outcome_vec | [mu_vec_1, mu_vec_2], L);

}