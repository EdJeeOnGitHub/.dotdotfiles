data {
    int<lower=1> N;
    int<lower=1> p;
    int<lower=1> q;
    int<lower=1> d;

    vector[N] y;
    array[N] vector[d] h;

    matrix[N, p] X;
    matrix[N, q*d] Z;
    array[N] int<lower=0, upper = 1> D;
}
transformed data {
    matrix[N, d] y_h = append_col(y, h);
}
parameters {
#include sec-parameters-lkj.stan
    vector[p] beta;
    matrix[q, d] gamma;
}
transformed parameters {
#include sec-transformed-parameters-lkj.stan
}
model {
#include sec-model-priors-lkj.stan
    beta ~ normal(0, 5);
    gamma ~ normal(0, 5);

    {
        // log-likelihood
        vector[N] Xb = X * beta;
        matrix[N, d] Zg = Z * gamma;
        matrix[N, d+1] mu_vec;
        mu_vec = append_col(Xb, Zg);
        for(i in 1:N) {
            if (D[i] == 1) {
                target += log(make_stuff(mu_vec[i, ]', L, b, s, u)[2]);
                target += multi_normal_cholesky_lpdf(y_h[i, ] | mu_vec[i, ], L);
            }
            if (D[i] == 0) {
                h[i] ~ normal(Zg[i], sqrt(Sigma[2, 2])) T[b[2] , ];
            }
        }
    }
}
generated quantities {
   /* ... declarations ... statements ... */
   real<lower=-1, upper=1> rho = Sigma[1, 2]/(sqrt(Sigma[1, 1])*sqrt(Sigma[2, 2]));
   real<lower=0> sigma_y = Sigma[1, 1];
   real<lower=0> sigma_h = Sigma[2, 2];
}
