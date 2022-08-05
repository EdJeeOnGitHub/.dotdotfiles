
functions {
#include sec-functions-trunc-mvn.stan
}
data {
#include sec-data.stan
    array[N] int<lower=0, upper = 1> D;

    vector[2] b;                // lower or upper bound
    // s[k] ==  0 implies no constraint; otherwise 
    // s[k] == -1 -> b[k] is an upper bound 
    // s[k] == +1 -> b[k] is a lower bound
    vector<lower=-1,upper=1>[2] s;
}
transformed data {
#include sec-transformed-data.stan
}
parameters {
#include sec-parameters-lkj.stan

    vector[p] beta;
    vector[q] gamma;


    vector<lower=0, upper=1>[2] u;
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
        vector[N] Zg = Z * gamma;
        matrix[N, 2] mu_vec;
        mu_vec = append_col(Xb, Zg);
        for(i in 1:N) {
            if (D[i] == 1) {
                target += log(make_stuff(mu_vec[i, ]', L, b, s, u)[2]);
                target += multi_normal_cholesky_lpdf(y_h[i, ] | mu_vec[i, ], L);
                target += (1 - log(Phi(-Zg[i]/sigma_h)));
            }
            if (D[i] == 0) {
                h[i] ~ normal(Zg[i], sqrt(Sigma[2, 2])) T[ , 0];
                target += log(Phi(-Zg[i]/sigma_h));
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
