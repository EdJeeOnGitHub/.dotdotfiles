data {

int<lower=1> N;
int<lower=1> p;
int<lower=1> q;
int<lower=1> d;
vector[N] y;
vector[N] h;

matrix[N, p] X;
matrix[N, q] Z;
    array[N] int<lower=0, upper = 1> D;
}
transformed data {

matrix[N, d+1] y_h;
y_h = append_col(y, h);
}
parameters {

cholesky_factor_corr[d+1] L_Omega; // Components for MVN Sigma
vector<lower=0, upper=pi()/2>[d+1] tau_unif; // Scale for MVN Sigma
    vector[p] beta;
    matrix[q, d] gamma;
    vector<lower=0, upper=1>[d+1] u;
}
transformed parameters {

cov_matrix[d+1] Sigma;
cholesky_factor_cov[d+1] L;
vector<lower=0>[d+1] tau = 2.5*tan(tau_unif);
L = diag_pre_multiply(tau, L_Omega);
Sigma = L * L';
    cholesky_factor_cov[d+1] L_h;
    L_h = block(L, 2, 2, d-1, d-1);
}
model {

L_Omega ~ lkj_corr_cholesky(2);
tau_unif ~ uniform(0, pi()/2); // not needed but let's be explicit
    beta ~ normal(0, 5);
    to_vector(gamma) ~ normal(0, 5);
    {
        print(L_h);
        // log-likelihood
        vector[N] Xb = X * beta;
        matrix[N, d] Zg = Z * gamma;
        matrix[N, d+1] mu_vec;
        mu_vec = append_col(Xb, Zg);
        for(i in 1:N) {
            if (D[i] == 1) {
                target += multi_normal_cholesky_lpdf(y_h[i, ] | mu_vec[i, ], L);
            }
            if (D[i] == 0) {
                if (d == 1) {
                    y_h[i, 2] ~ normal(Zg[i], sqrt(Sigma[2,2]));
                } else {
                    target += multi_normal_cholesky_lpdf(y_h[i, 2:(d+1)] | mu_vec[i, 2:(d+1)], L_h);
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