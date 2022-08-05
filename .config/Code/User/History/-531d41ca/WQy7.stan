data {
  // dimensions
  int<lower=1> N;
  int<lower=1> p;
  int<lower=1> q;
  // covariates
  matrix[N, p] X;
  matrix[N, q] Z;
  // responses
  int<lower=0, upper=1> D[N];
  vector[N] y;
}
parameters {
  vector[p] beta;
  vector[q] gamma;
  real<lower=-1,upper=1> rho;
  real<lower=0> sigma_y;
}
model {
  // naive (truncated) priors
  beta ~ normal(0, 1);
  gamma ~ normal(0, 1);
  rho ~ normal(0, 0.25);
  sigma_e ~ normal(0, 1);
  {
    // log-likelihood
    vector[N] Xb = X * beta;
    vector[N] Zg = Z * gamma;
    for(i in 1:N) {
      if(D[i] > 0) {
        target += normal_lpdf(y[i] | Xb[i], sigma_e) + log(Phi((Zg[i] + rho / sigma_e * (y[i] - Xb[i])) / sqrt(1 - rho^2)));
      }
      else {
        target += log(Phi(-Zg[i]));
      }
    }
  }
}