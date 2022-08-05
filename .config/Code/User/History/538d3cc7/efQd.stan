
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
  vector[N] h;
}
parameters {
  vector[p] beta;
  vector[q] gamma;
  real<lower=-1,upper=1> rho;
  real<lower=0> sigma_y;
}
model {
  // naive (truncated) priors
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  rho ~ normal(0, 0.5);
  sigma_y ~ normal(0, 5);
  {
    // log-likelihood
    vector[N] Xb = X * beta;
    vector[N] Zg = Z * gamma;
    for(i in 1:N) {
      if(D[i] > 0) {
        target += normal_lpdf(y[i] | Xb[i], sigma_y) + log(Phi((Zg[i] + rho / sigma_y * (y[i] - Xb[i])) / sqrt(1 - rho^2)));
      }
      else {
        h[i] ~ normal(Zg[i], sigma_h);
        // target += log(Phi(-Zg[i]));
      }
    }
  }
}
generated quantities {
   cov_matrix[2] Sigma;
   int sigma_h = 1;

   Sigma[1,1] = sigma_y^2;
   Sigma[1,2] = sigma_y*sigma_h*rho;
   Sigma[2,1] = sigma_y*sigma_h*rho;
   Sigma[2,2] = sigma_h^2;
}
