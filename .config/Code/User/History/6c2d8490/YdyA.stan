
functions {
    real gompertz_lcdf(real t, real shape, real scale) {
        real lp;
        lp = log(1 - exp(-shape * (exp(scale*t) - 1)));
        return lp;
    }
    real gompertz_lccdf(real t, real shape, real scale) {
        real lp;
        lp = -shape*(exp(scale*t) - 1);
        return lp
    }
}
/* 
  Stan code for interval-censored analyses using Weibull survival function 
  follows prior structure same as in non-interval method
  assumes all patients have been interval- or right-censored 
*/
data {
  int<lower=0> N;             //number of datapoints
  int<lower=1> nc;            //number of covariates + 1
  int<lower=0> nsc;           //number of survival constraints
  
  /* covariates */
  matrix[N, nc] X;     //design matrix, including intercept
  
  /* events */
  int  censoring[N];
  real interval_left[N]; //times of intervals for interval-censored data
  real interval_right[N]; //times of intervals for interval-censored data
  
  /* priors */
  real          beta_mean[nc];  //prior mean
  real<lower=0> beta_sigma[nc]; //prior variance
}

parameters {
  vector[nc]    beta;     
}


model {
  real pred_vec[N];

  for(j in 1:nc) {
    beta[j]  ~ normal(beta_mean[j], beta_sigma[j]);
  }
  if(N > 0) {
    for(i in 1:N){
      pred_vec[i] = exp(-X[i,]*beta);
      //censoring 1 means non-interval censoring
      if(censoring[i] == 1) {
        target += exponential_lccdf(interval_left[i] | pred_vec[i]);
      } 
      //censoring 0 means interval censored patient
      else {
        target += log_diff_exp(exponential_lcdf(interval_right[i] | pred_vec[i]),
                               exponential_lcdf(interval_left[i]  | pred_vec[i]));
      }
    }
  }
}
generated quantities {
  real shape;
  shape = 1;   

}