
functions {
    real gompertz_lcdf(real t, real shape, real scale) {
        real lp;
        lp = log(1 - exp(-shape * (exp(scale*t) - 1)));
        return lp;
    }
    real gompertz_lccdf(real t, real shape, real scale) {
        real lp;
        lp = -shape*(exp(scale*t) - 1);
        return lp;
    }
}

/* 
  Stan code for interval-censored analyses using Weibull survival function 
  follows prior structure same as in non-interval method
  assumes all patients have been interval- or right-censored 
*/

data {
  int<lower=0> N;             //number of datapoints
  // int<lower=0> no;            //number of outcomes
  // int<lower=0> nd[no];        //number of beta parameters (for each outcome)
  int<lower=1> nc;            //number of covariates + 1
  int<lower=0> nsc;           //number of survival constraints
  
  /* covariates */
  matrix[N, nc] X;     //design matrix, including intercept
  
  /* events */
  int  censoring[N];
  real interval_left[N]; //times of intervals for interval-censored data
  real interval_right[N]; //times of intervals for interval-censored data
  
  /* priors */
  real          mu_beta[nc];  //prior mean
  real<lower=0> sigma_beta[nc]; //prior variance
  // real          baseline_mean[nsc];
  // real          baseline_sigma[nsc];
  // real<lower=0> baseline_time[nsc];
}

parameters {
  vector[nc]    beta;     //values of log(HR)'s - see below
  real lshape_;   //log(shape)
}

transformed parameters {
  /* 
     in medical statistics usually S(t) = exp(-lambda*t^shape)
     where lambda = scale^(-shape);
     if we parameterize R/Stan scale = sum(beta*X), we obtain HR as:
     HR_i = exp(-shape * beta_i)
     
     so the parameter we estimate is not log(HR), but its transformation
     therefore we call model estimate 'beta_transformed' and 'beta' is just log(HR)
     for intercept this issue is not important!
  */
  vector[nc]    beta_transformed;
  real shape;
  shape = exp(lshape_);
  if(nc > 0) {
    for(i in 1:nc)
      beta_transformed[i] = -1*beta[i];
  }
}

model {
  real scale[N];
  // vector[nsc] baseline_survival;
  
  lshape_ ~ normal(2, 0.5);

  for(j in 1:nc) {
    beta[j]  ~ normal(mu_beta[j], sigma_beta[j]);
  }
  if(N > 0) {
    for(i in 1:N){
      scale[i] = exp(X[i,]*beta_transformed);
      //censoring 1 means non-interval censoring
      if(censoring[i] == 1) {
        target += gompertz_lccdf(interval_left[i] | shape, scale[i]);
      } 
      //censoring 0 means interval censored patient
      else {
        target += log_diff_exp(gompertz_lcdf(interval_right[i] | shape, scale[i]),
                               gompertz_lcdf(interval_left[i]  | shape, scale[i]));
      }
    }
  }
}
