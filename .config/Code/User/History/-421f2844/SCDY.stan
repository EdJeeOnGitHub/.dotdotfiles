functions {
    real gompertz_lpdf(real t, real shape, real scale) {
        real lp;
        lp = log(scale) + log(shape) + shape + scale * t - shape * exp(scale * t);
        return lp;
    }
    
    real gompertz_lcdf(real t, real shape, real scale) {
        real lp;
        lp = log(1 - exp(-shape * (exp(scale * t) - 1)));
        return lp;
    }

    real gompertz_lccdf(real t, real shape, real scale) {
        real lp;
        lp = -shape * (exp(scale * t) - 1);
        return lp;
    }
    
    real survival_lpdf(real t, real shape, real scale, int model_type) {
        real lp;
        if (model_type == 1) {
            lp = exponential_lpdf(t | scale);
        }
        if (model_type == 2) {
            lp = weibull_lpdf(t | shape, scale);
        }
        if (model_type == 3) {
            lp = gompertz_lpdf(t | shape, scale);
        }
        if (model_type == 4) {
            lp = gamma_lpdf(t | shape, scale);  // rate = scale 
        }
        if (model_type == 5) {
            lp = lognormal_lpdf(t | shape, scale);  // mu = shape, sigma = scale
        }
        return lp;
    }
    
    real survival_lcdf(real t, real shape, real scale, int model_type) {
        real lp;
        if (model_type == 1) {
            lp = exponential_lcdf(t | scale);
        }
        if (model_type == 2) {
            lp = weibull_lcdf(t | shape, scale);
        }
        if (model_type == 3) {
            lp = gompertz_lcdf(t | shape, scale);
        }
        if (model_type == 4) {
            lp = gamma_lcdf(t | shape, scale);  // rate = scale 
        }
        if (model_type == 5) {
            lp = lognormal_lcdf(t | shape, scale);  // mu = shape, sigma = scale
        }
        return lp;
    }

    real survival_lccdf(real t, real shape, real scale, int model_type) {
        real lp;
        if (model_type == 1) {
            lp = exponential_lccdf(t | scale);
        }
        if (model_type == 2) {
            lp = weibull_lccdf(t | shape, scale);
        }
        if (model_type == 3) {
            lp = gompertz_lccdf(t | shape, scale);
        }
        if (model_type == 4) {
            lp = gamma_lccdf(t | shape, scale);  // rate = scale 
        }
        if (model_type == 5) {
            lp = lognormal_lccdf(t | shape, scale);  // mu = shape, sigma = scale
        }
        return lp;
    }
    
    real survival_rng(real shape, real scale, int model_type) {
        real lp; 
        if (model_type == 1) {
            lp = exponential_rng(scale);
        }
        if (model_type == 2) {
            lp = weibull_rng(shape, scale);
        }
        return lp;
        
        // Leaving out Gompertz for now
    }
}
/* 
  Stan code for interval-censored analyses using Weibull survival function 
  follows prior structure same as in non-interval method
  assumes all patients have been interval- or right-censored 
*/

data {
    int<lower=0> N;             //number of datapoints
    int<lower=1> J; // N sites
    int<lower=1> nc;            //number of covariates + 1
    array[N] int<lower=1,upper=J> site;  // site index
    /* covariates */
    matrix[N, nc] X;     //design matrix, including intercept

    /* events */
    array[N] int  censoring; // 0: right censored, 1: no censoring, 2: left censored, 3: interval censored
    array[N] real interval_left; //times of intervals for interval-censored data
    array[N] real interval_right; //times of intervals for interval-censored data

    int model_type; // 1 => exponential, 2 => weibull, 3 => gompertz, 4 => gamma, 5 => log normal
}

parameters {
  array[J] vector[nc]    beta;     //values of log(HR)'s - see below
  vector[nc] mu_beta;
  vector<lower=0>[nc] sigma_beta;
  vector[J] lshape_tilde;
  vector[1] mu_lshape;
  vector<lower=0, upper=pi()/2>[1] sigma_lshape_unif;
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
  array[J] vector[nc]    beta_transformed;
  vector<lower=0>[1] sigma_lshape;
  vector<lower=0>[J] shape;

  // Set shape 
  sigma_lshape[1] = tan(sigma_lshape_unif[1]);
  for (j in 1:J) {
    shape[j] = exp(mu_lshape[1] + lshape_tilde[j] * sigma_lshape[1]);
  }
    
  // Exponential
  if (model_type == 1) {
      // Fix shape to be 1 - a bit hacky
    if(nc > 0) {
        for (j in 1:J) {
            shape[j] = 1;
            for(i in 1:nc) {
                beta_transformed[j, i] = -1*beta[j, i]; // shape === 1
            }
        }
    }
  }
    // Weibull
  if (model_type == 2) {
    if(nc > 0) {
        for (j in 1:J) {
            for(i in 1:nc) {
                beta_transformed[j, i] = -1*beta[j, i]/shape[j];
            }
        }
    }
  }


  // Gompertz we don't re-parametrise by dividing by shape
  if (model_type == 3) {
    for (j in 1:J) {
        for(i in 1:nc) {
            beta_transformed[j, i] = -1*beta[j, i];
        }
    }
  }

  // Gamma
  if (model_type == 4) {
    if(nc > 0) {
        for (j in 1:J) {
            for(i in 1:nc) {
                beta_transformed[j, i] = -1*beta[j, i];
            }
        }
    }
  }
  
  // Log-Normal
  if (model_type == 5) {
    if(nc > 0) {
        for (j in 1:J) {
            for(i in 1:nc) {
                beta_transformed[j, i] = -1*beta[j, i];
            }
        }
    }
  }
}

model {
  array[N] real scale;
  
  mu_beta ~ normal(0, 100);
  sigma_beta ~ normal(0, 100);

  for (j in 1:J) {
      beta[j, ] ~ normal(mu_beta, sigma_beta);
  }
  
  mu_lshape ~ normal(1.4, 0.5); 
  sigma_lshape_unif ~ uniform(0, pi()/2); 
  lshape_tilde ~ normal(0, 1);
    
  // Log likelihood
  if(N > 0) {
    for(i in 1:N){
      scale[i] = exp(X[i,]*beta_transformed[site[i], ]);
      // censoring 0 means right-censored
      if(censoring[i] == 0) {
        target += survival_lccdf(interval_left[i] | shape[site[i]], scale[i], model_type);
      } 
      // censoring 1 means no censoring
      else if (censoring[i] == 1) {
        target += survival_lpdf(interval_left[i] | shape[site[i]], scale[i], model_type);
      }
      // censoring 3 means interval censoring 
      else if (censoring[i] == 3) {
        target += log_diff_exp(survival_lcdf(interval_right[i] | shape[site[i]], scale[i], model_type),
          survival_lcdf(interval_left[i] | shape[site[i]], scale[i], model_type));
      }
    }
  }
}

generated quantities {
  array[J] vector[nc]   hazard_ratio; 
  
  if(nc > 0) {
    for (j in 1:J) {
      for(i in 1:nc) {
        hazard_ratio[j, i] = exp(-shape[j] * beta[j, i]);
      }
    }
  }
}
