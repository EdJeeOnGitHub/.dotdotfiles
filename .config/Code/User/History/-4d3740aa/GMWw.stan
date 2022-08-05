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
    int<lower=1> J; // N sites
    int<lower=1> nc;            //number of covariates + 1
    int<lower=0> nsc;           //number of survival constraints
    array[N] int<lower=1,upper=J> site; // site index
    /* covariates */
    matrix[N, nc] X;     //design matrix, including intercept

    /* events */
    array[N] int  censoring; // 0: right censored, 1: no censoring, 2: left censored, 3: interval censored
    array[N] real interval_left; //times of intervals for interval-censored data
    array[N] real interval_right; //times of intervals for interval-censored data

    int model_type; // 1 => exponential, 2 => weibull, 3 => gompertz
}

parameters {
  array[J] vector[nc]    beta;     //values of log(HR)'s - see below
  vector[model_type ? 1 : 0] lshape_; // log(shape), disappear in exponential models
  vector[nc] mu_beta;
  vector<lower=0>[nc] sigma_beta;
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
  vector<lower=0>[model_type ? 1 : 0] shape;


  // Exponential
  if (model_type == 1) {
      // Fix shape to be 1 - a bit hacky
    shape = exp(lshape_*0);
    if(nc > 0) {
        for (j in 1:J) {
            for(i in 1:nc) {
                beta_transformed[j, i] = -1*beta[j, i]; // shape === 1
            }
        }
    }
  }
    // Weibull
  if (model_type == 2) {
    shape = exp(lshape_);
    if(nc > 0) {
        for (j in 1:J) {
            for(i in 1:nc) {
                beta_transformed[j, i] = -1*beta[j, i]/shape[1];
            }
        }
    }
  }


  // Gompertz we don't re-parametrise by divivding by shape
  if (model_type == 3) {
    for (j in 1:J) {
        for(i in 1:nc) {
            beta_transformed[j, i] = -1*beta[j, i];
        }
    }
  }

}

model {
    array[N] real scale;

    mu_beta ~ normal(0, 1);
    sigma_beta ~ normal(0, 1);

    for (j in 1:J) {
        beta[j, ] ~ normal(mu_beta, sigma_beta);
    }
    lshape_ ~ normal(3, 1);
    if(N > 0) {
    for(i in 1:N){
      scale[i] = exp(X[i,]*beta_transformed[site[i], ]);
      // censoring 0 means right-censored
      if(censoring[i] == 0) {
      target += survival_lccdf(interval_left[i] | shape[1], scale[i], model_type);
      } 
      // censoring 1 means no censoring
      else if (censoring[i] == 1) {
      target += survival_lpdf(interval_left[i] | shape[1], scale[i], model_type);
      }
      // censoring 3 means interval censoring 
      else if (censoring[i] == 3) {
      target += log_diff_exp(survival_lcdf(interval_right[i] | shape[1], scale[i], model_type),
        survival_lcdf(interval_left[i] | shape[1], scale[i], model_type));
      }
    }
  }
}
generated quantities {
   array[N] real HR_i;
   HR_i = exp(-shape*beta)
}
