
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
    array[N] int  censoring;
    array[N] real interval_left; //times of intervals for interval-censored data
    array[N] real interval_right; //times of intervals for interval-censored data

    // /* priors */
}

parameters {
  array[J] vector[nc]    beta;     //values of log(HR)'s - see below
  real lshape_;   //log(shape)
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
  real<lower=0> shape;
  shape = exp(lshape_);
  if(nc > 0) {
    for (j in 1:J) {
        for(i in 1:nc) {
            beta_transformed[j, i] = -1*beta[j, i]/shape;
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
        //censoring 1 means non-interval censoring
        if(censoring[i] == 1) {
        target += weibull_lccdf(interval_left[i] | shape, scale[i]);
        } 
        //censoring 0 means interval censored patient
        else {
        target += log_diff_exp(weibull_lcdf(interval_right[i] | shape, scale[i]),
                                weibull_lcdf(interval_left[i]  | shape, scale[i]));
        }
    }
    }
}
