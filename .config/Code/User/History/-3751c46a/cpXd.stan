/* 

    Hierarchical Gompertz Stan Code

  Stan code for interval-censored analyses using gompertz survival function 
  follows prior structure same as in non-interval method
  assumes all patients have been interval- or right-censored 
*/
functions {
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

}

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

}

parameters {
  array[J] vector[nc]    beta;     //values of log(HR)'s - see below
  // real lshape_; //log(shape)   
  real<lower=0> shape;
  vector[nc] mu_beta;
  vector<lower=0>[nc] sigma_beta;
}

transformed parameters {
  array[J] vector[nc]    beta_transformed;
  if(nc > 0) {
    for (j in 1:J) {
        for(i in 1:nc) {
            beta_transformed[j, i] = -1*beta[j, i];
        }
    }
   }

}

model {
    array[N] real scale;
    shape ~ normal(1, 5); 
    mu_beta ~ normal(0, 1);
    sigma_beta ~ normal(0, 1);

    for (j in 1:J) {
        beta[j, ] ~ normal(mu_beta, sigma_beta);
    }


    if(N > 0) {
    for(i in 1:N){
        scale[i] = exp(X[i,]*beta_transformed[site[i], ]);
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

