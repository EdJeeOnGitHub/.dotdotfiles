/* 
  Stan code for interval-censored analyses using Weibull survival function 
  follows prior structure same as in non-interval method
  assumes all patients have been interval- or right-censored 
*/
data {
  int<lower=0> N;             //number of datapoints
  int<lower=1> J;   // number of groups

  int<lower=1> nc;            //number of covariates + 1
  int<lower=0> nsc;           //number of survival constraints
  
  /* covariates */
  matrix[N, nc] X;     //design matrix, including intercept
  int<lower=1,upper=J> site[N]; // site 1-J index
  /* events */
  int  censoring[N];
  real interval_left[N]; //times of intervals for interval-censored data
  real interval_right[N]; //times of intervals for interval-censored data
  
  /* priors */
  // real          beta_mean[nc];  //prior mean
  // real<lower=0> beta_sigma[nc]; //prior variance
}

parameters {
  real lshape_;   //log(shape) - fixing this across sites for now
  // hierarchical stuff
  array[J] vector[nc] beta; //values of log(HR)'s - see below J x nc
  corr_matrix[nc] Omega; // prior correlation
  vector<lower=0>[nc] tau; // prior scale
  vector[nc] beta_mu;

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
    array[J] vector[nc]   beta_transformed;
  real shape;
  shape = exp(lshape_);
  if(nc > 0) {
    #TODO: Vectorise this
    for(j in 1:J){
        for (coef in 1:nc){
            beta_transformed[j, coef] = -1*beta[j, coef]/shape;
        }
    }
  }
}

model {
    real scale[N];
    
    tau ~ normal(0, 1);
    Omega ~ lkj_corr(2);
  
    lshape_ ~ normal(2, 0.5);
    beta_mu ~ normal(0, 1);
    beta ~ multi_normal(beta_mu, quad_form_diag(Omega, tau));

  if(N > 0) {
    for(i in 1:N){
      scale[i] = exp(X[i,]*beta_transformed[site[i], ]);
      print("beta");
      print(beta);
      print("bt");
      print(beta_transformed);
      print("sc");
      print(scale);

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
