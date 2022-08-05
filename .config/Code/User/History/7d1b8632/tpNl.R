library(tidyverse)


#' Generate "Data" for simulations
#' 
#' Here data really means hyperparameters such as number of observations, 
#' number of covariates,#' 
#' @param N Number of total observations
meta_gen_data_function = function(N = 1000, J = 3){
    function(seed) {
        set.seed(seed + 1e6)
        N = N
        J = J
        pr_arm = rep(1/J, J)
        treatment_arm = sample(1:J, pr_arm)
        return(list(
            N = N,
            J = J,
            treatment_arm = treatment_arm
        ))
    }
}



#' Generate parameters 
#' 
#' These are the parameters we want to estimate.
meta_gen_params = function(TEs, baseline_mean){
    function(seed,
             data){

        set.seed(seed + 2e6)

        return(list(
            TEs = TEs,
            baseline_mean = baseline_mean
        ))
    
    }

}



#' Generate Modelled Data
#'
#'
#' Using hyper parameters from `gen_data` and parameters from `gen_params`,
#' create our modelled data. That is, create binary outcome data according to the generative model. 
meta_gen_modeled_data_function = function(dgp = "logit"){
    function(seed,
             data,
             params) {
        set.seed(seed + 3e6)

    
        TEs = params$TEs
        treatment_arm = data$treatment_arm
        mu_vec = params$baseline_mean + TEs[treatment_arm]

        if (dgp == "probit") {
            p_vec = pnorm(mu_vec)
        } 
        if (dgp == "logit") {
            p_vec = exp(-mu_vec)/(1 + exp(-mu_vec))
        }

        y = rbinom(n = data$N, size = 1, prob = p_vec) 

        return(list(
            y = y
        ))
    }

}