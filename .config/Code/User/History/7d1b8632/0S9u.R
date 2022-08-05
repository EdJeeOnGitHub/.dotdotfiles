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
        treatment_arm = sample(1:J, N, pr_arm, replace = TRUE)
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


simulate_data = function(seed = 1234,
                         hyper_params){ 

    gen_data = meta_gen_data_function(
        N = hyper_params$N,
        J = hyper_params$J
    )
    gen_modeled_data = meta_gen_modeled_data_function(dgp = hyper_params$dgp)
    gen_params = meta_gen_params(TEs = hyper_params$TEs, baseline_mean = hyper_params$baseline_mean)
    data = gen_data(seed)
    params = gen_params(seed,
                        data)
    modeled_data = gen_modeled_data(seed,
                                    data,
                                    params)
    sim_data_list = c(
        data,
        params,
        modeled_data
    )
    return(sim_data_list)
}



sim_draw(seed = 1, hyper_params = list(N = 100, J = 3, dgp = "probit", TEs = c(0, 1, 2), baseline_mean = 0))

