library(tidyverse)





#' Calculates MDE for a given experiment, using total study proportions and N
#'  
#'  This function calculates the MDE for a hypothesis.
#'  
#'  We distinguish between a "sub"-experiment and the RCT. p_{t,c}_total describes 
#'  what proportion of individuals (as a fraction of entire RCT) used for 
#'  treatment or control in this "sub"-experiment hypothesis.
#' 
#'  
#'  
#' @param p_t_total total proportion of experiment in the treatment arm of this 
#'                  "sub"-experiment.
#' @param p_c_total total proportion of experiment in the control arm of this 
#'                  sub-experiment
#' @param N_total Total number of individuals in the entire RCT.
#' @param alpha Test size.
#' @param k Test power, usually 0.8.
#' @param rho Intra-cluster correlation.
#' @param m Average cluster size.
calculate_exp_mde = function(p_c_total, 
                             p_t_total, 
                             N_total = 100,
                             alpha = 0.025,
                             k = 0.8,
                             rho = 0,
                             m = 8){
    N_exp = N_total*(p_c_total + p_t_total)
    p_c = p_c_total / (p_c_total + p_t_total)
    p_t = p_t_total / (p_c_total + p_t_total)
    t_stat = qt(k, N_exp - 1) + qt(1 - alpha, N_exp - 1)
    icc_correction = sqrt(1 + rho*(m - 1))
    st_dev_outcome = 0.3
    mde = t_stat*icc_correction*st_dev_outcome*(1/sqrt(N_exp))*sqrt((1/p_c) + (1/p_t))
    return(mde)
}



calculate_MDEs = function(p_vector, N_total = 2250, alpha = 0.025, k = 0.8, rho = 0, m = 8){
        mde_1 = calculate_exp_mde(
            p_c_total = p_vector[1],
            p_t_total = p_vector[2],
            N_total = N_total,
            alpha = alpha,
            k = k,
            rho = rho,
            m = m)

        mde_2 = calculate_exp_mde(
            p_c_total = p_vector[2],
            p_t_total = p_vector[3],
            N_total = N_total,
            alpha = alpha,
            k = k,
            rho = rho,
            m = m)
        
        mde_3 = calculate_exp_mde(
            p_c_total = p_vector[1] + p_vector[2],
            p_t_total = p_vector[4],
            N_total = N_total,
            alpha = alpha,
            k = k,
            rho = rho,
            m = m
            )

        mde_4 = calculate_exp_mde(
            p_c_total = p_vector[3],
            p_t_total = p_vector[4],
            N_total = N_total,
            alpha = alpha,
            k = k,
            rho = rho,
            m = m)


        return(c(
            mde_1 = mde_1,
            mde_2 = mde_2,
            mde_3 = mde_3,
            mde_4 = mde_4
        ))

}



linear_utility_function = function(MDE_weights, MDE_vector){
    util = sum(MDE_weights*MDE_vector)
    return(util)

}







mde_utility = function(p_vector, N_total, alpha, k, rho, m,  utility_function){
    p_4 = 1 - sum(p_vector)
    p_vector = c(p_vector, p_4) 
    MDEs = calculate_MDEs(
        p_vector = p_vector, 
        N_total = N_total,
        alpha = alpha,
        k = k,
        rho = rho,
        m = m )
    util = utility_function(MDEs)
    return(util)
}


anon_equal_linear_function = function(MDEs) {
    linear_utility_function(rep(1/4, 4), MDEs)
} 

anon_equal_cd_function = function(MDEs) {
    util = prod(MDEs^(1/4))
}



find_prop_log = function(prop_weights, N_total, alpha, k, rho, m) {

    optim_res = optim(
        par = rep(1/4, 3),
        fn = function(x){mde_utility(
            p_vector = x, 
            N_total = N_total, 
            alpha = alpha,
            k = k,
            rho = rho,
            m = m,
            utility_function = function(x){prod(x^prop_weights)})}
    )
    optim_par = optim_res$par
    optim_par_4 = 1 - sum(optim_par)
    optim_par = c(optim_par, optim_par_4)
    optim_mde = calculate_MDEs(
        optim_par, 
        N_total = N_total, 
        alpha = alpha, 
        k = k, 
        rho = rho,
        m = m)
    if (optim_res$convergence == 0) {
        return(list(
            p_vector = optim_par, 
            mde_vector = optim_mde))
    } else {
        return(NULL)
    }
}
find_prop_linear = function(prop_weights, N_total, alpha, k, rho, m) {

    optim_res = optim(
        par = rep(1/4, 3),
        fn = function(x){mde_utility(
            p_vector = x, 
            N_total = N_total, 
            alpha = alpha,
            k = k,
            rho = rho,
            m = m,
            utility_function = function(x){linear_utility_function(prop_weights, x)})}
    )
    optim_par = optim_res$par
    optim_par_4 = 1 - sum(optim_par)
    optim_par = c(optim_par, optim_par_4)
    optim_mde = calculate_MDEs(
        optim_par, 
        N_total = N_total, 
        alpha = alpha, 
        k = k, 
        rho = rho,
        m = m)
    if (optim_res$convergence == 0) {
        return(list(
            p_vector = optim_par, 
            mde_vector = optim_mde))
    } else {
        return(NULL)
    }
}
find_prop_linear(
    prop_weights = rep(1/4, 4),
    N_total =  100,
    alpha = 0.025,
    k = 0.8,
    rho = 0.0,
    m = 8) 


mde_utility(rep(1/4, 4), N_total =  100,
    alpha = 0.025,
    k = 0.8,
    rho = 0.32,
    m = 8, utility_function = function(x){linear_utility_function(rep(1/4, 4), x)})



