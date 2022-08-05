library(tidyverse)





#' Calculates MDE for a given experiment, using total study proportions and N
calculate_exp_mde = function(p_c_total, p_t_total, N_total = 100){
    N_exp = N_total*(p_c_total + p_t_total)
    p_c = p_c_total / (p_c_total + p_t_total)
    p_t = p_t_total / (p_c_total + p_t_total)

    mde = 0.3*2.8*(1/sqrt(N_exp))*sqrt((1/p_c) + (1/p_t))
    return(mde)
}



calculate_MDEs = function(p_vector, N_total = 2250){
        mde_1 = calculate_exp_mde(
            p_c_total = p_vector[1],
            p_t_total = p_vector[2],
            N_total = N_total)

        mde_2 = calculate_exp_mde(
            p_c_total = p_vector[2],
            p_t_total = p_vector[3],
            N_total = N_total)
        
        mde_3 = calculate_exp_mde(
            p_c_total = p_vector[1] + p_vector[2],
            p_t_total = p_vector[4],
            N_total = N_total)

        mde_4 = calculate_exp_mde(
            p_c_total = p_vector[3],
            p_t_total = p_vector[4],
            N_total = N_total)


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

test_MDEs = calculate_MDEs(rep(1/4, 4), N_total = 2250)

linear_utility_function(rep(1/4, 4), test_MDEs)

test_weights = rep(1/4, 4)


test_weights * test_MDEs[]] 
unlist(test_MDEs)





mde_utility = function(p_vector, N_total, utility_function){
    MDEs = calculate_MDEs(p_vector = p_vector, N_total = N_total)
    util = utility_function(MDEs)
    return(util)
}