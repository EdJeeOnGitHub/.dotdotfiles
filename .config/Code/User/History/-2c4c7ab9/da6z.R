library(tidyverse)



test_arm_proportions = c(0.1, 0.2, 0.3, 0.1)
hypothesis_proportions = function(arm_proportions){
    T_1 = arm_proportions[1]
    T_2 = arm_proportions[2]
    T_3 = arm_proportions[3]
    T_4 = arm_proportions[4]

    H_1_A = T_1
    H_2_A = T_2
    H_3_A = T_1 + T_2
    H_4_A = T_3


    H_1_B = T_2
    H_2_B = T_3
    H_3_B = T_4 
    H_4_B = T_4

    T_H_df = tibble(
        T_1,
        T_2,
        T_3,
        T_4, 

        H_1_A,
        H_2_A,
        H_3_A,
        H_4_A,

        H_1_B,
        H_2_B,
        H_3_B,
        H_4_B
    )
    return(T_H_df)
}


hypothesis_proportions(test_arm_proportions)


#' Calculates MDE for a given experiment, using total study proportions and N
calculate_exp_mde = function(p_c_total, p_t_total, N_total = 100){
    N_exp = N_total*(p_c + p_t)
    p_c = p_c_total / (p_c_total + p_t_total)
    p_t = p_t_total / (p_c_total + p_t_total)

    mde = 0.3*2.8*(1/sqrt(N_exp))*sqrt((1/p_c) + (1/p_t))
    return(mde)
}



calculate_MDEs = function(p_vector, N_total = 2250){
        mde_1 = ca

}