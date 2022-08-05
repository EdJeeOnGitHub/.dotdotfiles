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
    p_4 = 1 - sum(p_vector)
    p_vector = c(p_vector, p_4) 
    MDEs = calculate_MDEs(p_vector = p_vector, N_total = N_total)
    util = utility_function(MDEs)
    return(util)
}


anon_equal_linear_function = function(MDEs) {
    linear_utility_function(rep(1/4, 4), MDEs)
} 

anon_equal_cd_function = function(MDEs) {
    util = prod(MDEs^(1/4))
}


test_optim = optim(
    par = rep(1/4, 3),
    fn = function(x){mde_utility(
        p_vector = x, 
        N_total = 2250, 
        utility_function = anon_equal_linear_function)}
)


test_p_4 = 1 - sum(test_optim$par)
test_par = c(test_optim$par, test_p_4)
test_par
sum(test_par)


cd_optim = optim(
    par = rep(1/4, 3),
    fn = function(x){mde_utility(
        p_vector = x, 
        N_total = 2250, 
        utility_function = anon_equal_cd_function)}
)

cd_optim_p_4 = 1 - sum(cd_optim$par)
cd_optim_par = c(cd_optim$par, cd_optim_p_4)

cd_optim_par

props = expand.grid(
    p_1 = seq(from = 0, to = 1, length.out = 5),
    p_2 = seq(from = 0, to = 1, length.out = 5),
    p_3 = seq(from = 0, to = 1, length.out = 5)
) %>% 
    as_tibble() %>% 
    mutate(p_4 = 1 - p_1 - p_2 - p_3)
props

mde_df = props %>%
    rowwise() %>%
    mutate(mdes = list(calculate_MDEs(p_vector = c(p_1, p_2, p_3, p_4), N_total = 2250)))
mde_df %>%
    unnest(mdes) %>%
    ggplot(aes(x = p_1, y = mdes)) +
    geom_point()
