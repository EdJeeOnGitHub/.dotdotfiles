library(tidyverse)




pull_lever = function(action){
    if (!(action %in% c(1, 2, 3))){
        stop("Must choose one of 1, 2, 3.")
    }
    
    bandit_rewards = c(
        rnorm(1, 3, 1.5),
        rnorm(1, 3.5, 1),
        rbernoulli(1, 0.5) + 1
    )


    return(
        list(reward = bandit_rewards[action],
             action = action,
             best_arm_reward = max(bandit_rewards)))
}





result_df = tribble(
    ~time, ~action, ~reward, ~best_arm_reward,
    0, 1, NA, 0,
    0, 2, NA, 0,
    0, 3, NA, 0
)
t = 1




action_func = function(action, type = "reward"){

t <<- t + 1
result = pull_lever(action)
print(paste0("Result: ", round(result$reward, 3)))
result_df <<- add_row(
    result_df,
    time = t,
    action = result$action,
    reward = result$reward,
    best_arm_reward = result$best_arm_reward
) %>% filter(!is.na(reward))
if (type == "reward") {

p = result_df %>%
    group_by(action) %>%
    mutate(mean_reward = cummean(reward), 
           sum_optimal = cumsum(best_arm_reward),
           sum_realised = cumsum(reward), 
           reward = reward,
           regret = sum_optimal - sum_realised) %>%
    gather(variable, value, -time, -action) %>%
    filter(variable %in% c("reward")) %>%
    ggplot(aes(
        x = time, 
        y = value, 
        colour = factor(action))) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(colour = "Action",
         title = "Reward Per Bandit",
         subtitle = paste0("Current Reward: ", round(result_df$reward[length(result_df$reward)], 2) ,", Total Reward: ", round(sum(result_df$reward), digits = 2)))

}
if (type == "mean"){

p = result_df %>%
    group_by(action) %>%
    mutate(mean_reward = cummean(reward), 
           sum_optimal = cumsum(best_arm_reward),
           sum_realised = cumsum(reward), 
           reward = reward,
           regret = sum_optimal - sum_realised) %>%
    gather(variable, value, -time, -action) %>%
    filter(variable %in% c("mean_reward")) %>%
    ggplot(aes(
        x = time, 
        y = value, 
        colour = factor(action))) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(colour = "Action",
         title = "Mean Reward Per Bandit",
         subtitle = paste0("Current Reward: ", round(result_df$reward[length(result_df$reward)], 2) ,", Total Reward: ", round(sum(result_df$reward), digits = 2)))
    
}
return(p)
}


#########
action = sample(1:3, size = 1) 
action = 3
action_func(action, type = "reward") # type = "mean"

result_df
######################
result_df %>%
    group_by(action) %>%
    mutate(mean_reward = cummean(reward), 
           sum_optimal = cumsum(best_arm_reward),
           sum_realised = cumsum(reward), 
           regret = sum_optimal - sum_realised, 
           mean_regret = regret/time) %>%
    ggplot(aes(
        x = time, 
        y = mean_regret)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
         title = "Mean Regret Over Time",
         subtitle = paste0("Total Reward: ", round(sum(result_df$reward), digits = 2)))
    

result_df %>% mutate()
