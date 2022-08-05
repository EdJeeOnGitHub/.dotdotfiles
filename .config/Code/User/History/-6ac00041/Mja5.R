library(tidyverse)


N = 10000
df = tibble(
    x_1 = rnorm(N),
    x_2 = rlnorm(N)
)


df_long = df %>%
    gather(variable, value)

df_long %>%
    group_by(variable) %>%
    mutate(mean = mean(value), median = median(value)) %>%
    ggplot(aes( 
        x = value,
        fill = variable
    )) +
    geom_histogram(bins = 100) +
    facet_wrap(~variable, scales = "free", ncol = 1) +
    geom_vline(aes(xintercept = mean)) + 
    geom_vline(aes(xintercept = median), linetype = "longdash")
