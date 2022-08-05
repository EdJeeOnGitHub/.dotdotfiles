library(tidyverse)
library(broom)
df = mtcars %>%
    as_tibble()
df



mv_lm = df %>%
    lm(
        data = ., 
        formula = mpg ~ cyl + disp
    ) %>%
    tidy()
mv_lm



### FWL


fwl_df = df %>%
    mutate( 
        resid = resid(lm(cyl ~ disp, data = .))
    )
fwl_df

fwl_fit = fwl_df %>%
    lm( 
        data = ., 
        formula = mpg ~  resid
    ) 
fwl_lm = tidy(fwl_fit)


fwl_lm
mv_lm


fwl_df %>%
    mutate( 
        fitted = predict(fwl_fit)
    ) %>%
    ggplot(aes( 
        x = resid, 
        y = mpg 
    )) +
    geom_point() +
    geom_line(
        aes(x = resid, y = fitted), 
        linetype = "longdash", 
        colour = "pink", 
        size = 2
    ) +
    theme_bw()

