library(tidyverse)
library(haven)



df = read_dta("data/wolf-additions/MSW Public/7_outputs/analysis/child_analysis.dta")


vals = df %>%
    select( 
        childdiarrhea, wash
    ) %>%
    na.omit() %>% # diarrhea status not listed
    group_by(childdiarrhea, wash) %>%
    summarise(n = n()) %>%
    pull(n) %>%
    as.list()

names(vals) = c("d", "b", "c", "a")



RR = function(a, b, c, d){(a/(a + b)) / (c/(c + d))}
se_lnRR = function(a, b, c, d){
    sqrt((1/a) + (1/c) - 1/(a+b) - 1/(c+d))
}
rr = RR(vals$a, vals$b, vals$c, vals$d)
se_lnrr = se_lnRR(vals$a, vals$b, vals$c, vals$d)


ci_lower = exp(log(rr) - 1.96*se_lnrr)
ci_upper = exp(log(rr) + 1.96*se_lnrr)


ci_lower
rr
ci_upper