library(tidyverse)
library(haven)


# download this file from Dupas' website
# https://web.stanford.edu/~pdupas/MSW%20Public.zip
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


## Haushofer et al calculations
se2 <- function(a,b,c,d) 
  sqrt(b/(a*(a+b)) + d/(c*(c+d)))


se2(vals$a, vals$b, vals$c, vals$d)
n1 <- 1772/2
n2 <- 1772/2
p2 <- .1448
or <- 0.752
o2 <- p2/(1-p2)
o1 <- or*o2
p1 <- o1/(1+o1)
p1
rr <- p1/p2
rr

a <- round(p1*n1)
b <- n1 - a
c <- round(p2*n2)
d <- n2 - c
se2(a,b,c,d)

ci_lower = exp(log(rr) - 1.96*se2(a, b, c, d))
rr
ci_upper = exp(log(rr) + 1.96*se2(a, b, c, d))
