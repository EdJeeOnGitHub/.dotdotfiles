se2 <- function(a,b,c,d) 
  sqrt(b/(a*(a+b)) + d/(c*(c+d)))
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
ci_upper = exp(log(rr) + 1.96*se2(a, b, c, d))

ci_lower
rr
ci_upper
