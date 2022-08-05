# run results.R first to get the study estimates
source("prepare_models.R")
# For the "linear" model that multiplies diarrhea reductions:
# source("simulations/check-multiplication-model.R")
bg_alt_models2 <- readRDS("analysis_output/stan/bg_alt_priors_8mar2022.rds")
# Student's T hypermean model is my top choice right now
bg_alt1 <- bg_alt_models2[["hypermean: Gaussian, 10%; hyperSD: Turner et al."]]
bg_alt2 <- bg_alt_models2[["hypermean: Student T, 10%; hyperSD: Turner et al."]]

library(dplyr)
set.seed(1990)

cea_v2 <- function(x, #odds ratio (row 1)
                p1, #<5y mortality rate (row 2) in untreated
                tkup, #takeup rate for the intervention (row 6)
                cost, #cost, row 8
                years = 5,
                result = "single",
                input = "or"
                ) {
  if(input == "or"){
    or <- exp(effect_draw(x, 1e06))
    o1 <- p1/(1-p1)
    o2 <- or*o1
    p2 <- o2/(o2+1)
    rr <- p2/p1
    x <- treatment_effect(x)[[1]]
  } else if(input == "rr") {
    rr <- x
  }
  
  
  # What proportion of population will avoid death when trt introduced?
  t_ma <- .59 #takeup in meta-analysis
  takeup_rate <- tkup / t_ma
  reduction <- takeup_rate*p1*(1-rr)
  # What is cost of the program, total, per person?
  total_cost <- years*cost
  
  daly_lost <- 81.25 - 2 #let's assume average age of death is 2
  #WW: 81.25 has been discussed with MK, but I'd like to change it later
  
  daly_reduction <- daly_lost*reduction
  tab <- list(
    or = round(mean(exp(x)), 3),
    rr = round(mean(rr), 2),
    p1 = round(100*p1, 1),
    t_ma = t_ma,
    tkup = tkup,
    reduction = round(mean(reduction), 4),
    daly_reduction = round(mean(daly_reduction), 2),
    cost = total_cost,
    cost_per_death = round(total_cost/mean(reduction)),
    cost_per_daly = round(total_cost/mean(daly_reduction))
  )
  
  if(result == "table") #output the rows of Table 2
    return(tab)
  else
    return(daly_reduction)
}

# ORs have huge right tails:
or_ppd_baggr   <- exp(effect_draw(bg_main, 1e06))
summary(or_ppd_baggr)

df_cea <- bind_rows(
  cea_v2(result = "table", bg_main,   .0692, 0.51, 9.1),
  cea_v2(result = "table", bg_alt1,   .0692, 0.51, 9.1),
  cea_v2(result = "table", bg_alt2,   .0692, 0.51, 9.1),
  # cea_v2(result = "table", rr_mm,        .0692, 0.51, 9.1, input = "rr"),
  cea_v2(result = "table", bg_main,   .0502, 0.32, 4.0),
  cea_v2(result = "table", bg_alt1,   .0502, 0.32, 4.0),
  cea_v2(result = "table", bg_alt2,   .0502, 0.32, 4.0)
  # cea_v2(result = "table", rr_mm,        .0502, 0.32, 4.0, input = "rr")
)

df_cea
df_cea %>% 
  mutate_all(as.character) %>% 
  t() %>% 
  write.csv("analysis_output/tables/table-cea-estimates.csv")

# "we find that cost-effectiveness threshold (1,838 USD per DALY) 
# is reached at 0.6% reduction in risk of under 5 mortality":
cea_v2(result = "table", input="rr", .994, .0692, 0.51, 9.1)$cost_per_daly

# Table 8 calculations 
reduction_calc<- function(x, #odds ratio (row 1)
                                   p1, #<5y mortality rate (row 2) in untreated
                                   tkup, #takeup rate for the intervention (row 6)
                                   input = "or"
){
  if(input == "or"){
    or <- x
    o1 <- p1/(1-p1)
    o2 <- or*o1
    p2 <- o2/(o2+1)
    rr <- p2/p1
  } else if(input == "rr") {
    rr <- x
  }
  
  # What proportion of population will avoid death when trt introduced?
  t_ma <- .59 #takeup in meta-analysis
  takeup_rate <- tkup / t_ma
  reduction <- takeup_rate*(1-rr)
  reduction = round(mean(reduction), 4)
  return( reduction)
}

# Col 3
no_access<-c(272.8, 223.2, 136.4, 69.6)
cost_coupons<-20
col_3<-no_access*cost_coupons

# Col 4
reduction<-reduction_calc(or_ppd_baggr, .0502, 0.32, input = "or")
mortality_rate<-c(2.739, 2.241, 1.369, 0.698)
saved_lives<-mortality_rate*reduction
saved_lives<-saved_lives*1000
