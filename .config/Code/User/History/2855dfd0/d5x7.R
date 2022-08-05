#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                        Water meta-analysis 
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Clear memory and set seed
rm(list=ls())
set.seed(24)

# Packages for analytics
library(baggr)
options(mc.cores = 8)
library(meta)

# Load in data
source("data_prep.R")

# Bayesian models take time to run (think 1-2 hrs for all)
# You can re-generate them by sourcing an external script, 
# otherwise we just load them in from saved objects later
# source("bayes_v2.R")

# Helper functions for processing outputs (e.g. generating data frames with results)
source("helpers.R")



#------------------------------------------------------------------------------#
# Odds ratio for all studies (main specifications) -----
#------------------------------------------------------------------------------#

# Peto model -----
peto_m <- metabin(tcases, t_n, ccases, c_n,
                  incr = 0.5, #0.5 is the meta default but I wanted to make it clear
                  data = apply_cc(df_rare), #for Peto, apply correction to 0-events study
                                            #as metabin() won't do it automatically
                  method = "Peto", random = T)

# Weights
weights<-round(stats::weights(peto_m)$p.random,2)
peto_weights <- df_rare %>% 
  select(trial_name, weeks) %>%
  rename(week = weeks) %>%
  tibble(weights) 

# Peto OR estimates in each study
Individual_estimates_peto<-
  c(round(exp(peto_m$TE),3), 
    round(exp(peto_m$lower), 3), 
    round(exp(peto_m$upper), 3))%>%
  matrix(ncol=3)%>%
  as.data.frame()%>%
  rename(mean=V1, lower=V2, upper=V3)%>%
  mutate(trial_name = df_rare$trial_name,
         intervention = df_rare$intervention) %>%
  mutate(CI = interval2(lower, upper)) %>%
  mutate(CI = ifelse(CI=="(NA,NA)", NA, CI)) %>%
  arrange(trial_name)


# Peto RE estimates overall
Overall_estimate_peto<-overall_est_peto(peto_m)
Overall_estimate_peto$CI<-paste0("(", Overall_estimate_peto$lower, ",", Overall_estimate_peto$upper, ")")


# Bayesian model -----
bg_none <- readRDS("analysis_output/stan/bg_none.rds")

# no estimation done here, we just load in models
bg_none <- readRDS("analysis_output/stan/bg_none.rds")
bg_loo <- readRDS("analysis_output/stan/bg_loo.rds")
bg_main <- bg_loo$full_model

# helper function that converts default baggr group_effects
# into the kind of data.frame that older code in this project used
ctdf_helper <- function(x) {
  left_join(
    select(df_rare, trial_name, intervention),
    #this is the only important line here:
    group_effects(x, s=T, transform = exp)[,,1] %>% 
      as.data.frame() %>%
      rownames_to_column() %>% rename(trial_name = rowname),
    by = "trial_name"
  ) %>%
    transmute(trial_name, intervention, lower=lci, mean=mean, upper=uci)
}

study_est_bayes <- ctdf_helper(bg_main)
study_est_bayes_no_pooling <- ctdf_helper(bg_none)
overall_est_bayes <- oeb(bg_main)

bayes_weights<-cbind(round((weights(bg_main)[2,,1])*100,1), #row 2 is mean
                     bg_main$summary_data[,1])%>%
  as.data.frame()

#------------------------------------------------------------------------------#
#  Odds ratio for subset of studies (models for sensitivity analyses) -----
#------------------------------------------------------------------------------#

# Peto -----

# List of model summaries
m1<-list() 

# List of mean effects, lower and upper 
peto_estimates_sub<-matrix(NA, ncol=4, nrow = length(df))
colnames(peto_estimates_sub)<-c("mean", "lower", "upper", "p-value")

# --- looping over sets of data to get subset specific odds --- #

for (i in 1:length(df)) {
  m1[[i]] <- metabin(tcases, t_n, ccases, c_n,
                     data = apply_cc(df[[i]]), 
                     method = "Peto",random = T)
  ## Peto RE
  peto_estimates_sub[i, 1]<-round(exp(m1[[i]]$TE.random),3)
  peto_estimates_sub[i, 2]<-round(exp(m1[[i]]$lower.random),3)
  peto_estimates_sub[i, 3]<-round(exp(m1[[i]]$upper.random),3)
  # p- values
  peto_estimates_sub[i, 4]<-round(m1[[i]]$pval.random,3)
}


# Bayes -----

# List of mean effects, lower and upper 
b1 <- readRDS("analysis_output/stan/bg_subsets.rds")

bayes_estimates_sub<-matrix(NA, ncol=3, nrow = length(df))
colnames(bayes_estimates_sub)<-c("mean", "lower", "upper")
# Bayesian
for (i in  c(1, 4:length(df))){
  overall_estimate<-baggr_compare(b1[[i]], transform=exp)
  ## Overall estimates
  bayes_estimates_sub[i, 1]<-round(overall_estimate$mean_trt[2],3)
  bayes_estimates_sub[i, 2]<-round(overall_estimate$mean_trt[1],3)
  bayes_estimates_sub[i, 3]<-round(overall_estimate$mean_trt[3],3)
}

# Leave one study out analysis (used in Table S5)
S5_bayes <- data.frame(
    excluded_study = df_rare$trial_name, 
    do.call(rbind, lapply(bg_loo$models, oeb)) %>% 
      as.data.frame())


# Summarise data (for publication) -----
# This would work best in an .Rmd format, but it's late in the project

print("Full dataset characteristics (see in code):")
or_dt <- df_rare %>% select(ccases, cnoncases, tcases, tnoncases, c_n, t_n) %>% 
  apply(2,sum)
print(or_dt)

or_dt[["tcases"]]/or_dt[["tnoncases"]]
or_dt[["ccases"]]/or_dt[["cnoncases"]]
or_dt[["tcases"]]*or_dt[["cnoncases"]]/or_dt[["ccases"]]/or_dt[["tnoncases"]]

df_rare %>% mutate(ccases_per_yr = 52*ccases/weeks) %>% 
  summarise(sum(ccases_per_yr)/sum(c_n))

# Table with weights, compliance, prevalence --- for comparisons
print("Weighted year, compliance and prevalence")
df_rare %>% select(trial_name, prevalence, compliance, year, Obs, weeks) %>% 
  arrange(trial_name) %>% 
  mutate(person_wks = Obs*weeks) %>%
  mutate(weights1 = weights(bg_main)[2,,1]) %>%
  mutate(weights2 = person_wks/sum(person_wks)) %>%
  mutate(compliance = ifelse(is.na(compliance), mean(compliance, na.rm=T), compliance)) %>%
  mutate(prevalence1 = prevalence*weights1, 
         prevalence2 = prevalence*weights2, 
         year1 = year*weights1,
         compliance1 = compliance*weights1,
         compliance2 = compliance*weights2) %>% 
  select(prevalence1, prevalence2, compliance1, compliance2) %>%
  summarise_if(is.numeric, sum) %>% 
  print()

# Posterior predictive dist for Bayes
print("PPD for main Bayesian model:")
mint(exp(effect_draw(bg_main, 1e06))) %>% round(2)

print("RR implied by the Bayesian OR, at 5.02% 5-year mortality:")
