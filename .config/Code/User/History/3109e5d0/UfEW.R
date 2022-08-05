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

# Calculate Peto Log Odds ratio (take exponent for OR)
calculate_peto_or = function(a, b, c, d, cont = 0.5){
    if ( a == 0 & c == 0) {
        a = a + cont
        b = b + cont
        c = c + cont 
        d = d + cont
    }
    n = a + b + c + d
    E = (a + b)*(a + c)/ n
    V = (a + b)*(c + d)*(a + c) * (b + d)/(n^2 *(n-1))
    O = a
    peto = (O - E) / V
    return(peto)
}


study_peto_manual = df_rare %>%
    rowwise() %>%
    mutate(
        or = calculate_peto_or(a = tcases, b = tnoncases, c = ccases, d = cnoncases)
    ) %>%
    select(trial_name, or) %>%
    mutate(type = "manual")

# Fixed Peto model
peto_m_fixed <- metabin(tcases, t_n, ccases, c_n,
                  incr = 0.5, #0.5 is the meta default but I wanted to make it clear
                  data = apply_cc(df_rare), #for Peto, apply correction to 0-events study
                                            #as metabin() won't do it automatically
                  method = "Peto", 
                  random = FALSE,
                  fixed = TRUE)
# Random peto model
peto_m_random <- metabin(tcases, t_n, ccases, c_n,
                  incr = 0.5, #0.5 is the meta default but I wanted to make it clear
                  data = apply_cc(df_rare), #for Peto, apply correction to 0-events study
                                            #as metabin() won't do it automatically
                  method = "Peto", 
                  random = T)
# Extract log odds estimates and CIs
study_peto_random = tibble(
    or = peto_m_random$TE,
    conf.low = peto_m_random$lower,
    conf.high = peto_m_random$upper,
    std.error = peto_m_random$seTE,
    trial_name = df_rare$trial_name
) %>%
    mutate(type = "replication", peto = "random")
# Extract log odds estimates and CIs for fixed model
study_peto_fixed = tibble(
    or = peto_m_fixed$TE,
    conf.low = peto_m_fixed$lower,
    conf.high = peto_m_fixed$upper,
    std.error = peto_m_fixed$seTE,
    trial_name = df_rare$trial_name
) %>%
    mutate(type = "replication", peto = "fixed")

load_old_or = FALSE
if (load_old_or == TRUE) {
    old_or = read_csv("Publication_bias/Applications/water/OR_Estimates.csv",
                    col_names = c("or", "std.error", "row_id") )
    old_or_labels = read_csv("Publication_bias/Applications/water/OR_Labels.csv",
                            col_names = c("trial_name", "name_two_why"),
                            skip = 1 
        ) %>%
        select(-name_two_why)
    old_or = bind_cols(
        old_or,
        old_or_labels
    ) 

    comp_df = bind_rows(
        study_peto_random,
        study_peto_fixed
    ) %>%
        left_join(old_or %>% rename(old_or = or, old_std.error = std.error), by = "trial_name")

    comp_df %>%
        select(or, 
            trial_name, 
            type, 
            peto, 
            old_or, 
            old_std.error) %>%
        ggplot(aes( 
            x = old_or, 
            y = or
        )) +
        geom_point() +
        geom_abline() +
        facet_wrap(~peto)

    comp_df %>%
        select(or,
            trial_name,
            type, 
            peto) %>%
        spread(peto, or) %>%
        ggplot(aes(
            x = fixed, 
            y = random)) +
        geom_point() +
        geom_abline()

}


peto_or_check = map2(study_peto_fixed$or, study_peto_manual$or, ~all.equal(.x , .y, tolerance = 1e-2)) %>%
    unlist() %>%
    all()

if (peto_or_check == FALSE) {
    stop("Peto Log Odds Ratio doesn't match metabin")
}

study_peto_fixed %>%
    select(or, std.error) %>%
    mutate(row = 1:n()) %>%
    write_csv("publication_bias/Applications/water/or-estimates.csv", col_names = FALSE)


# for whatever reason these are the colnames in the original so for now we 
# stick with them until I figure out if we can remove them
study_peto_fixed %>%
    select(trial_name) %>%
    mutate(
        "Name (year)" = trial_name,
        "Shorted Name (year)" = trial_name
   )   %>%
   select(-trial_name) %>%
    write_csv("publication_bias/Applications/water/or-labels.csv")













# Weights
weights<-round(stats::weights(peto_m_random)$p.random,2)
peto_weights <- df_rare %>% 
  select(trial_name, weeks) %>%
  rename(week = weeks) %>%
  tibble(weights) 

# Peto OR estimates in each study
Individual_estimates_peto<-
  c(round(exp(peto_m_random$TE),3), 
    round(exp(peto_m_random$lower), 3), 
    round(exp(peto_m_random$upper), 3))%>%
  matrix(ncol=3)%>%
  as.data.frame()%>%
  rename(mean=V1, lower=V2, upper=V3)%>%
  mutate(trial_name = df_rare$trial_name,
         intervention = df_rare$intervention) %>%
  mutate(CI = interval2(lower, upper)) %>%
  mutate(CI = ifelse(CI=="(NA,NA)", NA, CI)) %>%
  arrange(trial_name)


# Peto RE estimates overall
Overall_estimate_peto<-overall_est_peto(peto_m_random)
Overall_estimate_peto$CI<-paste0("(", Overall_estimate_peto$lower, ",", Overall_estimate_peto$upper, ")")


# Bayesian model -----

# no estimation done here, we just load in models
bg_none <- readRDS("analysis_output/stan/bg_nopool_jul2022.rds")
bg_loo <- readRDS("analysis_output/stan/bg_loo_jul2022.rds")
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
b1 <- readRDS("analysis_output/stan/bg_subsets_jul2022.rds")

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
