source("prepare_models.R")

#------------------------------------------------------------------------------#
# Figure (Forest plots)
#------------------------------------------------------------------------------#
library(forestplot)

# 3A: Peto FP -----
chlori_studies_peto <- filter(Individual_estimates_peto, intervention == "Chlorination")
filter_studies_peto <- filter(Individual_estimates_peto, intervention == "Filtration")
spring_studies_peto <- filter(Individual_estimates_peto, intervention == "Spring protection")

output <- data.frame(
  mean  = c(NA, NA, NA, 
            pull(chlori_studies_peto, mean), 
            peto_estimates_sub[1,1],
            NA, NA, 
            pull(filter_studies_peto, mean), 
            peto_estimates_sub[2,1],
            NA, NA, 
            pull(spring_studies_peto, mean), 
            peto_estimates_sub[3,1],
            NA, Overall_estimate_peto[["mean"]]), 
  lower  = c(NA, NA, NA, 
             pull(chlori_studies_peto, lower), 
             peto_estimates_sub[1,2],
             NA, NA, 
             pull(filter_studies_peto, lower), 
             peto_estimates_sub[2,2],
             NA, NA, 
             pull(spring_studies_peto, lower), 
             peto_estimates_sub[3,2],
             NA, Overall_estimate_peto[["lower"]]), 
  upper = c(NA, NA, NA, 
            pull(chlori_studies_peto, upper), 
            peto_estimates_sub[1,3],
            NA, NA, 
            pull(filter_studies_peto, upper),
            peto_estimates_sub[2,3],
            NA, NA, 
            pull(spring_studies_peto, upper), 
            peto_estimates_sub[2,3],
            NA, Overall_estimate_peto[["upper"]])
)


tabletext<-cbind(
  c("Study", "",  c("Chlorine", Individual_estimates_peto$trial_name[which(Individual_estimates_peto$intervention %in% "Chlorination")]), "Sub-group estimate", NA,
    c("Filtration", Individual_estimates_peto$trial_name[which(Individual_estimates_peto$intervention %in% "Filtration")]), "Sub-group estimate", NA, 
    c("Spring protection", df$df_spring$trial_name), "Sub-group estimate", NA, "Overall estimate"),
  c("Peto OR", NA, NA, as.character(Individual_estimates_peto$mean[which(Individual_estimates_peto$intervention %in% "Chlorination")]),as.character(peto_estimates_sub[1,1]),
    NA, NA, as.character(Individual_estimates_peto$mean[which(Individual_estimates_peto$intervention %in% "Filtration")]), as.character(peto_estimates_sub[2,1]),
    NA, NA, as.character(Individual_estimates_peto$mean[which(Individual_estimates_peto$intervention %in% "Spring protection")]), as.character(peto_estimates_sub[3,1]), 
    NA, as.character(Overall_estimate_peto$mean)),
  c("CI 95%", NA, NA, Individual_estimates_peto$CI[which(Individual_estimates_peto$intervention %in% "Chlorination")],
    paste0("(", as.character(peto_estimates_sub[1,2]), ",", as.character(peto_estimates_sub[1,3]), ")"), 
    NA, NA, Individual_estimates_peto$CI[which(Individual_estimates_peto$intervention %in% "Filtration")], 
    paste0("(", as.character(peto_estimates_sub[2,2]), ",", as.character(peto_estimates_sub[2,3]), ")"), 
    NA, NA, Individual_estimates_peto$CI[which(Individual_estimates_peto$intervention %in% "Spring protection")], 
    paste0("(", as.character(peto_estimates_sub[3,2]), ",", as.character(peto_estimates_sub[3,3]), ")"), 
    NA, Overall_estimate_peto$CI)
)

png(file = "analysis_output/figures/peto-forest.png", width = 2400, height = 2000, res = 300)

forestplot(tabletext, output, new_page = TRUE,
           is.summary=c(T,T,T,rep(FALSE,length(which(Individual_estimates_peto$intervention %in% "Chlorination"))),
                        T,T,T,F,F,T,T, T, F,T, T, T),
           clip=c(0.062,4), 
           xlog=T, 
           col=fpColors(box="black",line="black", summary="black"),
           txt_gp = fpTxtGp(cex=1),
           hrzl_lines=list("2" = gpar(lwd=1, columns=1:3)))      

dev.off()



# 3B: Bayes FP -----
study_est_bayes_with_ci_nopool <- study_est_bayes_no_pooling %>%
  mutate(CI = interval2(lower, upper)) %>%
  mutate(CI = ifelse(CI=="(NA,NA)", "NA", CI)) %>%
  arrange(trial_name)
chlori_studies <- filter(study_est_bayes_with_ci_nopool, intervention == "Chlorination") 
filter_studies <- filter(study_est_bayes_with_ci_nopool, intervention == "Filtration")
spring_studies <- filter(study_est_bayes_with_ci_nopool, intervention == "Spring protection")

tabletext<-cbind(
  c("Study", "",  
    c("Chlorine",          pull(chlori_studies, trial_name)), 
    "Sub-group estimate", 
    NA,
    c("Filtration",        pull(filter_studies, trial_name)), 
    NA, 
    c("Spring protection", pull(spring_studies, trial_name)), 
    NA, 
    "Overall estimate"),
  c("Bayes OR", 
    NA, NA, 
    acr(pull(chlori_studies, mean)), 
    acr(bayes_estimates_sub[1, "mean"]),
    NA, NA, 
    acr(pull(filter_studies, mean)),
    NA, NA, 
    acr(pull(spring_studies, mean)),  
    NA, 
    acr(overall_est_bayes[["mean"]])),
  c("CrI 95%", 
    NA, NA, 
    pull(chlori_studies, CI), 
    interval2(bayes_estimates_sub[1, "lower"], bayes_estimates_sub[1, "upper"]), 
    NA, NA, 
    pull(filter_studies, CI), 
    NA, NA, 
    pull(spring_studies, CI), 
    NA, 
    interval2(overall_est_bayes[["lower"]], overall_est_bayes[["upper"]]))
)

tablenum <- data.frame(
  mean  = c(NA, NA, NA, 
            pull(chlori_studies, mean), 
            bayes_estimates_sub[1,1],
            NA, NA, 
            pull(filter_studies, mean), 
            NA, NA, 
            pull(spring_studies, mean), 
            NA, overall_est_bayes[["mean"]]), 
  lower  = c(NA, NA, NA, 
             pull(chlori_studies, lower), 
             bayes_estimates_sub[1,2],
             NA, NA, 
             pull(filter_studies, lower), 
             NA, NA, 
             pull(spring_studies, lower), 
             NA, overall_est_bayes[["lower"]]), 
  upper = c(NA, NA, NA, 
            pull(chlori_studies, upper), 
            bayes_estimates_sub[1,3],
            NA, NA, 
            pull(filter_studies, upper), 
            NA, NA, 
            pull(spring_studies, upper), 
            NA, overall_est_bayes[["upper"]])
)

png(file = "analysis_output/figures/bayes-forest.png", 
    width = 2400, height = 2000, res = 300)
forestplot(tabletext, tablenum, new_page = TRUE,
           is.summary=c(T,T,T,rep(F,nrow(chlori_studies)),T,T,T,F,F,T,T,F,T,T,T,T,F,T),
           clip=c(0.062,4), 
           xlog=T, 
           col=fpColors(box="black",line="black", summary="black"),
           hrzl_lines=list("2" = gpar(lwd=1, columns=1:3)))   
dev.off()



#------------------------------------------------------------------------------#
# Table Summary Estimates of Peto and Bayes Mortality
#------------------------------------------------------------------------------#

peto_estimates_sub<-peto_estimates_sub%>%
  cbind(CI=paste0("(", peto_estimates_sub[,"lower"], "," , peto_estimates_sub[,"upper"], ")"))

bayes_estimates_sub<-bayes_estimates_sub%>%
  cbind(CI=paste0("(", bayes_estimates_sub[,"lower"], "," , bayes_estimates_sub[,"upper"], ")"))

n_chlorination<-nrow(df_rare[which(df_rare$intervention %in% "Chlorination"),])
n_all<-nrow(df_rare)

# Table

peto_bayes_summ_mort_df <-Overall_estimate_peto[,c(1,4)]%>% # overall peto estimates
  rbind(cbind(mean=round(overall_est_bayes[2],3), # Overall bayes estimate
        CI=paste0("(", round(overall_est_bayes[1],3), "," ,
                  round(overall_est_bayes[3],3), ")")))%>%
  rbind(peto_estimates_sub[1, c(1,5)])%>%
  rbind(bayes_estimates_sub[1, c(1,4)])%>%
  t()%>%
  rbind(Interventions = c("All", "All", "Chlorine", "Chlorine"))%>%
  rbind(Studies=c(n_all, n_all, n_chlorination, n_chlorination))

colnames(peto_bayes_summ_mort_df)<-c("Mean Peto OR", "Mean bayesian OR", "Mean Peto OR", "Mean Bayesian OR")
rownames(peto_bayes_summ_mort_df)<-c("ITT effect on child mortality", "CI 95%", "Interventions", "Studies")
write.csv(peto_bayes_summ_mort_df, "analysis_output/tables/peto-bayes-summary-mortality.csv")

#------------------------------------------------------------------------------#
# OR estimates for subset of 5 studies (ww: but actually only 4 selected?)
#------------------------------------------------------------------------------#


# using IV approach
df_tableS4_iv <- df$df_tableS4%>%
  filter(!trial_name %in% "Luby et al., 2006")

iv <- metabin(tcases, t_n, ccases, c_n,
              data = df_tableS4_iv, method = "Inverse", sm= "OR", random = T)

# study estimates - random, inverse methods
RE_IV<-c( round(exp(iv$TE.random),3), 
         paste0("(" , round(exp(iv$lower.random), 3) ,"," ,
                round(exp(iv$upper.random), 3), ")" ))

# study estimates - fixed, inverse methods
FE_IV<-c( round(exp(iv$TE.fixed),3), 
          paste0("(" , round(exp(iv$lower.fixed), 3) ,"," , 
                 round(exp(iv$upper.fixed), 3) , ")" ))

# Peto OR 
Peto<-c(peto_estimates_sub[4,1], 
        paste0("(" , peto_estimates_sub[4,2] ,"," , 
               peto_estimates_sub[4,3] , ")" ))

# Panel A

Panel_A<-study_est_bayes%>%
  filter(trial_name %in% c("Luby et al., 2018", "Null et al., 2018", 
                           "Peletz et al., 2012", "Luby et al., 2006",
                           "Crump et al., 2005"
  ))%>%
  mutate(mean=round(mean,3),
         lower=round(mean,3),
         upper=round(upper,3))%>%
  mutate(CI= paste0("(",lower, "," ,upper, ")"))%>%
  select( mean, CI)%>%
  as.matrix()%>%
  rbind(RE_IV)%>%
  rbind(FE_IV)%>%
  rbind(c(bayes_estimates_sub[4,1], # Mean bayes OR
          bayes_estimates_sub[4,4] ))%>%
  rbind(c(NA, NA))%>% # Mean Peto OR
  t()

Panel_A[,2]<-c( NA, NA) # Not reporting Luby et al. estimates for the IV method panel

s4_names<-study_est_bayes%>%
  filter(trial_name %in% c("Luby et al., 2018", "Null et al., 2018", 
                           "Peletz et al., 2012", "Luby et al., 2006",
                           "Crump et al., 2005"
  ))%>%
  select(trial_name)%>%
  as.matrix()

s4_obs<-df_rare%>%
  filter(trial_name %in% c("Luby et al., 2018", "Null et al., 2018", 
                           "Peletz et al., 2012", "Luby et al., 2006",
                           "Crump et al., 2005"
  ))%>%
  select(Obs)%>%
  as.matrix()

# Panel B
Panel_B<-Individual_estimates_peto%>%
  filter(trial_name %in% c("Luby et al., 2018", "Null et al., 2018", 
                           "Peletz et al., 2012","Luby et al., 2006",
                           "Crump et al., 2005"
                           ))%>%
  select(mean, CI)%>%
  as.matrix()%>%
  rbind(c(NA, NA))%>% 
  rbind(c(NA, NA))%>%
  rbind(c(NA, NA))%>%
  rbind(Peto)%>%
  t()


table_study_subset <-rbind(c("Panel A: Odds ratio", rep(NA, 8)),
  Panel_A,c("Panel A: Peto Odds ratio", rep(NA, 8)), Panel_B, 
  c(s4_obs, rep(NA, 4)))

colnames(table_study_subset)<-c(s4_names, "Random-Effects OR", "Fixed-Effects OR", 
                      "Mean Bayesian OR", "Mean Peto OR")
rownames(table_study_subset)<-c(NA, "ITT effect on child mortality", "CI 95%",
                      NA, "ITT effect on child mortality", "CI 95%", 
                      "Obs.")
write.csv(table_study_subset, "analysis_output/tables/study-subset-estimates.csv")

#------------------------------------------------------------------------------#
#  Odds ratio for dropping one study at a time - Table S5
#------------------------------------------------------------------------------#

df_rare_S5 <- arrange(df_rare, -weeks)

S5_bayes_print <- S5_bayes%>%
  mutate(CI_bayes=paste0("(", round(S5_bayes$lower,2), 
                         ",", round(S5_bayes$upper,2), ")"),
         mean=round(mean,2))%>%
  select(excluded_study, mean, CI_bayes)%>%
  rename(mean_bayes=mean)



S5_peto<-list()
for(i in 1:nrow(df_rare_S5)){
  df_iteration <- filter(df_rare_S5, !row_number() %in% i)
  # Peto 
  peto_S5 <- metabin(tcases, t_n, ccases, c_n,
                     data = apply_cc(df_iteration), 
                     method = "Peto",random = T)
  S5_peto[[i]]<-c(round(exp(peto_S5$TE.random), 2), 
                  round(exp(peto_S5$lower.random),2), 
                  round(exp(peto_S5$upper.random),2))
}

S5_peto<-unlist(S5_peto)%>%
  matrix(nrow=3)%>%
  t()

colnames(bayes_weights)<-c("weights_bayes", "excluded_study")

table_loo_study <-cbind(S5_peto[,1], 
               CI_peto=paste0("(", S5_peto[,2], ",", S5_peto[,3], ")"),
               trial_name=df_rare_S5$trial_name)%>%
  as.data.frame()%>%
  inner_join(peto_weights)%>%
  rename(mean_peto=V1,
         weights_peto=weights,
         excluded_study=trial_name)%>%
  select(excluded_study, mean_peto, CI_peto, weights_peto)%>%
  inner_join(S5_bayes_print)%>%
  inner_join(bayes_weights)%>%
  as.matrix()%>%
  t()

colnames(table_loo_study) <- table_loo_study[1,]
table_loo_study <- table_loo_study[-1,]
write.csv(table_loo_study, "analysis_output/tables/table-loo-study.csv")

print("For dropping one study analysis:")
cat("Max Peto ", max(S5_peto[,1]), " min Peto ", min(S5_peto[,1]))
cat("Max Bayes ", max(S5_bayes$mean), " min Peto ", min(S5_bayes$mean))
cat("UCI: Max Bayes ", max(S5_bayes$upper), " max Peto ", max(S5_peto[,3]))
cat("LCI: min Peto ", max(S5_peto[,2]), " min Bayes ", min(S5_bayes$lower))

#------------------------------------------------------------------------------#
# Table S6
#------------------------------------------------------------------------------#

many_summ_peto_bayes_df <-c()

for(i in 5:9){
  many_summ_peto_bayes_df<-rbind(many_summ_peto_bayes_df, 
                  peto_estimates_sub[i, c(1,5,4)],
                  c(bayes_estimates_sub[i, c(1,4)], NA))
}

many_summ_peto_bayes_df<-t(many_summ_peto_bayes_df)
colnames(many_summ_peto_bayes_df)<-c("Mean Peto OR (1)", "Mean Bayesian OR (2)",
                      "Mean Peto OR (3)", "Mean Bayesian OR (4)",
                      "Mean Peto OR (5)", "Mean Bayesian OR (6)",
                      "Mean Peto OR (7)", "Mean Bayesian OR (8)",
                      "Mean Peto OR (9)", "Mean Bayesian OR (10)")

rownames(many_summ_peto_bayes_df)<-c("ITT effect on child mortality", "CI 95%", "p-value")

write.csv(many_summ_peto_bayes_df, "analysis_output/tables/many-summ-peto-bayes.csv")

#------------------------------------------------------------------------------#
# Table elpd
#------------------------------------------------------------------------------#

#Averagle elpd
table_elpd<-c(bg_loo$elpd, bg_loo$se)%>%
  as.matrix()%>%
  rbind("Random effects")%>%
  rbind("Odds Ratio")

rownames(table_elpd)<-c("Average Expected log predictive density", "se",
                      "Pooling", "Model")

write.csv(table_elpd, "analysis_output/tables/table-elpd.csv")

#------------------------------------------------------------------------------#
# Table S10
#------------------------------------------------------------------------------#

table_s10<-rbind(
  peto_estimates_sub[10, c(1,5,4)], 
  c(bayes_estimates_sub[10, c(1,4)], NA)
)

table_s10<-t(table_s10)
rownames(table_s10)<-c("ITT effect on child mortality", "CI 95%", "p-value")
colnames(table_s10)<-c("Mean Peto OR", "Mean bayesian OR")

write.csv(table_s10, "analysis_output/tables/table-peto-bayes-row-10.csv")

#------------------------------------------------------------------------------#
# Table S11
#------------------------------------------------------------------------------#

table_diff_means<-matrix(NA, ncol=1, nrow = 4)
source("inclusion_exclusion_analysis.R")
for(i in 1:4){
  table_diff_means[i, 1]<-summary(reg[[i]])$coef[2, "Pr(>|t|)"]
}

table_diff_means <-t(table_diff_means)
rownames(table_diff_means)<-"Test for difference in means - p-val"
colnames(table_diff_means)<-c("Diarrhea effect size", "Compliance rate", "Setting", "Water source type")

write.csv(table_diff_means, "analysis_output/tables/table-diff-means.csv")

#------------------------------------------------------------------------------#
# Figure S1
#------------------------------------------------------------------------------#

# Inverse variance, OR
# Keep studies that have events (avoid making corrections)
df_rare_iv_or <- filter(df_rare, ccases > 0 & tcases > 0)

iv_all_or <- metabin(tcases, t_n, ccases, c_n,
                     data = df_rare_iv_or, 
                     method = "Inverse",
                     random = T,
                     sm="OR",
                     subgroup =intervention,
                     studlab = trial_name)

png(file = "analysis_output/figures/forest-no-corrections.png", width = 2400, height = 2000, res = 300)
forest.meta(iv_all_or, sortvar = intervention, leftcols = "studlab", leftlabs = "Study",
            rightcols = c("effect.ci", "w.random"), fixed=F, hetstat=F, test.subgroup=F)
dev.off()

# FE estimates
round(exp(c(iv_all_or$TE.fixed, iv$lower.fixed, iv$upper.fixed)), 3)

# Inverse variance, risk difference
iv_all_rd <- metabin(tcases, t_n, ccases, c_n,
                     data = df_rare_iv_or, 
                     method = "Inverse",
                     random = T,
                     sm="RD",
                     subgroup =intervention,
                     studlab = trial_name,
                     pscale = 100)

png(file = "analysis_output/figures/forest-rd.png", width = 2400, height = 2000, res = 300)
forest.meta(iv_all_rd, sortvar = intervention, leftcols = "studlab", leftlabs = "Study",
            rightcols = c("effect.ci", "w.random"), fixed=F, hetstat=F, test.subgroup=F)
dev.off()



#------------------------------------------------------------------------------#
# Figure S5 -----
#------------------------------------------------------------------------------#

# --------------------- Peto Odds --------------------- #

# Monitoring periods
period<-c(104, 78, 65, 52, 37, 20, 13, 9.5)

# List of mean effects, lower and upper 
peto_estimates_duration<-matrix(NA, ncol=5, nrow = length(period))
colnames(peto_estimates_duration)<-c("mean", "lower", "upper", "p-value", "Weeks")

# --- looping over subsets of data to get subset specific odds --- #
for (i in 1:8) {
  
  # Creating data frame with studies longer than i weeks 
  df_duration <- filter(df_rare, weeks>=period[i])
  
  # meta-analysis model
  monitoring_peto <- metabin(tcases, t_n, ccases, c_n,
                             data = apply_cc(df_duration), 
                             method = "Peto",random = T)
  
  # Peto RE
  peto_estimates_duration[i, 1]<-round(exp(monitoring_peto$TE.random),3)
  peto_estimates_duration[i, 2]<-round(exp(monitoring_peto$lower.random),3)
  peto_estimates_duration[i, 3]<-round(exp(monitoring_peto$upper.random),3)
  # p- values
  peto_estimates_duration[i, 4]<-monitoring_peto$pval.random
  # weeks cut-off
  peto_estimates_duration[i, 5]<-period[i]
}

peto_estimates_duration<-
  as.data.frame(peto_estimates_duration)

# Graphing 
png(file = "analysis_output/figures/ma-week-plot.png", width = 2400, height = 2000, res = 300)
peto_estimates_duration%>%
  ggplot(aes(x=Weeks)) +
  geom_hline(yintercept=1, linetype="dashed", 
             size=0.5) +
  geom_point(aes( y = mean, colour="Peto OR estimate")) +
  geom_line(aes( y = mean, colour="Peto OR estimate"), linetype="dashed") +
  geom_errorbar(aes(ymin=lower, ymax=upper, colour="95% CI"), width=3, position=position_dodge(0.1)) +
  scale_color_hue(l=40, c=35)+
  ylim(0.51, 1.1)+
  xlab("X weeks") + 
  ylab("Peto OR estimates") +
  ggtitle("Sensitivity of meta-analysis estimates to weeks of monitoring") + 
  default_theme +
  theme(legend.title = element_blank(),
        legend.position = "none")
dev.off()

#------------------------------------------------------------------------------#
# Figures S5, S6
#------------------------------------------------------------------------------#

library(ggpubr)

# (i) - diarrhea effects and compliance rates (S5) 

hist_diarr_effect<-df_studies%>%
  filter(!is.na(`effect estimate (on diarrhea)`))%>%
  ggplot(aes(x = `effect estimate (on diarrhea)`)) +
  geom_histogram() +
  facet_grid(Included ~ .) + 
  ylab("Number of studies") + 
  xlab("Effect on diarrhea (OR") +
  default_theme

hist_compliance<-df_studies%>%
  ggplot(aes(x = `Compliance rate`)) +
  geom_histogram() +
  facet_grid(Included ~ .) + 
  ylab("Number of studies") + 
  xlab("Compliance rate (%)") +
  default_theme

png(file = "analysis_output/figures/fig-compliance-diarr-hist.png", width = 4000, height = 2000, res = 300)
ggarrange(hist_diarr_effect, hist_compliance)
dev.off()

# (iii) - Setting and source of water (S6)

hist_setting<-df_studies%>%
  filter(!is.na(setting))%>%
  ggplot(aes(x = setting)) +
  geom_histogram(stat="count", na.rm = T) +
  facet_grid(Included ~ .) + 
  ylab("Number of studies") + 
  xlab("Setting") +
  default_theme

hist_source<-df_studies%>%
  filter(!is.na(`baseline water/water in control group`))%>%
  ggplot(aes(x = `baseline water/water in control group`)) +
  geom_histogram(stat="count", na.rm = T) +
  facet_grid(Included ~ .) + 
  ylab("Number of studies") + 
  xlab("Water source -type") +
  default_theme


png(file = "analysis_output/figures/fig-study-breakdown.png", width = 4000, height = 2000, res = 300)
ggarrange(hist_setting, hist_source)
dev.off()


#------------------------------------------------------------------------------#
# Comparing mortality effects with diarrhea estimates and compliance rates -----
#------------------------------------------------------------------------------#

iep_df<-Individual_estimates_peto%>%
  inner_join(df_rare)%>%
  select(trial_name,intervention, mean, lower, upper, compliance,
         diarrhea_effects, prevalence, cluster_rand, intervention) %>%
  mutate(
    randomisation_unit=ifelse(cluster_rand=="yes", "Cluster", "Household"),
    chlorination      =ifelse(intervention=="Chlorination", "Chlorination", "Other"),
    filtration        =ifelse(intervention=="Filtration", "Filtration", "Other"),
    spring            =ifelse(intervention=="Spring protection", "Spring protection", "Other")
  ) 




# Prepping metareg calls
diarrhea_prev<-iep_df$prevalence
compliance<-iep_df$compliance
cluster<-iep_df$randomisation_unit
diarrhea <-iep_df$diarrhea_effects

png(file = "analysis_output/figures/peto-bubble-diarr.png", 
    width = 2400, height = 2000, res = 300)
m1<-metareg(peto_m_random, diarrhea_prev)
bubble(m1, lwd = 1, col.line = "blue", xlim = c(0, 0.4), 
       xlab="Diarrhea prevalence", ylab = "Mortality Peto OR estimates")
dev.off()

png(file = "analysis_output/figures/peto-bubble-compliance.png", 
    width = 2400, height = 2000, res = 300)
m2<-metareg(peto_m_random, compliance)
bubble(m2, lwd = 1, col.line = "blue", xlim = c(0, 1.5), 
       xlab="Compliance rates", ylab = "Mortality Peto OR estimates")
dev.off()

png(file = "analysis_output/figures/peto-bubble-level.png", 
    width = 2400, height = 2000, res = 300)
m3<-metareg(peto_m_random, cluster)
bubble(m3, lwd = 1, col.line = "blue", 
       xlab="Level of randomization", ylab = "Mortality Peto OR estimates")
dev.off()

# Intervention
#png(file = "analysis_output/figures/FigS12_chlorination.png", 
#    width = 2400, height = 2000, res = 300)
#chlorination <-iep_df$chlorination
#m4_a<-metareg(peto_m, chlorination)
#bubble(m4_a, lwd = 1, col.line = "blue", 
#       xlab="Type of intervention", ylab = "Mortality Peto OR estimates")
#dev.off()

#png(file = "analysis_output/figures/FigS12_filtration.png", 
#    width = 2400, height = 2000, res = 300)
#filtration <-iep_df$filtration
#m4_b<-metareg(peto_m, filtration)
#bubble(m4_b, lwd = 1, col.line = "blue", 
#       xlab="Type of intervention", ylab = "Mortality Peto OR estimates")
#dev.off()

#png(file = "analysis_output/figures/FigS12_spring.png", 
#    width = 2400, height = 2000, res = 300)
#spring <-iep_df$spring
#m4_c<-metareg(peto_m, spring)
#bubble(m4_c, lwd = 1, col.line = "blue", 
#       xlab="Type of intervention", ylab = "Mortality Peto OR estimates")
#dev.off()

# Diarrhea effects
png(file = "analysis_output/figures/peto-bubble-diarr-eff.png", 
    width = 2400, height = 2000, res = 300)
m5<-metareg(peto_m_random, diarrhea)
bubble(m5, lwd = 1, col.line = "blue", xlim = c(0, 1.5), 
       xlab="Diarrhea effect estimates", ylab = "Mortality Peto OR estimates")
dev.off()

# Year
png(file = "analysis_output/figures/peto-bubble-year.png", 
    width = 2400, height = 2000, res = 300)
m6<-metareg(peto_m_random, year)
bubble(m6, lwd = 1, col.line = "blue", 
       xlab="Year", ylab = "Mortality Peto OR estimates")
dev.off()



### Summarise heterogeneity by characteristic in one table
m_regs = list(
  m1,
  m2,
  m3,
  m5,
  m6
)

meta_reg_supp_summary = map_dfr(m_regs, broom::tidy) %>%
  filter(term != "intercept")  

meta_reg_supp_summary %>%
  write_csv("analysis_output/tables/het-te-metareg-table.csv")
