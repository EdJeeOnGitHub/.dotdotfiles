library(tidyverse)
library(readxl)
library(ggstance)
library(metafor)
library(meta)

df = read_xlsx("data/Wolf et al studies.xlsx", sheet = 1, skip = 1)

df = janitor::clean_names(df)

#' Impute Standard Errors based on CIs
#' 
#' We're only given confidence intervals not SEs so need to impute the SE given 
#' the CI. However, since this is RR CIs are calculated like so (I think:)
#' 
#'  CI = exp(log(RR) +- Z_{1 - \alpha/2} se(log(RR)))
#' Therefore, we can only uncover the se of log(RR) since exp(se(log(RR))) \neq 
#' se(RR).
#' 
#' When we do this, we can use either the upper or lower CI - I do both and plot 
#' imputed SEs against each other. I'm not sure why they don't align exactly in 
#' Wolf et al - perhaps due to rounding issues?
#'
impute_se = function(ci_data, bound) {
    if (bound == "upper") {
        ln_upper = log(ci_data$upper_95_percent_confidence_interval)
        ln_point = log(ci_data$effect_estimate_on_diarrhea)
        se_ln_RR = (ln_upper - ln_point)/1.96
    }
    if (bound == "lower") {
        ln_lower = log(ci_data$lower_95_percent_confidence_interval)
        ln_point = log(ci_data$effect_estimate_on_diarrhea)
        se_ln_RR = (ln_lower - ln_point)/1.96*-1
    }
    return(se_ln_RR)
}


# Some initial cleaning
df = df %>%
    mutate(upper_95_percent_confidence_interval = as.numeric(upper_95_percent_confidence_interval)) %>%
    mutate(
        ref_first_word = str_extract(reference, "^\\w+"), 
        shorthand_ref = paste0(ref_first_word, "-", study_year),
        se_imp_upper = impute_se(., bound = "upper"),
        se_imp_lower = impute_se(., bound = "lower"),
        se_imp = (se_imp_upper + se_imp_lower)/2, 
        ln_RR = log(effect_estimate_on_diarrhea), 
    ) %>%
    group_by(shorthand_ref) %>%
    mutate(tmp_id = 1:n()) %>%
    ungroup() %>%
    mutate(shorthand_ref = paste0(shorthand_ref, "-", tmp_id)) %>%
    select(-tmp_id)
# This study has a point estimate outside it's own CI...
df = df %>%
    filter(
        !str_detect(
            reference,
            ". H. Humphrey, M. N. N. Mbuya, R. Ntozini, L. H. Moulton," ))


p_se_imp = df %>%
    ggplot(aes( 
        x = se_imp_upper, 
        y = se_imp_lower
    )) +
    geom_point() +
    geom_abline(linetype = "longdash") +
    theme_minimal() +
    labs( 
        title = "Implied Standard Errors using Upper vs Lower CI"
    )
clean_df = df %>%
    filter( 
        !is.na(ln_RR),
        !is.na(se_imp)
    ) %>%
    mutate(study_number = 1:n())

chlorine_df = clean_df %>%
    filter(str_detect(intervention, "chlor"))

chlorine_df %>%
    ggplot(aes( 
        x = effect_estimate_on_diarrhea, 
        xmin = lower_95_percent_confidence_interval,
        xmax = upper_95_percent_confidence_interval, 
        y =  shorthand_ref
    )) +
    geom_pointrangeh()
## Saving data for Andrews Kasy pub bias
clean_df %>%
    select(
        shorthand_ref
        ) %>%
        write_csv("publication_bias/Applications/water/all-diarrhea-labels.csv")

clean_df %>%
    select( 
        ln_RR,
        se_imp, 
        study_number
    ) %>%
    write_csv("publication_bias/Applications/water/all-diarrhea-estimates.csv")



chlorine_df %>%
    select(
        shorthand_ref
        ) %>%
        write_csv("publication_bias/Applications/water/chlorine-diarrhea-labels.csv")

chlorine_df %>%
    select( 
        ln_RR,
        se_imp, 
        study_number
    ) %>%
    write_csv("publication_bias/Applications/water/chlorine-diarrhea-estimates.csv")


## Now some meta-analysis stuff
clean_fit = metagen(
    TE = ln_RR,
    seTE = se_imp,
    data = clean_df, 
    sm = "RR"
)
chlorine_fit = metagen(
    TE = ln_RR,
    seTE = se_imp,
    data = chlorine_df,
    sm = "RR"
)

png(
    filename = "analysis_output/figures/diarr-pub-bias-funnel.png",
    width = 8,
    height = 12,
    units = "in",
    res = 1200
)
plot_par = par(mfrow = c(2, 1))

clean_fit %>%
    funnel()
title(main = "All Diarrhea Interventions Funnel")

chlorine_fit %>%
    funnel()
title(main = "Chlorine Diarrhea Interventions Funnel")
dev.off()


bias_clean_fit = metabias(clean_fit)
bias_chlorine_fit = metabias(chlorine_fit)


bias_df = bind_rows(
    bias_clean_fit$estimate,
    bias_chlorine_fit$estimate
) %>%
    mutate(
        pval = c(bias_clean_fit$pval, bias_chlorine_fit$pval), 
        N = c(nrow(clean_df), nrow(chlorine_df)),
        data = c("All", "Chlorine"))

bias_df %>%
    write_csv( 
        "analysis_output/tables/diarr-pub-bias-eggers.csv"
    )
