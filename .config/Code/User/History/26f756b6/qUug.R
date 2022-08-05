
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                        Replicate Pub Bias ORs 
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Clear memory and set seed
rm(list=ls())
set.seed(24)

# Packages for analytics
library(meta)

# Load in data
source("data_prep.R")


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
                  method = "Peto", 
                  random = T)


study_peto = tibble(
    or = peto_m$TE,
    conf.low = peto_m$lower,
    conf.high = peto_m$upper,
    std.error = peto_m$seTE,
    trial_name = df_rare$trial_name
) %>%
    mutate(type = "replication")
study_peto


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
) %>%
    mutate(type = "old")




comp_df = bind_rows(
    old_or, 
    study_peto
)

comp_df %>%
    select(or, trial_name, type) %>%
    spread(type, or) %>%
    ggplot(aes(
        x = old,
        y = replication 
    )) +
    geom_point() +
    geom_abline(linetype = "longdash") +
    theme_bw() +
    labs(title = "Peto OR - Replication vs Brandon csv", 
         caption = "Calculated using metabin")
ggsave("analysis_output/figures/study-or-replication-check.png",
       width = 8,
       height = 6)

comp_df %>%
    select(std.error, trial_name, type) %>%
    spread(type, std.error) %>%
    ggplot(aes(
        x = old,
        y = replication 
    )) +
    geom_point() +
    geom_abline(linetype = "longdash") +
    theme_bw() +
    labs(title = "Peto OR Std.Errors - Replication vs Brandon csv", 
         caption = "Calculated using metabin")

ggsave("analysis_output/figures/study-or-se-replication-check.png",
       width = 8,
       height = 6)

# Formatting for matlab
study_peto %>%
    select(or, std.error) %>%
    mutate(row = 1:n()) %>%
    write_csv("Publication_bias/Applications/water/rep-or-estimates.csv", col_names = FALSE)
# for whatever reason these are the colnames in the original so for now we 
# stick with them until I figure out if we can remove them
study_peto %>%
    select(trial_name) %>%
    mutate(
        "Name (year)" = trial_name,
        "Shorted Name (year)" = trial_name
   )   %>%
   select(-trial_name) %>%
    write_csv("Publication_bias/Applications/water/rep-or-labels.csv")
