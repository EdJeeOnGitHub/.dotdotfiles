library(tidyverse)

#------------------------------------------------------------------------------#
# Import data
#------------------------------------------------------------------------------#


df_all <- read_csv("data/summary_data.csv") %>%
  mutate(t_n = tcases + tnoncases,
         c_n = ccases + cnoncases)

# Select trials to analyse
trials <- c("Luby et al., 2018 (W vs. control)",
            "Null et al., 2018 (W vs. active + passive control)",
            "Haushofer et al., 2020 (W vs. passive control)", 
            "Reller et al., 2003 (All 4 water treatments vs. control)", 
            "Boisson et al., 2013 (Chlor vs. control (placebo))", 
            "Peletz et al., 2012 (Filtration vs. control)", 
            "Kremer et. al., 2011 (Year 1 Treatment vs control)", 
            "Luby et al., 2006 (Chlor + Floc vs. control)", 
            "Semenza et al., 1998 (Chlor vs control (non-piped source))", 
            "Chiller et al., 2006 (Floc vs control)",
            "Crump et al., 2005 (Chlor + Floc vs. control)", 
            "Kirby et al., 2019 (W vs. control)",
            "Humphrey et al., 2019 (WASH vs. control)",
            "Dupas et al., 2021 (Coupons + Free Delivery vs control)",
            "Quick et al., 1999 (Chlor + safe storage + community education vs control)")

df_rare <- df_all[df_all$trial %in% trials,]

# ---------- Creating a list of subsets of data ------------ #

df<-list(
  # (1)
  df_chlorination = df_rare%>%
    filter(intervention %in% "Chlorination") ,
  
  # (2)
  df_filtration = df_rare%>%
    filter(intervention %in% "Filtration"),
  
  # (3)
  df_spring = df_rare%>%
    filter(intervention %in% "Spring protection"),
  
  # (4)
  df_tableS4 = df_rare%>%
    filter(trial_name %in% c(
      "Luby et al., 2018", 
      "Null et al., 2018",
      "Peletz et al., 2012",
      "Luby et al., 2006",
      "Crump et al., 2005"
    )),
  
  # (5) Combining Null et al and Haushofer et al studies
  
  df_tableS6_col1 = df_all%>%
    filter(trial %in% c(trials, 
                        "Null et al., 2018 (W vs. active + passive control) + Haushofer et al., 2020 (W vs. passive control)"))%>%
    filter(!trial %in% c("Null et al., 2018 (W vs. active + passive control)", 
                         "Haushofer et al., 2020 (W vs. passive control)")),
  
  # (6) Adding du Preez et al., 2011
  
  df_tableS6_col2 = df_all%>%
    filter(trial %in% c(trials, 
                        "du Preez et al., 2011 (SODIS vs. control)")),
  
  # (7) Adding Boisson et al. 2010
  
  df_tableS6_col3 = df_all%>%
    filter(trial %in% c(trials, 
                        "Boisson et al., 2010 (Filtration vs. control (w/placebo))")),
  
  # (9) Adding Null et al with only active control group
  
  df_tableS6_col4 = df_all%>%
    filter(trial %in% c(trials, 
                        "Null et al., 2018 (W vs. active control)"))%>%
    filter(!trial %in% "Null et al., 2018 (W vs. active + passive control)"),
  
  # (9) Adding Kremer et al with spring protection in year 1 and 2
  
  df_tableS6_col5 = df_all%>%
    filter(trial %in% c(trials, 
                        "Kremer et. al., 2011 (Year 1 Treatment vs control + Year 2 Treatment)"))%>%
    filter(!trial %in% "Kremer et. al., 2011 (Year 1 Treatment vs control)"),
  
  # (10) Dropping studies that include hand washing interventions
  #df_tableS10 = df_rare%>%
  #  filter(!trial %in% c("Humphrey et al., 2019 (WASH vs. control)", 
  #                       "Luby et al., 2006 (Chlor + Floc vs. control)"))
  
  #(10) Dropping studies that include hand washing interventions/ cookstoves
  df_tableS10 = df_rare%>%
    filter(!trial %in% c("Humphrey et al., 2019 (WASH vs. control)", 
                         "Kirby et al., 2019 (W vs. control)"))
  
)
