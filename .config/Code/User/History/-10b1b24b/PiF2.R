# library(tidylog, warn.conflicts = FALSE)
library(readxl)
library(haven)
library(tidyverse)
library(lubridate)
library(lubridate)
source("helpers.R")

# Functions for data processing and checks 

# Generate variables for final data-set
gen_vars <- function(df, study, age_original){
  df_gen_vars<-df %>% 
    mutate(
      age_measure_original= age_original,
      study=study
    )
  return(df_gen_vars)
}

# (1) Reading all papers 

# Boisson et al
deaths_boisson <- read_xlsx("mortality_counts/Boisson et al. 2013/Aquatab deaths.xlsx", sheet="deaths")
data_boisson <- read_dta("mortality_counts/Boisson et al. 2013/Aquatab_INDIA10.dta")
# Dupas et al
data_dupas <- read_dta("mortality_counts/Dupas et al. 2021/child_analysis_nopii.dta")
# Kirby et al
deaths_kirby <- read_xlsx("mortality_counts/Kirby et al. 2019/mortality_data.xlsx")
data_kirby <- read_xlsx("mortality_counts/Kirby et al. 2019/Rwanda_TN_cRCT_blfu_child.xlsx")
# Kremer et al. 2012
data_kremer <- read_dta("mortality_counts/Kremer et al. 2011/data_clean.dta")
# Chiller et al
data_chiller_id <- read_xls("mortality_counts/Chiller et al. 2006/listaControl.xls",
                          sheet="listaControl")
data_chiller <- read_xlsx("mortality_counts/Chiller et al. 2006/Chiller incidence data for Ricardo.xlsx",
                        sheet="Vigfinal")
# Crump et al. 2005
data_crump <- read_xlsx("mortality_counts/Crump et al. 2005/WEEKLYSURVEILLANCE.xlsx",
                      sheet = "WEEKLYSURVEILLANCE")
# Luby et al. 2006
data_luby2006 <- read.csv("mortality_counts/Luby et al. 2006/Child plus HH plus cluster plus group Floc Health for Ricardo.csv")
deaths_luby2006<- read_xls("mortality_counts/Luby et al. 2006/FlocHealthDeaths.xls",
                            sheet = "Sheet1")
# Reller et al. 2003
data_reller <- read.csv("mortality_counts/Reller et al. 2003/Diarrhea Incidence Reller paper Reduced for Ricardo.csv")
# Peletz et al. 2012
data_peletz <- read_dta("mortality_counts/Peletz et al. 2012/Zambia data 7Feb19 short.dta")
# Null et al. 2018 - no age variables
data_null <- read_dta("mortality_counts/Null et al. 2018/washb-kenya-diar-public.dta")
data_null_death <- read_dta("mortality_counts/Null et al. 2018/washb-kenya-mortality-public.dta")
# Haushofer et al. 2020
data_haush <- read_dta("mortality_counts/Haushofer/kremer_martens_data.dta")


# (2) Data processing 

# ------ Boisson et al. 2013 ------ #

deaths_boisson<-deaths_boisson%>%
  filter(`<5`=="yes")%>%
  mutate(death=1)

mortality_boisson <- data_boisson %>% 
  # selecting identification and age variables
  select(indid, group, date, dob, agemth, ageyr, agereported) %>% 
  group_by(indid) %>% 
  # keeping observations with max age
  summarise(agemth=max(agemth),
            agereported=max(agereported),
            group=mean(group)) %>% 
  left_join(deaths_boisson) %>%
  mutate(death=ifelse(is.na(death),0,1),
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA) %>% 
  rename(age_year=agereported,
         age_month=agemth,
         wtreatment=group) %>% 
  gen_vars("Boisson et al. 2013", "month")

# ------ Dupas et al. 2021 ------ #

mortality_dupas <- data_dupas %>% 
  mutate(death=ifelse(child_gone==3, 1, 0),
         death=ifelse(is.na(death),0, death)) %>% 
  filter(!((fd_effect==1| ration_effect==1| wash_effect==1) & coupon!=1)) %>% 
  group_by(id, coupon, wash_effect, fd_effect, ration_effect) %>% 
  summarise(death=max(death),
            monthofdeath=max(monthofdeath, na.rm=T),
            bl_childage_years=max(bl_childage_years)
  ) %>% 
  rename(wtreatment=coupon,
         age_year=bl_childage_years,
         event_time=monthofdeath) %>% 
  mutate(
    treatment_time="2018-04-01",
    treatment_time=ymd(treatment_time),
    time_to_event=interval(treatment_time, event_time) %/% months(1),
    age_month=age_year*12,
    follow_up_months=18,
    t=treatment_time %m+% months(18)
  ) %>% 
  gen_vars("Dupas et al. 2021", "years")
  
# ------ "Kirby et al. 2019" ------ #

# age at death not clear from data 
# Since data was collected over several rounds
# For now, keeping the max age across all rounds

deaths_kirby <- deaths_kirby %>% 
  rename(a0=assignment,
         a5=cluster_id) %>% 
  group_by(child_id) %>% 
  mutate(max=max(surveyround)) %>% 
  # Keeping the last survey round
  filter(!surveyround<max) %>% 
  ungroup() %>% 
  select(a0, a5, child_id, agemonths) %>% 
  rename(age_month=agemonths) %>% 
  mutate(death=1)

mortality_kirby <- data_kirby %>% 
  group_by(childid) %>% 
  mutate(max=max(round)) %>% 
  # Keeping the last survey round
  filter(!round<max) %>% 
  ungroup() %>% 
  group_by(childid) %>% 
  mutate(max=max(round)) %>% 
  #Keeping the last survey round
  filter(!round<max) %>% 
  ungroup() %>% 
  select(childid, a0, a5, dobc, ch13c) %>% 
  rename(child_id=childid) %>% 
  left_join(deaths_kirby) %>% 
  mutate(death=ifelse(!is.na(death), 1, 0),
         wtreatment=ifelse(a0=="control", 0, 1),
         age_month=ifelse(!is.na(dobc), dobc, age_month),
         age_year=age_month/12,
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA) %>% 
  gen_vars("Kirby et al. 2019", "month")

# ------ "Kremer et al. 2012" ------ #

mortality_kremer <- data_kremer %>% 
  filter(t05!=1) %>% 
  select(child_death_dummy, age_combo_years, evertreat) %>% 
  mutate(age_months1=age_combo_years*12,
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA) %>% 
  rename(wtreatment=evertreat, 
         death=child_death_dummy,
         age_month=age_months1,
         age_year=age_combo_years) %>% 
  gen_vars("Kremer et al. 2012", "years")

mortality_kremer<-mortality_kremer %>% 
  filter(age_year<=5| is.na(age_year))

table(mortality_kremer$death, mortality_kremer$wtreatment)

# ------ "Chiller et al. 2006" ------ #

data_chiller_id<-data_chiller_id%>%
  select(MertuID, Cluster)%>%
  rename(Mertuid=MertuID)

mortality_chiller<-data_chiller%>%
  left_join(data_chiller_id)%>%
  mutate(village=substr(Mertuid, 1, 2))%>%
  select(PersonaID, Cluster, GRUPO, EDAD)%>%
  distinct()%>%
  mutate(age_month=EDAD*12)%>%
  mutate(death=ifelse(PersonaID == "143245", 1, 0),
         treatment=case_when(
           GRUPO== "CC" ~ 0,
           GRUPO== "CK" ~ 1
         ),
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA)%>%
  filter(!is.na(treatment))%>%
  rename(wtreatment=treatment,
         age_year=EDAD)%>% 
  gen_vars("Chiller et al. 2006", "years")

# ------ "Crump et al. 2005" ------ #

remove<-data_crump%>%
  filter(is.na(errorflag1))%>%
  filter(tw2week=="01" & tw2away==4)%>%
  select(tw2seq)%>%
  distinct()%>%
  mutate(remove=1)

temp<-data_crump%>%
  # Cleaning from Brandon's do-file
  filter(is.na(errorflag1))%>%
  left_join(remove)%>%
  filter(is.na(remove))%>%
  select(-remove)%>%
  mutate(compound = substr(tw2seq, 0,6),
         die = ifelse(tw2away==4,1,0)) %>% 
  group_by(tw2seq,compound,tw2studyarm) %>% 
  #filter(tw2age<5)%>% # filter
  summarize(death = max(die),
            maxage=max(tw2age))%>% 
  ungroup() %>% 
  filter(tw2seq!="015086G005", tw2seq!="157021A011", tw2seq!="009022B004", 
         tw2seq!="015080D006", tw2seq!="123140E004", tw2seq!="149001B004",
         tw2seq!="102052D003", tw2seq!="010129A014", tw2seq!="111036C008") %>%  
  filter(death==1)%>% 
  select(tw2seq,death, maxage)

mortality_crump<-data_crump %>% 
  filter(tw2age<5) %>%
  select(tw2seq,tw2v,tw2c,tw2h,i,tw2studyarm) %>%
  distinct() %>%
  left_join(temp) %>%
  mutate(die=ifelse(is.na(death),0,1)) %>%
  mutate(compound = paste0(tw2v,tw2c)) %>%
  select(die,compound,tw2studyarm, maxage)%>%
  #Combining into one treatment group
  mutate(tw2studyarm=case_when(
    tw2studyarm==3 ~ 1,
    tw2studyarm==2 ~ 1,
    tw2studyarm==1 ~ 0),
    age_measure_original="years",
    study=" Crump et al. 2005")%>% 
  rename(wtreatment=tw2studyarm,
         death=die)%>% 
  mutate(age_month=maxage*12,
         age_year=maxage,
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA)%>% 
  gen_vars("Crump et al. 2005", "year")

# ------ "Luby et al. 2006" ------ #

deaths_luby2006 <- deaths_luby2006 %>% 
  mutate(ChildID=as.numeric(ChildID)) %>% 
  rename(ChildId=ChildID,
         DOB_death=DOB)  

mortality_luby2006<-data_luby2006 %>%
  mutate(DOB=mdy(DOB)) %>% 
  left_join(deaths_luby2006) %>% 
  mutate(treatment=ifelse(Group!="X" & Group!="S", 1, 0),
         has_soap=ifelse(Group=="FS" | Group=="S", 1, 0)) %>%
  filter(has_soap==0)%>%
  rename(
    wtreatment=treatment,
    age_year=AGE,
    event_time=`Date of Death`
  ) %>% 
  mutate(death=ifelse(!is.na(event_time), 1,0),
         treatment_time="2003-04-01",
         treatment_time=ymd(treatment_time),
         event_time=ymd(event_time),
         time_to_event=interval(treatment_time, event_time) %/% months(1),
         age_month=age_year*12,
         follow_up_months=9,
         t=treatment_time %m+% months(9)
         ) %>% 
  gen_vars("Luby et al. 2006", "years") 

# ------ "Reller et al. 2003" ------ #
data_reller<-read.csv("mortality_counts/Reller et al. 2003/Diarrhea Incidence Reller paper Reduced for Ricardo.csv")

data_reller<-data_reller%>%
  rename(
    strata=localidad,
    clust_level=mertuid,
    childid=personaid
  )%>%
  mutate(
    wtreatment=ifelse(grupo!="CC", 1, 0)
  )%>%
  arrange(childid)%>%
  group_by(childid)%>%
  mutate(
    max_under5=max(Under5)
  )%>%
  ungroup()%>%
  filter(max_under5==1 | is.na(max_under5))%>%
  arrange(childid)%>%
  group_by(childid)%>%
  summarise(
    wtreatment=first(wtreatment),
    strata=first(strata),
    clust_level=first(clust_level),
    grupo=first(grupo),
    edad=mean(edad),
    edadac=max(edadac),
    agegroup=max(agegroup)
  )%>%
  arrange(clust_level)%>%
  group_by(clust_level)%>%
  mutate(child_order=row_number())%>%
  ungroup()

data_reller<-data_reller%>%
  mutate(
    # Children <5 with diarrhea data
    death=case_when(
      clust_level=="07054B" & child_order==1 ~ 1,
      clust_level=="11282" & child_order==1 ~ 1,
      clust_level=="12348" & child_order==1 ~ 1,
      clust_level=="04024" & child_order==1 ~ 1,
      clust_level=="12338" & child_order==1 ~ 1,
      clust_level=="09145C" & child_order==1 ~ 1,
      clust_level=="09234" & child_order==1 ~ 1,
      clust_level=="09175" & child_order==1~ 1,
      clust_level=="22704" & child_order==1~ 1,
      clust_level=="12134" & child_order==1~ 1
    ),
    death=ifelse(is.na(death), 0, death)
  )

# Children <5 with no diarrhea data - no age data 
no_dair<-c(NA, 0, 12, "12487", "CC", 1, NA)%>%
  rbind(c(NA, 0, 12, "12487", "CC", 1, NA))%>%
  rbind(c(NA, 0, 14, "14324", "CC", 1, NA))%>%
  rbind(c(NA, 1, 9, "09623", "CS", 1, NA))%>%
  rbind(c(NA, 1, 9, "09245", "CS", 1, NA))%>%
  as_tibble()

colnames(no_dair)<-c("childid", "wtreatment", "strata",
                     "clust_level", "grupo", "death",
                     "child_order")
no_dair<-no_dair%>%
  mutate(
    wtreatment=as.numeric(wtreatment),
    death=as.numeric(death),
    strata=as.numeric(strata),
    child_order=as.numeric(child_order)
  )

mortality_reller<-data_reller%>%
  bind_rows(no_dair)%>%
  # check if age variable is edad or edadac
  rename(age_month=edadac)%>%
  mutate(age_year=age_month/12,
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA) %>% 
  gen_vars("Reller et al. 2003", "month")

# ------ "Peletz et al. 2012" ------ #
data_peletz<-read_dta("mortality_counts/Peletz et al. 2012/Zambia data 7Feb19 short.dta")

mortality_peletz<-data_peletz%>%
  # 121 "child 1" (+twin in one housheold)
  filter((child==1 & id!=2019) | (id==2019))%>%
  group_by(person, id)%>%
  summarise(
    babydied=first(babydied),
    datedied=max(datedied, na.rm=T),
    arm=first(arm),
    diarr=first(diarr),
    communit=first(communit),
    age_month=max(agedied_mo),
    age_year=age_month/12
  )%>%
  rename(death=babydied,
         wtreatment=arm,
         strata=communit,
         event_time=datedied)%>% 
  mutate(
    treatment_time="2010-04-01",
    treatment_time=ymd(treatment_time),
    event_time=dmy(event_time),
    time_to_event=interval(treatment_time, event_time) %/% months(1),
    follow_up_months=12,
    t=treatment_time %m+% months(12)
  ) %>% 
  gen_vars("Peletz et al. 2012", "month")

# ------ "Null et al. 2018" ------ #
data_null<-data_null%>%
  select(clusterid, compoundid, hhid, childid, aged, agem, agey,tr)%>%
  group_by(clusterid, compoundid, hhid, childid)%>%
  mutate(max_age=max(agem))%>%
  ungroup()%>%
  mutate(age_year=agem/12) %>% 
  filter(agem==max_age)

mortality_null<-data_null_death%>%
  mutate(wtreatment=ifelse(tr==2,1,0),
         ac=ifelse(tr==1,1,0),
         pc=ifelse(tr==8,1,0),
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA)%>%
  rename(death=childdeath)%>%
  filter(wtreatment==1 | ac==1 | pc==1)%>%
  left_join(data_null)%>%
  rename(age_month=agem) %>% 
  gen_vars("Null et al. 2018", "month")

# ------ "Haushofer et al. 2020" ------ #
mortality_haush<-data_haush%>%
  filter(wave==1 & child_yob>=2013 | 
           wave!=1 & child_yob>=2014)%>%
  rename(
    age_month=child_dage,
    wtreatment=waterp,
    death=dead_u5
  ) %>% 
  mutate(age_year=age_month/12,
         treatment_time=NA,
         event_time=NA,
         time_to_event=NA,
         follow_up_months=NA,
         t=NA) %>% 
  gen_vars("Haushofer et al. 2020", "month")

# list of all available individual data 
  
mortality_all<-list(
  "Boisson et al., 2013" = mortality_boisson, 
  "Chiller et al., 2006"= mortality_chiller,
  "Crump et al., 2005" = mortality_crump, 
  "Dupas et al., 2021" = mortality_dupas,
  "Haushofer et al., 2020" = mortality_haush,
  "Kirby et al., 2019" = mortality_kirby,
  "Kremer et. al., 2011" = mortality_kremer, 
  "Luby et al., 2006" = mortality_luby2006,
  "Null et al., 2018" = mortality_null, 
  "Peletz et al., 2012" = mortality_peletz,
  "Reller et al., 2003" = mortality_reller
)  

# (3) Filtering variables with children less than 5 years 
mortality_all <- lapply(mortality_all, function(x) filter(x, age_year<5 | is.na(age_year))) 
# For some of the data the age variable is not available - studies where the sample only consists of 
# individuals under the age of 5

# (4) Keeping relevant variables for final dataset
vars=c("death", "wtreatment", "age_month", "age_year", "age_measure_original",
       "study", "treatment_time", "event_time",  "time_to_event", "follow_up_months",
       "t")
mortality_all <- lapply(mortality_all, function(x) select(x , vars))

# (5) Checks 

# Load event counts from the main data 
mortality_counts_data<-read.csv("data_rare.csv") %>% 
  filter(trial_name %in% c(
    "Boisson et al., 2013", 
    "Chiller et al., 2006",
    "Crump et al., 2005" , 
    "Dupas et al., 2021", 
    "Haushofer et al., 2020" , 
    "Kirby et al., 2019" , 
    "Kremer et. al., 2011",
    "Luby et al., 2006",
    "Null et al., 2018",
    "Peletz et al., 2012",
    "Reller et al., 2003"
  )) %>% 
  select(trial_name, ccases, cnoncases, tcases, tnoncases, Obs)

# Individual level data 
mortality_counts<-lapply(mortality_all, function(x){
  counts<-x %>% 
    group_by(wtreatment) %>% 
    summarise(
      cases=sum(death),
      count=n()
    ) %>% 
    mutate(noncases=count-cases)
})

# (5.1) Check if event counts are the same

for(i in 2:length(mortality_counts)){
  df<-filter(mortality_counts_data, trial_name %in% 
           names(mortality_counts)[i])  
    if(df$ccases != mortality_counts[[i]]$cases[1]){
      stop(paste0("Control cases do not match - ", names(mortality_counts)[i]))
    }
    if(df$tcases != mortality_counts[[i]]$cases[2]){
      stop(paste0("Treatment cases do not match - ", names(mortality_counts)[i]))
    }
    if(df$cnoncases != mortality_counts[[i]]$noncases[1]){
      stop(paste0("Control non-cases do not match - ", names(mortality_counts)[i]))
    }
    if(df$tnoncases != mortality_counts[[i]]$noncases[2]){
      stop(paste0("Treatment non-cases do not match - ", names(mortality_counts)[i]))
    }
  if(df$Obs != sum(mortality_counts[[i]]$count)){
    stop(paste0("Total number of observations do not match - ", names(mortality_counts)[i]))
  }
}

# (5.2) Check if all age variables are consistent with filtering conditions

lapply(mortality_all, function(x){
  df<-x %>% 
    mutate(flag_year=ifelse(age_year>=5, 1, 0),
           flag_month=ifelse(age_month>=60, 1, 0))
  if(sum(df$flag_year, na.rm=T)>0){
    stop("age greater than 5")
  }
  if(sum(df$flag_month, na.rm=T)>=1){
    stop("age greater than 5")
  }
})

# Final data-set of event counts for all papers 

mortality_all <- bind_rows(mortality_all) %>% 
  select(-person)

write.csv(mortality_all, file="mortality_counts/individual_data.csv")


############### ------------------ OLD CODES ------------------ ###############


# Boisson et al. 2013

deaths_boisson<-read_xlsx("mortality_counts/Boisson et al. 2013/Aquatab deaths.xlsx", sheet="deaths")%>%
  filter(`<5`=="yes")%>%
  mutate(death=1)
data_boisson<-read_dta("mortality_counts/Boisson et al. 2013/Aquatab_INDIA10.dta")

mortality_boisson<-data_boisson%>%
  # selecting identification and age variables
  select(indid, group, date, dob, agemth, ageyr, agereported)%>%
  group_by(indid)%>%
  # keeping observations with max age
  summarise(agemth=max(agemth),
            agereported=max(agereported),
            group=mean(group))%>%
  # Filtering on age reported but agemonth is >5 in some cases
  # Uncertain which age to keep for non-deaths
  filter(agereported<5)%>%
  # correcting for age above 60 months
  mutate(agemth=ifelse(agemth>=60, 60, agemth))%>%
  left_join(deaths_boisson)%>%
  mutate(death=ifelse(is.na(death),0,1),
         age_measure_original="month",
         study="Boisson et al. 2013")%>%
  rename(wtreatment=group,
         age_month=agemth)%>%
  select(death, wtreatment, age_month, age_measure_original, study)

# checking counts with main data 
table(mortality_boisson$death, mortality_boisson$wtreatment) # noncases do not match

# Dupas et al. 2021

data_dupas<-read_dta("mortality_counts/Dupas et al. 2021/child_analysis_nopii.dta")
mortality_dupas<-data_dupas%>%
  # treatment is either coupon of free delivery
  mutate(treatment=ifelse(coupon==1 | fd==1 | ration==1, 1, 0),
         death=ifelse(child_gone==3, 1, 0))%>%
  filter(!(wash==1 & treatment==0))%>%
  filter(bl_childage_years<5 & bl_childage_years>0)%>%
  select(death, id, child_position, treatment, cluster_id, mwanza, chw, bl_childage_years)%>%
  group_by(treatment, child_position, id, cluster_id, mwanza, chw)%>%
  summarise(death=max(death, na.rm = T),
            bl_childage_years=max(bl_childage_years),
            treatment=max(treatment))%>%
  ungroup()%>%
  mutate(death=ifelse(death==0 | is.na(death) | is.infinite(death),0,1))%>%
  distinct()%>%
  # Note number of non-cases is much higher than the one in data 
  select(treatment, death, bl_childage_years)%>%
  rename(wtreatment=treatment)%>%
  mutate(age_month=bl_childage_years*12,
         age_measure_original="years",
         study="Dupas et al. 2021")%>%
  select(-bl_childage_years)
  
table(mortality_dupas$death, mortality_dupas$wtreatment)

# Kirby et al. 2019 
# age at death not clear from data 
# Since data was collected over several rounds
# For now, keeping the max age across all rounds

deaths_kirby<-read_xlsx("mortality_counts/Kirby et al. 2019/mortality_data.xlsx")
data_kirby<-read_xlsx("mortality_counts/Kirby et al. 2019/Rwanda_TN_cRCT_blfu_child.xlsx")

deaths_kirby<-deaths_kirby%>%
  rename(a0=assignment,
         a5=cluster_id)%>%
  group_by(child_id)%>%
  mutate(max=max(surveyround))%>%
  # Keeping the last survey round
  filter(!surveyround<max)%>%
  ungroup()%>%
  select(a0, a5, child_id, agemonths)%>%
  rename(age_month=agemonths)%>%
  mutate(death=1)

mortality_kirby<-data_kirby%>%
  group_by(childid)%>%
  mutate(max=max(round))%>%
  # Keeping the last survey round
  filter(!round<max)%>%
  ungroup()%>%
  group_by(childid)%>%
  mutate(max=max(round))%>%
  #Keeping the last survey round
  filter(!round<max)%>%
  ungroup()%>%
  select(childid, a0, a5, dobc, ch13c)%>%
  rename(child_id=childid)%>%
  left_join(deaths_kirby)%>%
  mutate(death=ifelse(!is.na(death), 1, 0),
         wtreatment=ifelse(a0=="control", 0, 1),
         age_month=ifelse(!is.na(dobc), dobc, age_month),
         age_measure_original="month",
         study="Kirby et al. 2019 ")%>%
  select(-c(child_id, a5, a0, dobc))

mortality_kirby%>%
  filter(!is.na(ch13c))
  
table(mortality_kirby$death, mortality_kirby$wtreatment)

# Kremer et al. 2012

data_kremer<-read_dta("mortality_counts/Kremer et al. 2011/data_clean.dta")

mortality_kremer<-data_kremer%>%
  filter(t05!=1)%>%
  select(child_death_dummy, age_combo_years, age_combo_yrs2, evertreat)%>%
  mutate(age_months1=age_combo_years*12,
         age_months2=age_combo_yrs2*12,
         age_measure_original="years",
         study="Kremer et al. 2012")%>%
  rename(wtreatment=evertreat, 
         death=child_death_dummy,
         age_month=age_months1)%>%
  select(wtreatment,death,age_month, age_measure_original, study)
  
table(mortality_kremer$death, mortality_kremer$wtreatment)

# Chiller et al. 2006

data_chiller_id<-read_xls("mortality_counts/Chiller et al. 2006/listaControl.xls",
                          sheet="listaControl")
data_chiller<-read_xlsx("mortality_counts/Chiller et al. 2006/Chiller incidence data for Ricardo.xlsx",
                        sheet="Vigfinal")


data_chiller_id<-data_chiller_id%>%
  select(MertuID, Cluster)%>%
  rename(Mertuid=MertuID)

mortality_chiller<-data_chiller%>%
  left_join(data_chiller_id)%>%
  mutate(village=substr(Mertuid, 1, 2))%>%
  select(PersonaID, Cluster, GRUPO, EDAD)%>%
  distinct()%>%
  filter(EDAD<5)%>%
  mutate(age_month=EDAD*12)%>%
  mutate(death=ifelse(PersonaID == "143245", 1, 0),
         treatment=case_when(
           GRUPO== "CC" ~ 0,
           GRUPO== "CK" ~ 1
         ),
         age_measure_original="years",
         study="Chiller et al. 2006")%>%
  filter(!is.na(treatment))%>%
  rename(wtreatment=treatment)%>%
  select(wtreatment,death,age_month, age_measure_original, study)

table(mortality_chiller$death, mortality_chiller$wtreatment)

# Crump et al. 2005

data_crump<-read_xlsx("mortality_counts/Crump et al. 2005/WEEKLYSURVEILLANCE.xlsx",
                      sheet = "WEEKLYSURVEILLANCE")
remove<-data_crump%>%
  filter(is.na(errorflag1))%>%
  filter(tw2week=="01" & tw2away==4)%>%
  select(tw2seq)%>%
  distinct()%>%
  mutate(remove=1)

temp<-data_crump%>%
  # Cleaning from Brandon's do-file
  filter(is.na(errorflag1))%>%
  left_join(remove)%>%
  filter(is.na(remove))%>%
  select(-remove)%>%
  mutate(compound = substr(tw2seq, 0,6),
         die = ifelse(tw2away==4,1,0)) %>% 
  group_by(tw2seq,compound,tw2studyarm) %>% 
  filter(tw2age<5)%>% 
  summarize(death = max(die),
            maxage=max(tw2age))%>% 
  ungroup() %>% 
  filter(tw2seq!="015086G005", tw2seq!="157021A011", tw2seq!="009022B004", 
         tw2seq!="015080D006", tw2seq!="123140E004", tw2seq!="149001B004",
         tw2seq!="102052D003", tw2seq!="010129A014", tw2seq!="111036C008") %>%  
  filter(death==1)%>% 
  select(tw2seq,death, maxage)

mortality_crump<-data_crump %>% 
  filter(tw2age<5) %>%
  select(tw2seq,tw2v,tw2c,tw2h,i,tw2studyarm) %>%
  distinct() %>%
  left_join(temp) %>%
  mutate(die=ifelse(is.na(death),0,1)) %>%
  mutate(compound = paste0(tw2v,tw2c)) %>%
  select(die,compound,tw2studyarm, maxage)%>%
  #Combining into one treatment group
  mutate(tw2studyarm=case_when(
    tw2studyarm==3 ~ 1,
    tw2studyarm==2 ~ 1,
    tw2studyarm==1 ~ 0),
    age_measure_original="years",
    study=" Crump et al. 2005")%>% 
  rename(wtreatment=tw2studyarm,
         death=die)%>% 
  mutate(age_month=maxage*12)%>% 
  select(wtreatment,death,age_month, age_measure_original, study)

table(mortality_crump$death, mortality_crump$wtreatment)

# Luby et al. 2006

data_luby2006<-read.csv("mortality_counts/Luby et al. 2006/Child plus HH plus cluster plus group Floc Health for Ricardo.csv")

mortality_luby2006<-data_luby2006%>%
  filter(AGE<5 | ChildId==214402)%>%
  mutate(treatment=ifelse(Group!="X" & Group!="S", 1, 0),
         has_soap=ifelse(Group=="FS" | Group=="S", 1, 0),
         death=ifelse(
           ChildId==061001| 
             ChildId==241807|
             ChildId==070406|
             ChildId==064906|
             ChildId==214402|
             ChildId==112705|
             ChildId==064202,
           1, 0
         ),
         age_measure_original="years",
         study="Luby et al. 2006")%>%
  filter(has_soap==0)%>%
  rename(wtreatment=treatment)%>%
  mutate(age_month=AGE*12,)%>% 
  select(wtreatment,death,age_month, age_measure_original, study)

table(mortality_luby2006$death, mortality_luby2006$wtreatment)
table(mortality_all[[8]]$death, mortality_all[[8]]$wtreatment)

# Luby et al. 2018 - does not have age variable

# Reller et al. 2003
data_reller<-read.csv("mortality_counts/Reller et al. 2003/Diarrhea Incidence Reller paper Reduced for Ricardo.csv")

data_reller<-data_reller%>%
  rename(
    strata=localidad,
    clust_level=mertuid,
    childid=personaid
  )%>%
  mutate(
    wtreatment=ifelse(grupo!="CC", 1, 0)
  )%>%
  arrange(childid)%>%
  group_by(childid)%>%
  mutate(
    max_under5=max(Under5)
  )%>%
  ungroup()%>%
  filter(max_under5==1 | is.na(max_under5))%>%
  arrange(childid)%>%
  group_by(childid)%>%
  summarise(
    wtreatment=first(wtreatment),
    strata=first(strata),
    clust_level=first(clust_level),
    grupo=first(grupo),
    edad=mean(edad),
    edadac=max(edadac),
    agegroup=max(agegroup)
  )%>%
  arrange(clust_level)%>%
  group_by(clust_level)%>%
  mutate(child_order=row_number())%>%
  ungroup()

data_reller<-data_reller%>%
  mutate(
    # Children <5 with diarrhea data
    death=case_when(
      clust_level=="07054B" & child_order==1 ~ 1,
      clust_level=="11282" & child_order==1 ~ 1,
      clust_level=="12348" & child_order==1 ~ 1,
      clust_level=="04024" & child_order==1 ~ 1,
      clust_level=="12338" & child_order==1 ~ 1,
      clust_level=="09145C" & child_order==1 ~ 1,
      clust_level=="09234" & child_order==1 ~ 1,
      clust_level=="09175" & child_order==1~ 1,
      clust_level=="22704" & child_order==1~ 1,
      clust_level=="12134" & child_order==1~ 1
    ),
    death=ifelse(is.na(death), 0, death)
  )

# Children <5 with no diarrhea data - no age data 
no_dair<-c(NA, 0, 12, "12487", "CC", 1, NA)%>%
  rbind(c(NA, 0, 12, "12487", "CC", 1, NA))%>%
  rbind(c(NA, 0, 14, "14324", "CC", 1, NA))%>%
  rbind(c(NA, 1, 9, "09623", "CS", 1, NA))%>%
  rbind(c(NA, 1, 9, "09245", "CS", 1, NA))%>%
  as_tibble()

colnames(no_dair)<-c("childid", "wtreatment", "strata",
                     "clust_level", "grupo", "death",
                     "child_order")
no_dair<-no_dair%>%
  mutate(
    wtreatment=as.numeric(wtreatment),
    death=as.numeric(death),
    strata=as.numeric(strata),
    child_order=as.numeric(child_order)
  )

mortality_reller<-data_reller%>%
  bind_rows(no_dair)%>%
  rename(age_month=edad)%>%
  mutate(age_measure_original="month",
         study="Reller et al. 2003")%>%
  select(wtreatment,death,age_month, age_measure_original, study)

table(mortality_reller$death, mortality_reller$wtreatment)

# Peletz et al. 2012
data_peletz<-read_dta("mortality_counts/Peletz et al. 2012/Zambia data 7Feb19 short.dta")

mortality_peletz<-data_peletz%>%
  # 121 "child 1" (+twin in one housheold)
  filter((child==1 & id!=2019) | (id==2019))%>%
  group_by(person, id)%>%
  summarise(
    babydied=first(babydied),
    arm=first(arm),
    diarr=first(diarr),
    communit=first(communit),
    age_month=max(agedied_mo)
  )%>%
  rename(death=babydied,
         wtreatment=arm,
         strata=communit)%>%
  mutate(age_measure_original="month",
         study="Peletz et al. 2012")%>%
  # only 10 observations with age 
  select(wtreatment,death,age_month, age_measure_original, study)

table(mortality_peletz$death, mortality_peletz$wtreatment)

# Null et al. 2018 - no age variables
data_null<-read_dta("mortality_counts/Null et al. 2018/washb-kenya-diar-public.dta")
data_null_death<-read_dta("mortality_counts/Null et al. 2018/washb-kenya-mortality-public.dta")

data_null<-data_null%>%
  select(clusterid, compoundid, hhid, childid, aged, agem, agey,tr)%>%
  group_by(clusterid, compoundid, hhid, childid)%>%
  mutate(max_age=max(agem))%>%
  ungroup()%>%
  filter(agem==max_age)
  

mortality_null<-data_null_death%>%
  mutate(wtreatment=ifelse(tr==2,1,0),
         ac=ifelse(tr==1,1,0),
         pc=ifelse(tr==8,1,0),
         age_measure_original="month",
         study="Null et al. 2018")%>%
  rename(death=childdeath)%>%
  filter(wtreatment==1 | ac==1 | pc==1)%>%
  left_join(data_null)%>%
  rename(age_month=agem)%>%
  select(wtreatment,death,age_month, age_measure_original, study)

table(mortality_null$death, mortality_null$wtreatment)

# haushofer et al. 2020
data_haush<-read_dta("mortality_counts/Haushofer/kremer_martens_data.dta")

mortality_haush<-data_haush%>%
  filter(wave==1 & child_yob>=2013 | 
         wave!=1 & child_yob>=2014)%>%
  rename(
    age_month=child_dage,
    wtreatment=waterp,
    death=dead_u5
  )%>%
  mutate(
    age_measure_original="month",
    study="haushofer et al. 2020"
  )%>%
  select(wtreatment,death,age_month, age_measure_original, study, child_yob)

table(mortality_haush$death, mortality_haush$wtreatment)

# Combining mortality data across all studies 

df_all<-bind_rows(
  mortality_boisson,
  mortality_chiller,
  mortality_crump,
  mortality_dupas,
  mortality_kirby,
  mortality_kremer,
  mortality_luby2006,
  mortality_peletz,
  mortality_reller,
  mortality_null,
  mortality_haush)%>%
  mutate(age_month=ifelse(age_month>=60, 60, age_month))

save(df_all, file="data/individual_mortality_data.Rda")




