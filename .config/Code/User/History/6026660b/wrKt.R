# library(tidylog, warn.conflicts = FALSE)
library(readxl)
library(haven)
library(tidyverse)
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
data_haush <- read_dta("mortality_counts/Haushofer et al. 2021/kremer_martens_data.dta")


# (2) Data processing 

# ------ Boisson et al. 2013 ------ #

deaths_boisson<-deaths_boisson %>%
  filter(`<5`=="yes") %>%
  mutate(death=1)

mortality_boisson <- data_boisson %>% 
  group_by(indid) %>% 
  # Age variables are age reported at the baseline/ first follow-up 
  # for those missed in the baseline census 
  summarise(
    ageyr=min(ageyr), # age of those included in baseline
    agereported=min(agereported), # age of those missed at baseline but included in follow-up + baseline
    newborn=mean(newborn, na.rm=T),
    agemth_end=max(agemth),
    agemth_base=min(agemth),
    group=mean(group),
    follow_up_time = max(date)
  ) %>% 
  ungroup() %>% 
  left_join(deaths_boisson) %>%
  mutate(death=ifelse(is.na(death),0,1),
         treatment_time=ymd("2010-11-01"),
         follow_up_time=ymd(follow_up_time),
         event_time_lb =case_when(
           indid == 122030501 ~ ymd("2011-02-10"),
           indid == 122170507 ~ ymd("2011-10-14"),
           indid == 245130801 ~ ymd("2011-01-19")
         ),
         event_time_ub =case_when(
           indid == 122030501 ~ ymd("2011-03-18"),
           indid == 122170507 ~ ymd("2011-11-29"),
           indid == 245130801 ~ ymd("2011-02-17")
         ),
         precision_fut="day",
         precision_trt ="month", 
         precision_evt = "month",
         crossed_five=ifelse(agereported<5 & 
                               agereported + interval(treatment_time, follow_up_time) %/% years(1)>=5, 1, 0)) %>% 
  rename(age_year=agereported, 
         age_month=agemth_base,
         wtreatment=group) %>% 
  gen_vars("Boisson et al. 2013", "month") 

mortality_boisson <- mortality_boisson[which(mortality_boisson$age_month < 60 |
  mortality_boisson$age_year < 5),]

# ------ Dupas et al. 2021 ------ #

mortality_dupas <- data_dupas %>% 
  select(id, child_position, childmob,coupon, wash_effect, fd_effect, ration_effect, 
         bl_childage_years, child_gone, sdate, monthofdeath) %>% 
  mutate(death=ifelse(child_gone==3, 1, 0),
         death=ifelse(is.na(death),0, death)) %>% 
  filter(!((fd_effect==1| ration_effect==1| wash_effect==1) & coupon!=1)) %>% 
  group_by(id, coupon, wash_effect, fd_effect, ration_effect) %>% 
  summarise(death=max(death),
            monthofdeath=max(monthofdeath, na.rm=T),
            monthofdeath=ifelse(monthofdeath == -Inf, NA, monthofdeath),
            # I think bl_childage_years takes the min of child in each households across surveys
            bl_childage_years=min(bl_childage_years),
            # Taking the first survey date - baseline survey
            sdate=min(sdate)) %>% 
        rename(wtreatment=coupon, age_year=bl_childage_years) %>% 
    mutate(monthofdeath=as.Date(monthofdeath, origin = "1970-01-01"),
        event_time_lb = monthofdeath,
        event_time_ub=event_time_lb + ifelse(month(monthofdeath) == 4 | 
            month(monthofdeath) == 6, 29, 30),
        event_time_ub=as.Date(event_time_ub, origin = "1970-01-01"),
        treatment_time=sdate,
        treatment_time=ymd(treatment_time),
        age_month=age_year*12,
        follow_up_time=ymd("2019-06-30"),
        precision_fut="month",
        precision_trt ="day",
        precision_evt = "month",
        crossed_five=ifelse(age_year<5 & 
            age_year + interval(treatment_time, follow_up_time) %/% years(1)>=5, 1, 0)
  ) %>% 
  gen_vars("Dupas et al. 2021", "years")

# ------ "Kirby et al. 2019" ------ #
  
deaths_kirby <- read_xlsx("mortality_counts/Kirby et al. 2019/mortality_data.xlsx")
data_kirby <- read_xlsx("mortality_counts/Kirby et al. 2019/Rwanda_TN_cRCT_blfu_child.xlsx")

# Event date unknown  
# age at death not clear from data 
# Since data was collected over several rounds
# For now, keeping the max age across all rounds

deaths_kirby <- deaths_kirby %>% 
  rename(a0=assignment,
         a5=cluster_id) %>% 
  group_by(child_id) %>% 
  mutate(max=max(surveyround)) %>% 
  # Keeping the last survey round - assuming here that the 
  # event took place in the last survey round 
  filter(!surveyround<max) %>% 
  ungroup() %>% 
  select(a0, a5, child_id, surveydate) %>% 
  mutate(death=1)

mortality_kirby <- data_kirby %>% 
  group_by(childid) %>% 
  mutate(age_month=min(dobc),
         max=max(round)) %>% 
  # Keeping the last survey round
  filter(!round<max) %>% 
  ungroup() %>%
  select(childid, a0, a5, dobc, ch13c, age_month) %>% 
  rename(child_id=childid) %>% 
  left_join(deaths_kirby) %>% 
  mutate(
    death=ifelse(!is.na(death), 1, 0),
    wtreatment=ifelse(a0=="control", 0, 1),
    # Time of last follow-up
    follow_up_time="2016-03-31",
    follow_up_time=ymd(follow_up_time),
    precision_fut="month",
    precision_trt ="month",
    precision_evt = "year",
    # Treatment time - end of baseline enrollment
    # Specific date of intervention not provided in the data
    treatment_time="2014-12-31",
    treatment_time=ymd(treatment_time),
    # Children aged out
    crossed_five=ifelse(age_month<60 &
                        age_month+ interval(treatment_time, follow_up_time) %/% months(1)>=60,
                        1, 0), 
    age_end=age_month + interval(treatment_time, follow_up_time) %/% months(1),
    age_year=age_month/12,
    event_time_lb=treatment_time  # This is a very rough estimate...
  ) %>% 
  rename(event_time_ub=surveydate) %>% 
  gen_vars("Kirby et al. 2019", "month")

mortality_kirby$event_time_lb[which(mortality_kirby$death == 0)] = NA

# ------ "Kremer et al. 2012" ------ #

mortality_kremer <- data_kremer %>% 
  # Not including treatment group 2 (treatment received in second year?)
  filter(t05!=1) %>% 
  select(child_death_dummy, age_base, evertreat, date_interview) %>% 
  mutate(age_months1=age_base*12,
         # treatment time - springs were protected from Jan-April 2005
         # follow-up time - first round from April-August 2005
         treatment_time=ymd("2005-03-01"),
         follow_up_time=ymd("2005-06-01"),
         event_time_lb=ymd("2005-04-01"), # Unclear how to get this!
         event_time_ub=ymd("2005-08-31"),
         precision_fut="day",
         precision_trt="quarter",
         precision_evt = "quarter",
         crossed_five=ifelse(age_base<5 & 
           age_base+interval(treatment_time, follow_up_time) %/% years(1)>=5,
           1,0)) %>%  # How to get age at end of study?
  rename(wtreatment=evertreat, 
         death=child_death_dummy,
         age_month=age_months1,
         age_year=age_base) %>% 
  gen_vars("Kremer et al. 2012", "years")

mortality_kremer$event_time[which(mortality_kremer$death == 0)] = NA

# ------ "Chiller et al. 2006" ------ #

# Note: For 103 observations, where the dob was missing 
# For those, the EDAD variable is still there and the 
# EDADAC variable takes the value 0. (05/24/2022) -
# SN is replacing EDADAC with EDAD for these cases 

data_chiller_id<-data_chiller_id%>%
  select(MertuID, Cluster)%>%
  rename(Mertuid=MertuID)

mortality_chiller<-data_chiller%>%
  left_join(data_chiller_id)%>%
  mutate(village=substr(Mertuid, 1, 2)) %>%
  select(PersonaID, Cluster, GRUPO, FECNAC, FECENT, village)%>%
  group_by(PersonaID, village, Cluster, GRUPO) %>%
  summarise(dob = max(FECNAC), follow_up_time=max(FECENT)) %>% 
  mutate(death=ifelse(PersonaID == "143245", 1, 0),
         treatment=case_when(
           GRUPO== "CC" ~ 0,
           GRUPO== "CK" ~ 1
         ),
         # Treatment time
         treatment_time="2002-11-04",
         treatment_time=ymd(treatment_time),
         # Follow-up time
         follow_up_time=ymd(follow_up_time),
         precision_fut="day",
         precision_trt ="week",
         precision_evt = "year",
         # Event time (best guess is just range of follow-up times)
         event_time_lb=case_when(PersonaID == "143245" ~ ymd("2002-11-04")),
         event_time_ub=case_when(PersonaID == "143245" ~ ymd("2003-01-27")),
         # Age
         age_month = as.numeric(treatment_time-ymd(dob))/30,
         age_year=age_month/12,
         # crossed the age of 5
         crossed_five=ifelse(
           age_month<60 & age_month + interval(treatment_time, follow_up_time) 
              %/% months(1)>=60, 1, 0),
         ) %>%
  filter(!is.na(treatment) )%>%
  rename(wtreatment=treatment) %>% 
  gen_vars("Chiller et al. 2006", "months") 

mortality_chiller <- mortality_chiller[which(mortality_chiller$age_month < 60 |
                                               mortality_chiller$age_year < 5),]

# ------ "Crump et al. 2005" ------ #

remove<-data_crump%>%
  filter(is.na(errorflag1))%>%
  filter(tw2week=="01" & tw2away==4)%>%
  select(tw2seq)%>%
  distinct()%>%
  mutate(remove=1)

temp<-data_crump%>%
  # Cleaning from Brandon's do-file
  filter(is.na(errorflag1)) %>%
  left_join(remove) %>%
  filter(is.na(remove)) %>%
  select(-remove) %>%
  mutate(compound = substr(tw2seq, 0,6),
         die = ifelse(tw2away==4,1,0),
         tw2date=as.Date(tw2date),
         event_time=ifelse(die==1, tw2date, NA),
         event_time=as.Date(event_time, origin = "1970-01-01")
         ) %>% 
  group_by(tw2seq,compound,tw2studyarm) %>% 
  summarize(death = max(die),
            minage=min(tw2age),
            event_time_ub=min(event_time, na.rm = TRUE),
            event_time_lb=as.Date(event_time_ub - 7, origin = "1970-01-01"),
            follow_up_time = max(as.Date(tw2date, origin = "1970-01-01")))%>% 
  ungroup() %>% 
  filter(tw2seq!="015086G005", tw2seq!="157021A011", tw2seq!="009022B004", 
         tw2seq!="015080D006", tw2seq!="123140E004", tw2seq!="149001B004",
         tw2seq!="102052D003", tw2seq!="010129A014", tw2seq!="111036C008") %>%  
  select(tw2seq,death, minage, event_time_ub, event_time_lb, follow_up_time)

mortality_crump<-data_crump %>% 
  filter(tw2age<5) %>%
  select(tw2seq,tw2v,tw2c,tw2h,i,tw2studyarm) %>%
  distinct() %>%
  left_join(temp) %>%
  mutate(die=ifelse(is.na(death),0,death)) %>%
  mutate(compound = paste0(tw2v,tw2c)) %>%
  select(die,compound,tw2studyarm, minage, event_time_ub, event_time_lb, follow_up_time) %>%
  #Combining into one treatment group
  mutate(tw2studyarm=case_when(
    tw2studyarm==3 ~ 1,
    tw2studyarm==2 ~ 1,
    tw2studyarm==1 ~ 0),
    age_measure_original="years",
    study=" Crump et al. 2005")%>% 
  rename(wtreatment=tw2studyarm,
         death=die, 
         age_year=minage)%>% 
  mutate(
    treatment_time=ymd("2003-04-08"),
    precision_fut="week",
    precision_trt ="month",
    precision_evt = "week",
    age_month=age_year*12,
    # crossed the age of 5
    crossed_five=ifelse(
      age_year<5 & age_year+interval(treatment_time, follow_up_time) %/% years(1)>5,
      1, 0
    ))%>% 
  gen_vars("Crump et al. 2005", "year")

mortality_crump$event_time_lb[which(mortality_crump$death == 0)] = NA
mortality_crump$event_time_ub[which(mortality_crump$death == 0)] = NA

# ------ "Luby et al. 2006" ------ #

deaths_luby2006 <- deaths_luby2006 %>% 
  mutate(ChildID=as.numeric(ChildID)) %>% 
  rename(ChildId=ChildID,
         DOB_death=DOB)  

mortality_luby2006<-data_luby2006 %>%
  mutate(DOB=str_replace_all(DOB, "/6", "/196"), 
         DOB=str_replace_all(DOB, "/5", "/195"), 
         DOB=str_replace_all(DOB, "/4", "/194"), 
         DOB=str_replace_all(DOB, "/3", "/193"),
         DOB=str_replace_all(DOB, "/196/", "/6/"), 
         DOB=str_replace_all(DOB, "/195/", "/5/"), 
         DOB=str_replace_all(DOB, "/194/", "/4/"), 
         DOB=str_replace_all(DOB, "/193/", "/3/"), 
         DOB=str_replace_all(DOB, "/1930/", "/30/"),
         DOB=str_replace_all(DOB, "/1931/", "/31/"), DOB=mdy(DOB)) %>% 
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
         event_time_lb=ymd(event_time),
         event_time_ub=event_time_lb,
         age_month=age_year*12,
         follow_up_time=ymd("2003-12-22"),
         precision_fut = "week",
         precision_trt = "month", 
         precision_evt = "day",
         follow_up_age = follow_up_time - DOB,
         follow_up_age = as.numeric(follow_up_age)/365,
         crossed_five = ifelse(follow_up_age > 5, 1, 0)) %>% 
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
    agegroup=max(agegroup),
    follow_up_time = max(mdy(fecent)),
    fecnac = max(fecnac)
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
    event_time_ub=case_when(
      clust_level=="07054B" & child_order==1 ~ "11/10/2001",
      clust_level=="11282" & child_order==1 ~ "11/16/2001",
      clust_level=="12348" & child_order==1 ~ "2/15/2002",
      clust_level=="04024" & child_order==1 ~ "5/24/2002",
      clust_level=="12338" & child_order==1 ~ "",
      clust_level=="09145C" & child_order==1 ~ "7/13/2002",
      clust_level=="09234" & child_order==1 ~ "7/17/2002",
      clust_level=="09175" & child_order==1~ "8/1/2002",
      clust_level=="22704" & child_order==1~ "8/17/2002",
      clust_level=="12134" & child_order==1~ ""
    ),
    event_time_lb=event_time_ub,
    death=ifelse(is.na(death), 0, death),
    fecnac=str_replace_all(fecnac, "/6", "/196"), 
    fecnac=str_replace_all(fecnac, "/5", "/195"), 
    fecnac=str_replace_all(fecnac, "/4", "/194"), 
    fecnac=str_replace_all(fecnac, "/3", "/193"),
    fecnac=str_replace_all(fecnac, "/15", "/1915"),
    fecnac=str_replace_all(fecnac, "/23", "/1923"),
    fecnac=str_replace_all(fecnac, "/25", "/1925"),
    fecnac=str_replace_all(fecnac, "/26", "/1926"),
    fecnac=str_replace_all(fecnac, "/28", "/1928"),
    fecnac=str_replace_all(fecnac, "/29", "/1929"),
    fecnac=str_replace_all(fecnac, "/196/", "/6/"), 
    fecnac=str_replace_all(fecnac, "/195/", "/5/"), 
    fecnac=str_replace_all(fecnac, "/194/", "/4/"), 
    fecnac=str_replace_all(fecnac, "/193/", "/3/"), 
    fecnac=str_replace_all(fecnac, "/1930/", "/30/"),
    fecnac=str_replace_all(fecnac, "/1931/", "/31/"),
    fecnac=str_replace_all(fecnac, "/1915/", "/15/"),
    fecnac=str_replace_all(fecnac, "/1923/", "/23/"),
    fecnac=str_replace_all(fecnac, "/1925/", "/25/"),
    fecnac=str_replace_all(fecnac, "/1926/", "/26/"),
    fecnac=str_replace_all(fecnac, "/1928/", "/28/"),
    fecnac=str_replace_all(fecnac, "/1929/", "/29/"), 
    fecnac=mdy(fecnac))


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
  mutate(treatment_time=ymd("2001-08-01"),
         event_time_ub = mdy(event_time_ub),
         event_time_lb = mdy(event_time_lb),
         age_month=as.numeric(treatment_time - fecnac)/30,
         age_year=age_month/12,
         precision_fut = "week",
         precision_trt = "month",
         precision_evt = "day",
         crossed_five = 0) %>% 
  gen_vars("Reller et al. 2003", "month")

# ------ "Peletz et al. 2012" ------ #
data_peletz<-read_dta("mortality_counts/Peletz et al. 2012/Zambia data 7Feb19 short.dta")
data_peletz$visit[which(is.na(data_peletz$dateinte))] = 0
names(data_peletz)[50] <- "agedays_b"
data_peletz$agedays_b[which(is.na(data_peletz$agedays_b))] = 0
data_peletz$firstdateinte = data_peletz$dateinte
data_peletz$firstdateinte[which(data_peletz$visit != 1)] = "2061-01-01"
data_peletz$dateinte[which(is.na(data_peletz$dateinte))] = "1961-01-01"
data_peletz$month = month(data_peletz$dateinte)

mortality_peletz<-data_peletz%>%
  # 121 "child 1" (+twin in one household)
  filter((child==1 & id!=2019) | ((child==1 | child==2) & id==2019))%>% 
  group_by(person, id)%>%
  summarise(
    babydied=first(babydied),
    datedied=max(datedied, na.rm=T),
    arm=first(arm),
    diarr=first(diarr),
    communit=first(communit),
    age_month=max(agedays_b/30),
    age_year=age_month/12,
    follow_up_time = max(dateinte),
    visit = max(visit),
    month = month(min(firstdateinte))
  )%>%
  rename(death=babydied,
         wtreatment=arm,
         strata=communit,
         event_time_ub=datedied)%>% 
  mutate(
    treatment_time=ifelse(month == 12 | (month == 1 & visit > 0), "2010-12-01", NA),
    treatment_time=ifelse(month == 11, "2010-11-01", treatment_time),
    treatment_time=ifelse(month == 10, "2010-10-01", treatment_time),
    treatment_time=ifelse(month == 9, "2010-09-01", treatment_time),
    treatment_time=ifelse(month == 8, "2010-08-01", treatment_time),
    treatment_time=ifelse(month <= 7 & month > 1, "2010-06-01", treatment_time),
    treatment_time=ymd(treatment_time),
    precision_trt =ifelse(treatment_time == "2010-06-01", "quarter", "month"),
    precision_evt = "day",
    crossed_five = 0,
    event_time_ub=dmy(event_time_ub)
  ) %>% 
  select(-c(visit, month)) %>%
  gen_vars("Peletz et al. 2012", "month")

mortality_peletz$follow_up_time[which(mortality_peletz$follow_up_time == "1961-01-01")] = NA
mortality_peletz$precision_fut = ifelse(is.na(mortality_peletz$follow_up_time), NA, "day")
mortality_peletz$event_time_ub[which(mortality_peletz$death != 1)] = NA
mortality_peletz$event_time_ub[which(is.na(mortality_peletz$death))] = NA
mortality_peletz$event_time_lb = mortality_peletz$event_time_ub
data_peletz$dateinte[which(data_peletz$dateinte == "1961-01-01")] = NA

# ------ "Null et al. 2018" ------ #
data_null <- transform(data_null,baseline=paste0(year_baseline, week_baseline))
data_null <- transform(data_null,survey=paste0(year_survey, week_survey))

data_null<-data_null%>%
  select(clusterid, compoundid, hhid, childid, aged, agem, agey,tr,
         baseline, survey, time)%>%
  group_by(clusterid, compoundid, hhid, childid)%>%
  summarize(baseline = max(baseline), 
            survey = max(survey),  # Month/week of last follow-up
            survey0 = min(survey),  # Month/week of first survey
            clusterid = max(clusterid),
            compoundid = max(compoundid), 
            hhid = max(hhid), 
            childid = max(childid),
            time = min(time),  # Time of first survey (0 = baseline)
            agem = min(agem),  # Age measured at first survey
            tr = max(tr))%>%
  ungroup()

data_null$survey[which(data_null$survey == data_null$baseline)] = NA


mortality_null <- data_null_death%>%
  mutate(wtreatment=ifelse(tr==2,1,0),
         ac=ifelse(tr==1,1,0),
         pc=ifelse(tr==8,1,0))%>%
  rename(death=childdeath) %>% 
  mutate(in_mort_df = TRUE) %>%
  filter(wtreatment==1 | ac==1 | pc==1)%>%
  left_join(data_null)%>%  # Note: some children are in mortality but not main data!
  mutate(year_baseline=substring(as.character(baseline), 1, 4),
         week_baseline=substring(as.character(baseline), 5),
         year_survey=substring(as.character(survey), 1, 4),
         week_survey=substring(as.character(survey), 5),
         year_survey0=substring(as.character(survey0), 1, 4),
         week_survey0=substring(as.character(survey0), 5),
         treatment_time=as.Date(paste(year_baseline, week_baseline, 1, sep="-"), "%Y-%U-%u"),
         follow_up_time=as.Date(paste(year_survey, week_survey, 1, sep="-"), "%Y-%U-%u"),
         survey0_time=as.Date(paste(year_survey0, week_survey0, 1, sep="-"), "%Y-%U-%u"),
         event_time_ub=follow_up_time,
         event_time_lb=treatment_time,
         precision_trt ="month",
         precision_fut ="week",
         precision_evt = "year",
         crossed_five = 0,
         age_month = agem - as.numeric(survey0_time-treatment_time)/30,
         age_year = age_month/12) %>% 
  gen_vars("Null et al. 2018", "month") %>%
  mutate(in_mort_df = replace_na(in_mort_df, FALSE))

mortality_null %>%
  filter(death == 1) %>%
  filter(is.na(treatment_time)) %>%
  View()

mortality_null$event_time_ub[which(mortality_null$death == 0)] = NA
mortality_null$event_time_lb[which(mortality_null$death == 0)] = NA

# ------ "Haushofer et al. 2020" ------ #
mortality_haush<-data_haush%>%
  filter(wave==1 & child_yob>=2013 | 
           wave!=1 & child_yob>=2014)%>%
  rename(
    age_month=child_dage,
    wtreatment=waterp,
    death=dead_u5
  ) %>% 
  mutate(event_time_lb=as.Date(paste(child_yod, child_mod, 1, sep="-"), "%Y-%m-%d"),
         end_day = ifelse(child_mod %in% c(4,6,9,11),30,31),
         event_time_ub=as.Date(paste(child_yod, child_mod, end_day, sep="-"), "%Y-%m-%d"),
         follow_up_time=ymd("2018-03-01"),
         treatment_time=case_when(
           wave == 1 ~ "2012-12-01",
           wave == 2 ~ "2013-07-01",
           wave == 3 ~ "2013-09-01",
           wave == 4 ~ "2013-11-01",
           wave == 5 ~ "2014-01-01",
           wave == 6 ~ "2014-03-01",
           wave == 7 ~ "2014-05-01"
         ), treatment_time = ymd(treatment_time),
         age_year=year(treatment_time)-child_yob,
         precision_trt ="month",
         precision_fut ="month",
         precision_evt = "month",
         crossed_five = 0) %>% 
  gen_vars("Haushofer et al. 2020", "month")

# ------ List of all available individual data ------ #
  
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
       "study", "study", "treatment_time", "event_time_lb", "event_time_ub", "follow_up_time", "precision_trt",
       "precision_evt", "precision_fut", "crossed_five")
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
  select(-c(person, PersonaID, village, Cluster, id, wash_effect, fd_effect))

write.csv(mortality_all, file="data/individual_data.csv")


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




