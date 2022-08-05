## Clean the raw ZM data dump
library(haven)
library(tidyverse)
library(data.table)

raw_dt = read_dta("data/input/Dummy Data_ZM (1).dta") %>%
    as.data.table()

cols_we_want = str_detect(
    colnames(raw_dt), 
    "^bcg|penta|measles|childid"
)
wip_dt = raw_dt[, ..cols_we_want]
# vaccinated => vaxxed
# retro => vaccinated somewhere else and know date
# retro_date_missing => vaxxed elsewhere, don't know the date
# outreach => vaccinated in outreach programme
# scheduled => scheduled, date missing  

vaccines = c(
    "bcg", 
    "measles1", 
    "measles2", 
    "penta1", 
    "penta2"
)

col_variable_type = c(
    "vaccinationstatus", 
    "vaccinationagedays",
    "vaccinationeventtype", 
    "district",
    "town",
    "uc",
    "centername",
    "vaccinator",
    "vaccinationduedate",
    "vaccinationdate",
    "due_date",
    "vac_date",
    "timelinessfactor",
    "approvedreminder",
    "eventtype",
    "vaccinereminder1status",
    "vaccinereminder2status",
    "vaccinereminder3status",
    "vaccinereminder1duedate",
    "vaccinereminder2duedate",
    "vaccinereminder3duedate"
)





raw_dt[, ..vaccines]

wip_dt[ , (vaccines) := lapply(.SD, str_detect, paste0(str_to_title(vaccines), collapse = "|")), .SDcols = vaccines]
wip_dt[, ..vaccines]
wip_dt[, ..vaccines]
bcg_vaxs = colnames(wip_dt)[str_detect(colnames(wip_dt), "bcg|child")]

bcg_vaxs

wip_dt[, ..bcg_vaxs]


melt_vars = map(col_variable_type, ~str_c(vaccines, .x))
melt_vars


ed = melt(
    wip_dt, 
    id.vars = "childid",
    measure.vars =  melt_vars, 
    value.name = col_variable_type
)



ed = melt(
    wip_dt,
    id = "childid"
)


ed[, vaccine := vaccines[variable]]

ed


town_dt = wip_dt[, 
                 .(vaccines = sum(.SD)), 
                 .SDcols = vaccines, 
                 by =   ]


raw_df %>%
    colnames()
raw_df %>%
    select( 
        childid, 
        contains("district"), 
        contains("town"), 
        contains("")

    )

raw_df %>%
    select( 
        bcgvaccinationdate, 
        bcgvac_date
    )
colnames(raw_df)
raw_df %>%
    group_by(bcgdistrict, bcgvaccinationdate) %>%
    summarise( 
        district_n = n()
    ) %>%
    mutate( 
        bcgvaccinationdate = lubridate::ymd_hms(bcgvaccinationdate)
    ) %>%
    ggplot(aes( 
        x = bcgvaccinationdate, 
        y = district_n, 
        colour = bcgdistrict
    )) + 
    geom_line()


raw_df %>%
    select(contains("bcg")) %>%
    View()
raw_df %>%
    colnames()
raw_df %>%
    View()

raw_df %>%
    View()

raw_dt = fread("relative-path-to-data.data")
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)


install_cmdstan()
check_cmdstan_toolchain(fix = TRUE)
