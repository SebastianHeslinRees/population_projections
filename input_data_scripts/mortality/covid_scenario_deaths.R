library(dplyr)
library(popmodules)
library(data.table)

message("covid-19 mortality")

#2020 = DATA To WEEK 27 (03/July/20202) - Mid Year
#2021 = DATA TO WEEK 26 of 2021 (02/July/2021) - Mid Year

####UPDATE PROCESS####
#1. Download data from ONS using 2 links below
#2. Edit in excel 
#3. Replace csvs ons_weekly_age_sex_3 & ons_weekly_la_totals_2 on Q:
#4. Go to NISRA and NRS sites and find national & weekly totals (links below)
#5. Hard code Sc and NI data (41-46)

#ONS weekly deaths
#contains sex/age breakdown and total
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales

#by LA
#https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

#Set Geography Year
geography_year <- 2021

#Mid-2020
ons_weekly_age_sex_1 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_age_sex_mid_2020.csv"

#Calendar 2020 Data
ons_weekly_age_sex_2 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_age_sex_July_to_Dec_2020.csv"
ons_weekly_la_totals_1 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_la_calendar_2020.csv"

#Calendar 2021 Data up to week 26 (2 July 2021)
ons_weekly_age_sex_3 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_age_sex_Jan_to_June_2021.csv"
ons_weekly_la_totals_2 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_la_calendar_2021.csv"

#national deaths sc and ni
#https://datavis.nisra.gov.uk/vitalstatistics/weekly-deaths-dashboard.html
#https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/weekly-and-monthly-data-on-births-and-deaths/deaths-involving-coronavirus-covid-19-in-scotland

#-------------------------------------------------------------------------------
#Deaths to 04/July/2021
scottish_to_mid_2020 <- 4173
scottish_all_to_date <- 10189
scottish_to_mid_2021 <- scottish_all_to_date - scottish_to_mid_2020

#Deaths to 02/July/2021
n_irish_to_mid_2020 <- 844
n_irish_all_to_date <- 2982
n_irish_to_mid_2021 <- n_irish_all_to_date - n_irish_to_mid_2020

scottish_deaths_2020 <- data.frame(gss_code = "S92000003", deaths = scottish_to_mid_2020, stringsAsFactors = F)
n_irish_deaths_2020 <-  data.frame(gss_code = "N92000002", deaths = n_irish_to_mid_2020, stringsAsFactors = F)
scottish_deaths_2021 <- data.frame(gss_code = "S92000003", deaths = scottish_to_mid_2021, stringsAsFactors = F)
n_irish_deaths_2021 <-  data.frame(gss_code = "N92000002", deaths = n_irish_to_mid_2021, stringsAsFactors = F)

#-------------------------------------------------------------------------------

deaths_sex_age_2020 <- fread(ons_weekly_age_sex_1) %>%
  data.frame()

deaths_sex_age_2021 <- rbind(
  data.frame(fread(ons_weekly_age_sex_2)),
  data.frame(fread(ons_weekly_age_sex_3))) %>% 
  group_by(min_age, max_age, sex) %>% 
  summarise(deaths = sum(deaths), .groups = 'drop_last') %>% 
  data.frame()

ons_weekly_la_totals_2020 <- fread(ons_weekly_la_totals_1) %>% data.frame() 
ons_weekly_la_totals_2021 <- fread(ons_weekly_la_totals_2) %>% data.frame()

deaths_la_2020 <- ons_weekly_la_totals_2020 %>%
  filter(cause_of_death == "COVID 19",
         week_number <= 27) %>%
  filter_to_LAs() %>%
  group_by(gss_code) %>%
  summarise(deaths = sum(deaths), .groups = 'drop_last') %>%
  as.data.frame() %>%
  rbind(scottish_deaths_2020, n_irish_deaths_2020) %>% 
  recode_gss_codes(recode_to_year =  geography_year)

deaths_la_2021 <- ons_weekly_la_totals_2020 %>%
  filter(week_number > 27) %>% 
  rbind(ons_weekly_la_totals_2021) %>%
  filter(cause_of_death == "COVID 19") %>%
  filter_to_LAs() %>%
  group_by(gss_code) %>%
  summarise(deaths = sum(deaths), .groups = 'drop_last') %>%
  as.data.frame() %>%
  rbind(scottish_deaths_2021, n_irish_deaths_2021) %>% 
  recode_gss_codes(recode_to_year =  geography_year)

#-------------------------------------------------------------------------------

if(geography_year == 2020){
  #Convert 2021 geography back to 2020 geography
  
  check <- sum(deaths_la_2021$deaths)
  
  E06000061_2021 <- filter(deaths_la_2021, gss_code == "E06000061")
  E06000062_2021 <- filter(deaths_la_2021, gss_code == "E06000062")
  
  E06000061_2020 <- filter(deaths_la_2020, gss_code %in% c("E07000150","E07000152","E07000153","E07000156"))
  E06000062_2020 <- filter(deaths_la_2020, gss_code %in% c("E07000151","E07000154","E07000155"))
  
  E06000061_2021 <- E06000061_2020 %>% mutate(deaths = (deaths / sum(E06000061_2020$deaths))*sum(E06000061_2021$deaths))
  E06000062_2021 <- E06000062_2020 %>% mutate(deaths = (deaths / sum(E06000062_2020$deaths))*sum(E06000062_2021$deaths))
  
  deaths_la_2021 <- filter(deaths_la_2021, !gss_code %in% c("E06000061","E06000062")) %>% 
    rbind(E06000061_2021, E06000062_2021) %>% 
    group_by(gss_code) %>% 
    summarise(deaths = sum(deaths)) %>% 
    data.frame()
  
  assertthat::are_equal(check, sum(deaths_la_2021$deaths))
}

#-------------------------------------------------------------------------------

sum(deaths_la_2020$deaths)
sum(deaths_sex_age_2020$deaths)

sum(deaths_la_2021$deaths)
sum(deaths_sex_age_2021$deaths)

#tidy up
rm(list=setdiff(ls(), c("deaths_la_2020","deaths_la_2021","week_no",
                        "deaths_sex_age_2020","deaths_sex_age_2021")))

#-------------------------------------------------------------------------------

####SINGLE YEAR OF AGE####

#2019 Mid-year estimates age structure for LAs by sex
uk_2019_deaths <- readRDS("input_data/mye/2020/deaths_ons.rds") %>%
  filter(year == 2019) %>%
  group_by(sex, age) %>%
  summarise(deaths = sum(deaths), .groups = 'drop_last') %>%
  as.data.frame()

#Apply 2019 age structure to covid-19 age group data
sya_covid_2020 <- list()
sya_covid_2021 <- list()
for(i in 1:nrow(deaths_sex_age_2020)){
  a <- deaths_sex_age_2020[i,]
  sya_covid_2020[[i]] <- a %>% 
    distribute_within_age_band(uk_2019_deaths,
                               "deaths", "deaths",
                               unique(a$min), unique(a$max),
                               col_aggregation=c("sex"))
  
  b <- deaths_sex_age_2021[i,]
  sya_covid_2021[[i]] <- b %>% 
    distribute_within_age_band(uk_2019_deaths,
                               "deaths", "deaths",
                               unique(b$min), unique(b$max),
                               col_aggregation=c("sex"))
}

sya_covid_2020 <- rbindlist(sya_covid_2020) %>%
  data.frame() %>% 
  mutate(structure = deaths/sum(deaths)) %>%
  select(sex, age, structure)

sya_covid_2021 <- rbindlist(sya_covid_2021) %>%
  data.frame() %>% 
  mutate(structure = deaths/sum(deaths)) %>%
  select(sex, age, structure)

rm(a,b,i)

#-------------------------------------------------------------------------------

####APPLY AGE/SEX STRUCTURE TO LAs####

la_sya_2020 <- list()
la_sya_2021 <- list()

for(i in 1:nrow(deaths_la_2020)){
  la_sya_2020[[i]] <- sya_covid_2020 %>%
    mutate(gss_code = deaths_la_2020[i,1],
           deaths = deaths_la_2020[i,2] * structure) %>%
    select(gss_code, sex, age, deaths)
  
  la_sya_2021[[i]] <- sya_covid_2021 %>%
    mutate(gss_code = deaths_la_2021[i,1],
           deaths = deaths_la_2021[i,2] * structure) %>%
    select(gss_code, sex, age, deaths)
  
}

#-------------------------------------------------------------------------------

covid_2020 <- rbindlist(la_sya_2020) %>% 
  data.frame() %>%
  mutate(year = 2020)

popmodules::validate_population(covid_2020,
                                col_data = "deaths",
                                test_complete = TRUE, 
                                test_unique = TRUE,
                                check_negative_values = TRUE,
                                col_aggregation = c("year","gss_code","sex","age"))

covid_2021 <- rbindlist(la_sya_2021) %>% 
  data.frame() %>%
  mutate(year = 2021)

popmodules::validate_population(covid_2021,
                                col_data = "deaths",
                                test_complete = TRUE, 
                                test_unique = TRUE,
                                check_negative_values = TRUE,
                                col_aggregation = c("year","gss_code","sex","age"))

#-------------------------------------------------------------------------------

covid_deaths <- rbind(covid_2020, covid_2021) %>%
  mutate(upc = deaths * -1) %>%
  select(year, gss_code, sex, age, upc)

#-------------------------------------------------------------------------------

####SAVE####

dir.create("input_data/scenario_data", showWarnings = FALSE)
saveRDS(covid_deaths, "input_data/scenario_data/covid19_deaths.rds")
saveRDS(covid_deaths, "input_data/scenario_data/covid19_deaths_week_27_2021.rds")

#-------------------------------------------------------------------------------

#Report
message("Covid mortality assumptions")
message(paste0("Total UK 2020 deaths: ", sum(covid_2020$deaths),"\n",
               "Total UK 2021 deaths: ", sum(covid_2021$deaths),"\n",
               "Overall UK 2020-2021: ", sum(covid_deaths$upc)*-1))
message(" ")
message(paste0("Total London 2020 deaths: ", sum(filter(covid_2020, substr(gss_code, 1, 3)=="E09")$deaths),"\n",
               "Total London 2021 deaths: ", round(sum(filter(covid_2021, substr(gss_code, 1, 3)=="E09")$deaths),0),"\n",
               "Overall London 2020-2021: ", round(sum(filter(covid_deaths, substr(gss_code, 1, 3)=="E09")$upc)*-1,0)))


#-------------------------------------------------------------------------------

rm(list = ls())
