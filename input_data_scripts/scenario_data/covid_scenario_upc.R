library(dplyr)
library(popmodules)

#DATA To WEEK 27 (03/July/20202) - Mid Year

####PROCESS####
#1. Download data from ONS using 2 links below
#2. Edit in excel and save as csvs
#3, Edit file paths (line 22-23)
#4. Go to NISRA and NRS sites and find national totals (links below)
#5. Hard code Sc and NI totals (line 26-27)
#6. Run script


#ONS weekly deaths
#contains sex/age breakdown and total
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales

#by LA
#https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

ons_weekly_file_1 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_age_sex_2020_07_03.csv"
ons_weekly_file_2 <- "Q:/Teams/D&PA/Data/covid19/ons/covid_deaths_la_2020_07_03.csv"

#national deaths sc and ni
#https://www.nisra.gov.uk/publications/weekly-deaths
#https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/weekly-and-monthly-data-on-births-and-deaths/deaths-involving-coronavirus-covid-19-in-scotland
scottish_deaths <- data.frame(gss_code = "S92000003", deaths = 4173, stringsAsFactors = F)
n_irish_deaths <-  data.frame(gss_code = "N92000002", deaths = 844, stringsAsFactors = F)

deaths_sex_age <- data.table::fread(ons_weekly_file_1) %>% data.frame()
deaths_la <- data.table::fread(ons_weekly_file_2) %>%
  data.frame() %>%
  filter(cause_of_death == "COVID 19") %>%
  filter_to_LAs() %>%
  group_by(gss_code) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame() %>%
  recode_gss_codes(recode_to_year = 2020) %>% 
  rbind(scottish_deaths, n_irish_deaths)

sum(deaths_la$deaths)
sum(deaths_sex_age$deaths)


#2018 Mid-year estimates age structure for LAs by sex
uk_2018_deaths <- readRDS("input_data/mye/2019/deaths_ons.rds") %>%
  filter(year == 2019) %>%
  group_by(sex, age) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()

#-----------------------------------------

####SINGLE YEAR OF AGE####

#Apply 2018 age structure to covid19 age group data
sya_covid <- list()
for(i in 1:nrow(deaths_sex_age)){
  a <- deaths_sex_age[i,]
  sya_covid[[i]] <- a %>% 
    distribute_within_age_band(uk_2018_deaths,
                               "deaths", "deaths",
                               unique(a$min), unique(a$max),
                               col_aggregation=c("sex"))
}
sya_covid <- data.table::rbindlist(sya_covid) %>%
  data.frame() %>% 
  mutate(structure = deaths/sum(deaths)) %>%
  select(sex, age, structure)

rm(a)


#-----------------------------------------

####APPLY AGE/SEX STRUCTURE TO LAs####
la_sya <- list()
for(i in 1:nrow(deaths_la)){
  la_sya[[i]] <- sya_covid %>%
    mutate(gss_code = deaths_la[i,1],
           deaths = deaths_la[i,2] * structure) %>%
    select(gss_code, sex, age, deaths)
}
la_sya <- data.table::rbindlist(la_sya) %>% 
  data.frame()

#-----------------------------------------

# current_deaths <- sum(deaths_sex_age$deaths) #this is ons deaths in and out of hospital in E&W where covid-19 is cause of death
# modeled_deaths <- 61920 #this is a modeled figure including excess deaths
# scaling <-  current_deaths / modeled_deaths
scaling <- 1

covid_2020 <- mutate(la_sya, deaths = deaths / scaling) %>%
  mutate(year = 2020)

sum(covid_2020$deaths) # final total deaths

#assume 2nd wave. Half as many deaths, same distribution
covid_2021 <- covid_2020 %>%
  mutate(year = 2021,
         deaths = deaths*0.5)


#------------------------------------------

####SAVE####

covid_upc <- rbind(covid_2020, covid_2021) %>%
  mutate(upc = deaths * -1) %>%
  select(year, gss_code, sex, age, upc)

dir.create("input_data/scenario_data", showWarnings = FALSE)
saveRDS(covid_upc, "input_data/scenario_data/covid19_upc.rds")

rm(list = ls())
