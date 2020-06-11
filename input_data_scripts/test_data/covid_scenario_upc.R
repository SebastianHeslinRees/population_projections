###UPC
library(dplyr)
devtools::load_all('model_code/popmodules')

#Hard coded data correct as of 21/05/20

#covid data repo
covid_repo <- "Q:/Projects/2019-20/Covid-19_public_facing/londonC19dashboard/data/updated/"

#ons data by LA
ons_file <- "ons_deaths_weekly_occurrences_by_la_2020-05-15"

#FIXME
#This excel workbook has deaths by sex and broad age for England
#nhse_file <- "nhse_deaths_total_2020-05-20"
#Sheet is COVID19 total deaths by age
#I don't have time to figure out how to extract that data properly
#So I'm hard-coding it for now
#Covid19 deaths for UK by age group and sex
covid_eng_age_sex <- data.frame(min_age = c(0,20,40,60,80),
                                max_age = c(19,39,59,79,90),
                                male = c(8, 99,1268, 6155,7218),
                                female = c(4,73,637,3118,5372))

#Covid19 total deaths by LA (E&W)
#Recode gss codes so they're consitent with othre inputs
covid_la_total <- data.table::fread(paste0(covid_repo,"processed/",ons_file,".csv")) %>%
  as.data.frame() %>%
  filter(cause_of_death == "COVID 19") %>% 
  group_by(gss_code = area_code) %>%
  summarise(la_total = sum(deaths)) %>%
  as.data.frame() %>%
  filter(substr(gss_code,1,3) %in% c("E06","E07","E08","E09","W06","S92","N92")) %>% 
  recode_gss_codes(data_cols = "la_total", recode_to_year=2020)

#FIXME
#This is ONS data so its just E&W
#I don't have time to code the N and S data so I'm hard coding it here
scotland <- 2184
nire <- 578
england <- sum(covid_eng_age_sex$male)+sum(covid_eng_age_sex$female)
wales <- 1238

#These numbers don't add up to the official UK total ad I honestly don't care anymore
latest_uk_figure <- 35704

covid_la_total <- data.frame(gss_code = c("S92000003","N92000002"),
                             la_total = c(scotland,nire),
                             stringsAsFactors = FALSE) %>%
  rbind(covid_la_total)

#2018 Mid-year estimates age structure for LAs by sex
uk_2018_deaths <- readRDS("input_data/mye/2018/deaths_ons.rds") %>%
  filter(year == 2018) %>%
  group_by(sex, age) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()

#-----------------------------------------

#Apply 2018 age structure to covid19 age group data
england_covid_deaths_sya <- list()
for(i in 1:nrow(covid_eng_age_sex)){
  
  a <- covid_eng_age_sex[i,] %>%
    tidyr::pivot_longer(cols=c("male","female"),
                        names_to = "sex",
                        values_to = "covid_deaths") %>%
    data.frame()
  
  england_covid_deaths_sya[[i]] <- distribute_within_age_band(a,
                                                              uk_2018_deaths,
                                                              "covid_deaths", "deaths",
                                                              unique(a$min), unique(a$max),
                                                              col_aggregation=c("sex"))
}

#Single year of age by sex for england
england_covid_deaths_sya <- data.table::rbindlist(england_covid_deaths_sya) %>%
  as.data.frame() %>% 
  select(sex, age, covid_deaths)

#deaths as a proportion of total by sex
covid_mort_structure <- england_covid_deaths_sya %>%
  mutate(country = "england") %>%
  group_by(country) %>%
  mutate(structure = covid_deaths/sum(covid_deaths)) %>%
  as.data.frame()

#Apply the age structure to the LA/sex data
covid_modelled_sya <- list()
for(i in 1:nrow(covid_la_total)){
  
  covid_modelled_sya[[i]] <- covid_mort_structure %>%
    mutate(deaths = structure * covid_la_total[i,2],
           gss_code = covid_la_total[i,1])
  
}

covid_modelled_sya <- data.table::rbindlist(covid_modelled_sya) %>%
  as.data.frame() %>%
  select(gss_code, sex, age, deaths)

#scale-up to latest data
#Again, no time, hard coding
modelled_figure <- sum(covid_modelled_sya$deaths)

#Future covid deaths
#There are about 6 weeks left in the year from 15/05
#and atm there are ~100 deaths per day in England
#So 100*7*6 more deaths to mid year?

total_2020_deaths <- (100*7*6) + latest_uk_figure

scaling <- total_2020_deaths/modelled_figure

covid_upc <- covid_modelled_sya %>% 
  mutate(upc = deaths*scaling,
         year = 2020) %>%
  select(year, gss_code, sex, age, upc)

#2021?
#Who knows, lets say a second wave has half the deaths of 2020

covid_2021 <- covid_upc %>%
  mutate(year = 2021,
         upc = upc*0.5)

covid_upc <- rbind(covid_upc, covid_2021) %>%
  mutate(upc = upc*-1) %>%
  recode_gss_codes(data_cols = "upc", recode_to_year = 2020)
  
#save
dir.create("input_data/scenario_data", showWarnings = FALSE)
saveRDS(covid_upc, "input_data/scenario_data/covid19_upc.rds")

