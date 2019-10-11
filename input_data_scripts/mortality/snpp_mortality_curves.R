#Mortality Curves
library(dplyr)
library(tidyr)
mort_curve_file <- "Q:/Teams/D&PA/Data/population_projections/ons_snpp/2016-based (May 2018)/model_inputs/2016 snpp sya mortality rates.csv"
national_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/national asmrs.csv"
over90_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/NPP deaths over 90.csv"

ons_mort <- fread(mort_curve_file) %>%
  gather(year, death_rate, 5:29) %>%
  filter(year == 2017)%>%
  mutate(sex = ifelse(sex=="M","male","female")) %>%
  select(gss_code, sex, age, death_rate)

national_mort <- fread(national_file) %>%
  mutate(age = ifelse(age == "Birth", -1, age),
         age = as.numeric(age),
         age = age+1) %>%
  filter(age < 90) %>%
  mutate(death_rate = mortality_rate/100000) %>%
  mutate(sex = ifelse(sex=="M","male","female")) %>%
  select(-mortality_rate)

over90s <- data.table::fread(over90_file) %>%
  mutate(death_rate = deaths/pop) %>%
  mutate(sex = ifelse(sex=="M","male","female")) %>%
  select(-pop, -deaths)

ons_mort <- rbind(ons_mort, national_mort, over90s) %>%
  mutate(year = 2017) %>%
  select(gss_code, sex, age, year, death_rate)
dir.create("input_data/mortality", recursive = TRUE, showWarnings = FALSE)
saveRDS(ons_mort, "input_data/mortality/ons_asmr_curves.rds" )

