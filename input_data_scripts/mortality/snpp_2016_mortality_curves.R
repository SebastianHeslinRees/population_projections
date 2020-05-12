#Mortality Curves
library(dplyr)
library(tidyr)
library(data.table)
mort_curve_file <- "Q:/Teams/D&PA/Data/population_projections/ons_snpp/2016-based (May 2018)/model_inputs/2016 snpp sya mortality rates.csv"
national_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/national asmrs.csv"
over90_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/NPP deaths over 90.csv"

#Data for English LAs
ons_mort <- data.table::fread(mort_curve_file) %>%
  data.frame() %>%
  gather(year, death_rate, 5:29) %>%
  filter(year == 2017)%>%
  mutate(sex = ifelse(sex=="M","male","female")) %>%
  select(gss_code, sex, age, death_rate)

#Data for Wales, Scotland and NI
#npp mortality data is published as deaths per 100k of population
#Also ages are offset by -1 year of age
#Death data are provided up to 125 so data can safely be filter to < age 90
#without the need to aggregate the 90+ age group following the age shift
national_mort <- data.table::fread(national_file) %>%
  data.frame() %>%
  mutate(age = ifelse(age == "Birth", -1, age),
         age = as.numeric(age),
         age = age+1) %>%
  filter(age < 90) %>%
  mutate(death_rate = mortality_rate/100000) %>%
  mutate(sex = ifelse(sex=="M","male","female")) %>%
  select(-mortality_rate)

over90s <- data.table::fread(over90_file) %>%
  data.frame() %>%
  mutate(death_rate = deaths/pop) %>%
  mutate(sex = ifelse(sex=="M","male","female")) %>%
  select(-pop, -deaths)

national_mort <- rbind(national_mort, over90s)

#Wales LA data is the Welsh national rates applied to each LA
#TODO: Can we find the actual data for Welshj LAs? 
welsh_gss_codes <- c(paste0("W0600000",1:6),paste0("W0600000",8:9),paste0("W060000",10:16),paste0("W060000",18:24))
wales <- list()
for(w in seq(welsh_gss_codes)){
  wales[[w]] <- filter(national_mort, gss_code == "W92000004") %>%
    mutate(gss_code = welsh_gss_codes[w])
}
wales <- data.table::rbindlist(wales) %>%
  data.frane()

national_mort <- filter(national_mort, gss_code != "W92000004")

#Recode - takes a simple average when rates have to to be aggregated
ons_mort <- rbind(ons_mort, national_mort, wales) %>%
  mutate(year = 2017) %>%
  select(gss_code, sex, age, year, rate = death_rate) %>%
  popmodules::recode_gss_codes(col_geog = "gss_code", col_aggregation = c("gss_code", "sex", "age", "year"), fun = list(mean))

dir.create("input_data/mortality", recursive = TRUE, showWarnings = FALSE)
saveRDS(ons_mort, "input_data/mortality/ons_asmr_curves_2016.rds" )

