#Mortality Curves
library(dplyr)
library(tidyr)
library(data.table)

mort_curve_file <- "Q:/Teams/D&PA/Data/population_projections/ons_snpp/2018-based (March 2020)/model_inputs/2018 snpp sya mort rates.csv"
national_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2018-based NPP/model_inputs/national asmrs.csv"
over90_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2018-based NPP/model_inputs/NPP deaths over 90.csv"

#Data for English LAs
ons_mort <- data.table::fread(mort_curve_file) %>%
  data.frame() %>%
  select(gss_code, sex, age, mort_rate)

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
  mutate(mort_rate = mort_rate/100000) %>%
  mutate(sex = ifelse(sex==1,"male","female"))

over90s <- data.table::fread(over90_file) %>%
  data.frame %>%
  select(-pop_2018, -deaths_2019)

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
  data.frame()

national_mort <- filter(national_mort, gss_code != "W92000004")

#Recode - takes a simple average when rates have to to be aggregated
ons_mort <- rbind(ons_mort, national_mort, wales) %>%
  mutate(year = 2019) %>%
  select(gss_code, sex, age, year, rate = mort_rate) %>%
  popmodules::recode_gss_codes(rate = "rate", fun = list(mean),
                               recode_to_year = 2018)

dir.create("input_data/mortality", recursive = TRUE, showWarnings = FALSE)
saveRDS(ons_mort, "input_data/mortality/ons_asmr_curves_2018.rds" )

