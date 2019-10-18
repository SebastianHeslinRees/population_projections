#Fertility Curves
library(dplyr)
library(tidyr)
library(data.table)
fert_curve_file <- "Q:/Teams/D&PA/Data/population_projections/ons_snpp/2016-based (May 2018)/model_inputs/2016 snpp sya fert rates.csv"
national_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/national asfrs.csv"
over90_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/NPP deaths over 90.csv"

#Data for English LAs
ons_fert <- data.table::fread(fert_curve_file) %>%
  gather(year, fert_rate, 4:28) %>%
  filter(year == 2017) %>%
  mutate(sex = "female") %>%
  select(gss_code, sex, age, fert_rate)

#Data for Wales, Scotland and NI
national_fert <- data.table::fread(national_file) %>%
  mutate(fert_rate = fertility_rate/100000) %>%
  mutate(sex = "female") %>%
  select(gss_code, sex, age, fert_rate)

#Wales LA data is the Welsh national rates applied to each LA
#TODO: Can we find the actual data for Welshj LAs? 
welsh_gss_codes <- c(paste0("W0600000",1:6),paste0("W0600000",8:9),paste0("W060000",10:16),paste0("W060000",18:24))
wales <- list()
for(w in seq(welsh_gss_codes)){
  wales[[w]] <- filter(national_fert, gss_code == "W92000004") %>%
    mutate(gss_code = welsh_gss_codes[w])
}
wales <- data.table::rbindlist(wales)

national_fert <- filter(national_fert, gss_code != "W92000004")

#Recode - takes a simple average when rates have to to be aggregated
ons_fert <- rbind(ons_fert, national_fert, wales) %>%
  mutate(year = 2017) %>%
  select(gss_code, sex, age, year, fert_rate) %>%
  recode_gss_to_2011(col_geog = "gss_code", col_aggregation = c("gss_code", "sex", "age", "year"), fun = list(mean))

smoothed_curves <- fit_fertility_rates(ons_fert)

dir.create("input_data/fertility", recursive = TRUE, showWarnings = FALSE)
saveRDS(smoothed_curves, "input_data/fertility/ons_asfr_curves.rds" )

