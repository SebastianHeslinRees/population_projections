#Fertility Curves
library(dplyr)
library(tidyr)
library(data.table)
library(popmodules)

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
  select(gss_code, sex, age, fert_rate) %>%
  popmodules::recode_gss_to_2011(col_geog = "gss_code",
                                 col_aggregation = c("gss_code", "sex", "age"),
                                 fun = list(mean)) %>%
  as.data.frame()

# Rename col and sex to "F" is so that the fit_fertility_rates function can work
# TODO change the fit_fertility_rates function to accept "fert_rate" and "female"
ons_fert <- ons_fert %>%
  filter(age < 45)
smoothed_curves <- smooth_fertility(ons_fert)$data %>%
  mutate(year = 2017) %>%
  rename(rate = fert_rate) %>%
  mutate(gss_code = as.character(gss_code))
  
validate_population(smoothed_curves, col_data = "rate")


dir.create("input_data/fertility", recursive = TRUE, showWarnings = FALSE)
saveRDS(smoothed_curves, "input_data/fertility/ons_asfr_curves.rds" )


rm(ons_fert, national_fert, wales,fert_curve_file, national_file, over90_file, w, welsh_gss_codes, smoothed_curves)
