#Fertility Curves
library(dplyr)
library(tidyr)
library(data.table)
library(popmodules)

message('2018-based SNPP fertility curves')

fert_curve_file <- "Q:/Teams/D&PA/Data/population_projections/ons_snpp/2018-based (March 2020)/model_inputs/2018 snpp sya fert rates.csv"
national_file <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2018-based NPP/model_inputs/national asfrs.csv"

#Data for English LAs
ons_fert <- data.table::fread(fert_curve_file) %>%
  data.frame() %>%
  mutate(sex = "female") %>%
  select(gss_code, sex, age, fert_rate)

#Data for Wales, Scotland and NI
national_fert <- data.table::fread(national_file) %>%
  data.frame() %>%
  mutate(fert_rate = fertility_rate/1000) %>%
  select(gss_code, sex, age, fert_rate)

#Wales LA data is the Welsh national rates applied to each LA
#TODO: Can we find the actual data for Welshj LAs? 
welsh_gss_codes <- c(paste0("W0600000",1:6),paste0("W0600000",8:9),paste0("W060000",10:16),paste0("W060000",18:24))
wales <- list()
for(w in seq(welsh_gss_codes)){
  wales[[w]] <- filter(national_fert, gss_code == "W92000004") %>%
    mutate(gss_code = welsh_gss_codes[w])
}
wales <- data.table::rbindlist(wales) %>%
  data.frame()

national_fert <- filter(national_fert, gss_code != "W92000004")

#Recode - takes a simple average when rates have to to be aggregated
ons_fert <- rbind(ons_fert, national_fert, wales) %>%
  as.data.frame() %>%
  select(gss_code, sex, age, fert_rate) %>%
  recode_gss_codes(col_geog = "gss_code",
                   data_cols = "fert_rate",
                   fun = "mean",
                   recode_gla_codes = TRUE,
                   recode_to_year = 2021) %>%
  as.data.frame()

#smooth curves
ons_fert <- ons_fert %>%
  filter(age < 45)

smoothed_curves <- smooth_fertility(ons_fert)$data %>%
  mutate(year = 2019) %>%
  rename(rate = fert_rate) %>%
  mutate(gss_code = as.character(gss_code))

validate_population(smoothed_curves, col_data = "rate",
                    test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)


dir.create("input_data/fertility", recursive = TRUE, showWarnings = FALSE)
saveRDS(smoothed_curves, "input_data/fertility/ons_asfr_curves_2018.rds" )


rm(ons_fert, national_fert, wales,fert_curve_file, national_file, w, welsh_gss_codes, smoothed_curves)
