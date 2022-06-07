library(assertthat)
library(dplyr)
library(popmodules)

message("household model inputs (2021 geography)")

ons_stage1 <- readRDS("input_data/household_model/ons_household_representative_rates.rds")
ons_stage2 <- readRDS("input_data/household_model/ons_headship_rates_2016.rds")
dclg_stage1 <- readRDS("input_data/household_model/dclg_stage1_data_2014.rds")
dclg_stage2 <- readRDS("input_data/household_model/dclg_headship_rates_2014.rds")

ons_stage1 <- recode_gss_codes(ons_stage1,
                               data_cols = "HRR",
                               fun = "mean",
                               recode_to_year = 2021)
ons_stage2 <- recode_gss_codes(ons_stage2,
                               data_cols = "rate",
                               fun = "mean",
                               recode_to_year = 2021)
assert_that(!any(is.na(ons_stage1)))
assert_that(!any(is.na(ons_stage2)))

dclg_stage1_pops <- dclg_stage1 %>%
  select(-hh_rep_rates) %>%
  recode_gss_codes(data_cols = c("households","household_population",
                                 "institutional_population",
                                 "total_population"),
                   col_geog="gss_code",
                   recode_to_year = 2021)

dclg_stage1_rates <- dclg_stage1 %>%
  select(-households, -household_population, -institutional_population, -total_population) %>%
  recode_gss_codes(data_cols = "hh_rep_rates",
                   col_geog="gss_code",
                   fun="mean",
                   recode_to_year = 2021)

dclg_stage1 <- left_join(dclg_stage1_pops, dclg_stage1_rates,
                         by = c("gss_code","year","sex","household_type","age_group"))

dclg_stage2 <- dclg_stage2 %>%
  recode_gss_codes(data_cols = "rate",
                   col_geog="gss_code",
                   fun="mean",
                   recode_to_year = 2021)

source("input_data_scripts/households/communal_est_popn_(2021 geog).R")

saveRDS(ons_stage1,"input_data/household_model/ons_household_representative_rates_(2021_geog).rds")
saveRDS(ons_stage2,"input_data/household_model/ons_headship_rates_2016_(2021_geog).rds")
saveRDS(dclg_stage1,"input_data/household_model/dclg_stage1_data_2014_(2021_geog).rds")
saveRDS(dclg_stage2,"input_data/household_model/dclg_headship_rates_2014_(2021_geog).rds")

rm(list=ls())
