library(assertthat)
library(dplyr)
library(popmodules)

ons_stage1 <- readRDS("input_data/household_model/ons_household_representative_rates.rds")
ons_stage2 <- readRDS("input_data/household_model/ons_headship_rates_2016.rds")
communal_est <- readRDS("input_data/household_model/ons_communal_establishment_population.rds")
dclg_stage1 <- readRDS("input_data/household_model/dclg_stage1_data_2014.rds")
dclg_stage2 <- readRDS("input_data/household_model/dclg_headship_rates_2014.rds")

ons_stage1 <- recode_gss_codes(ons_stage1,
                               data_cols = "HRR",
                               fun = list(mean),
                               recode_to_year = 2020)
ons_stage2 <- recode_gss_codes(ons_stage2,
                               data_cols = "rate",
                               fun = list(mean),
                               recode_to_year = 2020)
assert_that(!any(is.na(ons_stage1)))
assert_that(!any(is.na(ons_stage2)))


communal_est_pop <- communal_est %>%
  select(-ce_rate) %>%
  recode_gss_codes(data_cols = "ce_pop",
                   col_geog="gss_code",
                   recode_to_year = 2020)

communal_est_rate <- communal_est %>% 
  select(-ce_pop) %>%
  recode_gss_codes(data_cols = "ce_rate",
                   col_geog="gss_code",
                   fun = list(mean),
                   recode_to_year = 2020)

assert_that(!any(is.na(communal_est_pop)))

communal_est <- left_join(communal_est_pop, communal_est_rate,
                          by = c("gss_code","age_group","sex","year"))

dclg_stage1_pops <- dclg_stage1 %>%
  select(-hh_rep_rates) %>%
  recode_gss_codes(data_cols = c("households","household_population",
                                 "institutional_population",
                                 "total_population"),
                   col_geog="gss_code",
                   recode_to_year = 2020)

dclg_stage1_rates <- dclg_stage1 %>%
  select(-households, -household_population, -institutional_population, -total_population) %>%
  recode_gss_codes(data_cols = "hh_rep_rates",
                   col_geog="gss_code",
                   fun=list(mean),
                   recode_to_year = 2020)

dclg_stage1 <- left_join(dclg_stage1_pops, dclg_stage1_rates,
                         by = c("gss_code","year","sex","household_type","age_group"))

dclg_stage2 <- dclg_stage2 %>%
  recode_gss_codes(data_cols = "rate",
                   col_geog="gss_code",
                   fun=list(mean),
                   recode_to_year = 2020)

saveRDS(ons_stage1,"input_data/household_model/ons_household_representative_rates_(2020_geog).rds")
saveRDS(ons_stage2,"input_data/household_model/ons_headship_rates_2016_(2020_geog).rds")
saveRDS(communal_est,"input_data/household_model/ons_communal_establishment_population_(2020_geog).rds")
saveRDS(dclg_stage1,"input_data/household_model/dclg_stage1_data_2014_(2020_geog).rds")
saveRDS(dclg_stage2,"input_data/household_model/dclg_headship_rates_2014_(2020_geog).rds")
