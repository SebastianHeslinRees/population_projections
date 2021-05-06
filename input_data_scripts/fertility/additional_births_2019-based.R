# Script to incorporate the 2020 births data in the model.
# This exists because we have 2019 births data before any of the other 2019 MYE data.
# The script uses this to create a UK-wide fertility trajectory to 2050 with the 2019
# rates included in the calculations (for London only).

# The output can then be used as model input by pointing the additional_births_path
# parameter to it in the housing led config script

library(dplyr)
library(popmodules)
library(data.table)

#TODO
#THIS IS TEMPORARY UNTIL BEN's SCRIPT FOR MAKING THIS FILE CAN BE BROUGHT INTO THE REPO
file.copy("Q:/Teams/D&PA/Demography/Projections/population_models/temp/predicted_births_2019-based_projections.rds",
          "input_data/fertility/predicted_births_2019-based_projections.rds", overwrite = TRUE)

message('2019-based model additional births')

popn <- readRDS("input_data/mye/2019/population_gla.rds")

ons_births <- readRDS("input_data/mye/2019/births_ons.rds")

additional_births <- readRDS("input_data/fertility/predicted_births_2019-based_projections.rds")

london_backseries_births <- filter(ons_births, gss_code %in% unique(additional_births$gss_code)) %>%
  filter(age == 0) %>%
  select(names(additional_births)) %>%
  rbind(additional_births)

not_london_backseries_births <- filter(ons_births, !gss_code %in% unique(additional_births$gss_code)) %>%
  filter(age == 0) %>%
  select(names(additional_births)) 

scotland_n_ireland <- list()

for(method in c("average", "trend")) {
  london_fert_rates <- scaled_fertility_curve(popn_mye_path = popn,
                                              births_mye_path = london_backseries_births,
                                              target_curves_filepath = "input_data/fertility/ons_asfr_curves_2018_(2020_geog).rds",
                                              last_data_year = 2020,
                                              n_years_to_avg = 5,
                                              avg_or_trend = method,
                                              data_col = "births",
                                              output_col = "rate") %>%
    project_rates_npp(rate_col = "rate",
                      rate_trajectory_filepath = "input_data/fertility/npp_fertility_trend.rds",
                      first_proj_yr = 2021,
                      n_proj_yr = 30,
                      npp_var = "2018_principal")
  
  not_london_fert_rates <- scaled_fertility_curve(popn_mye_path = popn,
                                                  births_mye_path = not_london_backseries_births,
                                                  target_curves_filepath = "input_data/fertility/ons_asfr_curves_2018_(2020_geog).rds",
                                                  last_data_year = 2019,
                                                  n_years_to_avg = 5,
                                                  avg_or_trend = method,
                                                  data_col = "births",
                                                  output_col = "rate") %>%
    project_rates_npp(rate_col = "rate",
                      rate_trajectory_filepath = "input_data/fertility/npp_fertility_trend.rds",
                      first_proj_yr = 2020,
                      n_proj_yr = 31,
                      npp_var = "2018_principal")
  
  fertility_rates <- rbind(london_fert_rates, not_london_fert_rates) %>% 
    complete_fertility(popn)
  
  scotland_n_ireland[[method]] <- filter(fertility_rates, gss_code %in% c("S92000003","N92000002"))
  
  filename_suffix <- ifelse(method == "average", "_5yr_avg", "_5yr_trend")
  saveRDS(fertility_rates, paste0("input_data/fertility/fertility_rates_inc_2020_in_london",filename_suffix,".rds"))
}

rm(list=setdiff(ls(), c("popn", "ons_births", "scotland_n_ireland")))

#-------------------------------

#ONS Provisional births by month
provisional <- fread("Q:/Teams/D&PA/Data/births_and_deaths/births_by_month/births_Jul_19_June_20.csv",
                     header = TRUE) %>% data.frame()

#combined authorities
E06000052E06000053 <- filter(provisional, gss_code == "E06000052, E06000053")
E09000012E09000001 <- filter(provisional, gss_code == "E09000012, E09000001")

E52E63_2019 <- filter(ons_births, gss_code %in% c("E06000052", "E06000053"),
                      year == 2019, age == 0) %>%
  group_by(gss_code) %>% 
  summarise(combined_births = sum(births), .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(share = combined_births / sum(combined_births)) %>% 
  mutate(births = share * E06000052E06000053$births,
         year = 2020) %>% 
  select(gss_code, year, births)

E0912E0901_2019 <- filter(ons_births, gss_code %in% c("E09000012", "E09000001"),
                          year == 2019, age == 0) %>%
  group_by(gss_code) %>% 
  summarise(combined_births = sum(births), .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(share = combined_births / sum(combined_births)) %>% 
  mutate(births = share * E09000012E09000001$births,
         year = 2020) %>% 
  select(gss_code, year, births)

births_2020 <- provisional %>% 
  rbind(E52E63_2019, E0912E0901_2019) %>% 
  filter(gss_code %in% ons_births$gss_code) %>% 
  mutate(male = births * (105/205),
         female = births *(100/205)) %>% 
  select(-births) %>% 
  tidyr::pivot_longer(cols = c("male","female"), names_to = "sex", values_to = "births") %>% 
  mutate(age = 0) %>% 
  select(names(ons_births)) 

saveRDS(births_2020,  "input_data/fertility/provisional_births_2020_EW.rds")

rm(E52E63_2019, E0912E0901_2019, E06000052E06000053, E09000012E09000001, provisional)

EW_backseries <- births_2020 %>% 
  complete_fertility(popn, col_rate = "births") %>% 
  rbind(filter(ons_births, !gss_code %in% c("S92000003","N92000002"))) 

for(method in c("average", "trend")) {
  
  fert_rates <- scaled_fertility_curve(popn_mye_path = popn,
                                       births_mye_path = EW_backseries,
                                       target_curves_filepath = "input_data/fertility/ons_asfr_curves_2018_(2020_geog).rds",
                                       last_data_year = 2020,
                                       n_years_to_avg = 5,
                                       avg_or_trend = method,
                                       data_col = "births",
                                       output_col = "rate") %>%
    project_rates_npp(rate_col = "rate",
                      rate_trajectory_filepath = "input_data/fertility/npp_fertility_trend.rds",
                      first_proj_yr = 2021,
                      n_proj_yr = 30,
                      npp_var = "2018_principal")
  
  
  fertility_rates <- fert_rates %>% 
    complete_fertility(popn) %>% 
    rbind(scotland_n_ireland[[method]]) 
  
  filename_suffix <- ifelse(method == "average", "_5yr_avg", "_5yr_trend")
  saveRDS(fertility_rates, paste0("input_data/fertility/fertility_rates_provisional_2020",filename_suffix,".rds"))
}

rm(list=ls())