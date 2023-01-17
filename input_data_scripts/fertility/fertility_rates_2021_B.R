library(popmodules)
library(dplyr)

# GLA population denominators

message("fertility rates 2021-based model")

gla_popn_mye_path <- paste0("input_data/mye/2021/population_gla.rds")
births_mye_path <-  paste0("input_data/mye/2021/births_gla.rds")
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"
births_2022_path <- "input_data/scenario_data/births_mid_22.rds"

popn <- readRDS(gla_popn_mye_path) %>% filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))

births_eng <- readRDS(births_mye_path) %>%
  bind_rows(readRDS(births_2022_path)) %>% 
  filter(!substr(gss_code,1,3) %in% c("E12","E92")) %>% 
  filter(substr(gss_code,1,1) %in% c("E"))

births_all <-  readRDS(births_mye_path) %>%
  filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))
  
  
#------------------------------------------------------------------------------- 

# 5 year trend rates

trend_5yrs_eng <- scaled_fertility_curve(popn = popn,
                                         births = births_eng,
                                         target_curves = fertility_curve_filepath,
                                         last_data_year = 2022,
                                         n_years_to_avg = 5,
                                         avg_or_trend = "trend",
                                         data_col = "births",
                                         output_col = "rate") %>%
  
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2023,
                    n_proj_yr = 28,
                    npp_var = "2018_principal") %>% 
  filter(year >= 2023)

#------------------------------------------------------------------------------- 

trend_5yrs_other <- scaled_fertility_curve(popn = popn,
                                           births = births_all,
                                           target_curves = fertility_curve_filepath,
                                           last_data_year = 2021,
                                           n_years_to_avg = 5,
                                           avg_or_trend = "trend",
                                           data_col = "births",
                                           output_col = "rate") %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2018_principal")

trend_5yrs_eng_2022 <- filter(trend_5yrs_other, substr(gss_code,1,1)=="E", year <= 2022)
trend_5yrs_other <- filter(trend_5yrs_other, substr(gss_code,1,3) %in% c("N92","S92","W06"))

#-------------------------------------------------------------------------------

trend_5yrs <- bind_rows(trend_5yrs_eng, trend_5yrs_eng_2022, trend_5yrs_other)
validate_population(trend_5yrs)
validate_complete(trend_5yrs, test_cols = c("year","gss_code","sex","age"), nested_cols = "gss_code")

saveRDS(trend_5yrs, "input_data/fertility/fert_rates_5yr_trend_2021.rds")

rm(list=ls())
