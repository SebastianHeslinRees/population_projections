library(popmodules)
library(dplyr)

# GLA population denominators

message("fertility rates 2021-based model")

gla_popn_mye_path <- paste0("input_data/mye/2021/population_gla.rds")
births_mye_path <-  paste0("input_data/mye/2021/births_gla.rds")
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"

popn <- readRDS(gla_popn_mye_path) %>% filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))
births <- readRDS(births_mye_path) %>% filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))

#------------------------------------------------------------------------------- 

# 5 year average rates

average_5yrs <- scaled_fertility_curve(popn = popn,
                                       births = births,
                                       target_curves = fertility_curve_filepath,
                                       last_data_year = 2021,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "average",
                                       data_col = "births",
                                       output_col = "rate") %>%

  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2018_principal")

#------------------------------------------------------------------------------- 

# 5 year trended rates

trend_5yrs <- scaled_fertility_curve(popn = popn,
                                     births = births,
                                     target_curves = fertility_curve_filepath,
                                     last_data_year = 2021,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate")

trend_5yrs_principal <- trend_5yrs %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2018_principal")

trend_5yrs_low  <- trend_5yrs %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2018_low")


trend_5yrs_high <- trend_5yrs %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2018_high")

#------------------------------------------------------------------------------- 

# Trend 2 years (2018 & 2019)

trend_2yrs <- scaled_fertility_curve(popn = popn,
                                     births = births,
                                     target_curves = fertility_curve_filepath,
                                     last_data_year = 2019,
                                     n_years_to_avg = 2,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate") %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = fertility_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2018_principal")

#-------------------------------------------------------------------------------

saveRDS(average_5yrs, "input_data/fertility/fert_rates_5yr_avg_2021.rds")
saveRDS(trend_5yrs_principal, "input_data/fertility/fert_rates_5yr_trend_2021.rds")

saveRDS(trend_5yrs_low, "input_data/fertility/fert_rates_5yr_trend_2021_LOW.rds")
saveRDS(trend_5yrs_high, "input_data/fertility/fert_rates_5yr_trend_2021_HIGH.rds")

saveRDS(trend_2yrs, "input_data/fertility/fert_rates_2yr_trend_2019.rds")


rm(list=ls())
