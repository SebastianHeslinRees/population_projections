library(popmodules)
library(dplyr)

message("mortality rates 2019-based model")

popn_mye_path <- paste0("input_data/mye/2019/population_ons.rds")
gla_popn_mye_path <- paste0("input_data/mye/2019/population_gla.rds")

births_mye_path <-  paste0("input_data/mye/2019/births_ons.rds")
deaths_mye_path <-  paste0("input_data/mye/2019/deaths_ons.rds")

mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves_2018_(2020_geog).rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"

#---- 5 year average rates

average_5yrs <- scaled_mortality_curve(popn_mye_path = popn_mye_path,
                                       births_mye_path = births_mye_path,
                                       deaths_mye_path = deaths_mye_path,
                                       target_curves_filepath = mortality_curve_filepath,
                                       last_data_year = 2019,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "average",
                                       data_col = "deaths",
                                       output_col = "rate") %>% 
  
  project_rates_npp(rate_col = "rate",
                    rate_trajectory_filepath = mortality_trajectory_filepath,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")


#--- 5 year trended rates

trend_5yrs <- scaled_mortality_curve(popn_mye_path = popn_mye_path,
                                       births_mye_path = births_mye_path,
                                       deaths_mye_path = deaths_mye_path,
                                       target_curves_filepath = mortality_curve_filepath,
                                       last_data_year = 2019,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "trend",
                                       data_col = "deaths",
                                       output_col = "rate") %>% 
  
  project_rates_npp(rate_col = "rate",
                    rate_trajectory_filepath = mortality_trajectory_filepath,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#--- Save

saveRDS(average_5yrs, "input_data/mortality/mort_rates_5yr_avg_2019.rds")
saveRDS(trend_5yrs, "input_data/mortality/mort_rates_5yr_trend_2019.rds")
rm(average_5yrs, trend_5yrs)

#---

# GLA population denominators

#---- 5 year average rates

average_5yrs <- scaled_mortality_curve(popn_mye_path = gla_popn_mye_path,
                                       births_mye_path = births_mye_path,
                                       deaths_mye_path = deaths_mye_path,
                                       target_curves_filepath = mortality_curve_filepath,
                                       last_data_year = 2019,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "average",
                                       data_col = "deaths",
                                       output_col = "rate") %>% 
  
  project_rates_npp(rate_col = "rate",
                    rate_trajectory_filepath = mortality_trajectory_filepath,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")


#--- 5 year trended rates

trend_5yrs <- scaled_mortality_curve(popn_mye_path = gla_popn_mye_path,
                                     births_mye_path = births_mye_path,
                                     deaths_mye_path = deaths_mye_path,
                                     target_curves_filepath = mortality_curve_filepath,
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "deaths",
                                     output_col = "rate") %>% 
  
  project_rates_npp(rate_col = "rate",
                    rate_trajectory_filepath = mortality_trajectory_filepath,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#--- Save

saveRDS(average_5yrs, "input_data/mortality/mort_rates_5yr_avg_2019_gla_mye.rds")
saveRDS(trend_5yrs, "input_data/mortality/mort_rates_5yr_trend_2019_gla_mye.rds")

rm(list = ls())