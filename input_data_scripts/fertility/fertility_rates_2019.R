library(popmodules)
library(dplyr)

popn_mye_path <- paste0("input_data/mye/2019/population_ons.rds")
births_mye_path <-  paste0("input_data/mye/2019/births_ons.rds")

fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves_2018_(2020_geog).rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"

#---- 5 year average rates

average_5yrs <- scaled_fertility_curve(popn_mye_path = popn_mye_path,
                                       births_mye_path = births_mye_path,
                                       target_curves_filepath = fertility_curve_filepath,
                                       last_data_year = 2019,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "average",
                                       data_col = "births",
                                       output_col = "rate") %>% 
  
  project_rates_npp(rate_col = "rate",
                    rate_trajectory_filepath = fertility_trajectory_filepath,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")


#--- 5 year trended rates

trend_5yrs <- scaled_fertility_curve(popn_mye_path = popn_mye_path,
                                       births_mye_path = births_mye_path,
                                       target_curves_filepath = fertility_curve_filepath,
                                       last_data_year = 2019,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "trend",
                                       data_col = "births",
                                       output_col = "rate") %>% 
  
  project_rates_npp(rate_col = "rate",
                    rate_trajectory_filepath = fertility_trajectory_filepath,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#--- Save

saveRDS(average_5yrs, "input_data/fertility/fert_rates_5yr_avg_2019.rds")
saveRDS(trend_5yrs, "input_data/fertility/fert_rates_5yr_trend_2019.rds")

