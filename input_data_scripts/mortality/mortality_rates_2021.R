library(popmodules)
library(dplyr)


# GLA population denominators

message("mortality rates 2021-based model")


gla_popn_mye_path <- "input_data/mye/2021/population_gla.rds"
births_mye_path <-  "input_data/mye/2021/births_gla.rds"
deaths_mye_path <-  "input_data/mye/2021/deaths_gla.rds"
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"

popn <- readRDS(gla_popn_mye_path) %>% filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))
births <- readRDS(births_mye_path) %>% filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))
deaths <- readRDS(deaths_mye_path) %>% filter(!substr(gss_code,1,3) %in% c("E12","E92","W92"))

#-------------------------------------------------------------------------------

# 5 year average rates

average_5yrs <- scaled_mortality_curve(popn = popn,
                                       births = births,
                                       deaths = deaths,
                                       target_curves = mortality_curve_filepath,
                                       last_data_year = 2019,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "average",
                                       data_col = "deaths",
                                       output_col = "rate",) %>%
  
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = mortality_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2020_principal")

#-------------------------------------------------------------------------------

# 5 year trended rates

trend_5yrs <- scaled_mortality_curve(popn = popn,
                                     births = births,
                                     deaths = deaths,
                                     target_curves = mortality_curve_filepath,
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "deaths",
                                     output_col = "rate") %>%
  
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = mortality_trajectory_filepath,
                    first_proj_yr = 2022,
                    n_proj_yr = 29,
                    npp_var = "2020_principal")

#-------------------------------------------------------------------------------

saveRDS(average_5yrs, "input_data/mortality/mort_rates_5yr_avg_2021.rds")
saveRDS(trend_5yrs, "input_data/mortality/mort_rates_5yr_trend_2021.rds")


rm(list = ls())
