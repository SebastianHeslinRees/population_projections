library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("Ward 2013 fertility & mortality")

input_data_dir <- "input_data/flexible_area_model/"

ward_pop <- paste0(input_data_dir, "backseries/ward_population_WD13CD.rds") %>% readRDS()
ward_births <- paste0(input_data_dir, "backseries/ward_births_WD13CD.rds") %>% readRDS()
ward_deaths <- paste0(input_data_dir, "backseries/ward_deaths_WD13CD.rds") %>% readRDS()

#-------------------------------------------------------------------------------

# Mortality Rates
# Uses ASMR curves and NPP trend

mort_rates_5 <- scaled_mortality_curve(popn = ward_pop,
                                     births = ward_births,
                                     deaths = ward_deaths,
                                     target_curves = "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "deaths",
                                     output_col = "rate",
                                     col_aggregation=c("gss_code", "gss_code_ward", "year", "sex"),
                                     col_geog = c("gss_code","gss_code_ward")) %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/mortality/npp_mortality_trend.rds",
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#-------------------------------------------------------------------------------

# Fertility rates
# Uses ASFR rates and NPP trend

fert_rates_5 <- scaled_fertility_curve(popn = ward_pop,
                                     births = ward_births,
                                     target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2020,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate",
                                     col_geog = c("gss_code","gss_code_ward"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2021,
                    n_proj_yr = 30,
                    npp_var = "2018_principal") %>% 
complete_popn_dataframe()

fert_rates_5_high <- scaled_fertility_curve(popn = ward_pop,
                                       births = ward_births,
                                       target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                       last_data_year = 2020,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "trend",
                                       data_col = "births",
                                       output_col = "rate",
                                       col_geog = c("gss_code","gss_code_ward"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2021,
                    n_proj_yr = 30,
                    npp_var = "2018_high") %>% 
  complete_popn_dataframe()

fert_rates_5_low <- scaled_fertility_curve(popn = ward_pop,
                                       births = ward_births,
                                       target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                       last_data_year = 2020,
                                       n_years_to_avg = 5,
                                       avg_or_trend = "trend",
                                       data_col = "births",
                                       output_col = "rate",
                                       col_geog = c("gss_code","gss_code_ward"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2021,
                    n_proj_yr = 30,
                    npp_var = "2018_low") %>% 
  complete_popn_dataframe()


#-------------------------------------------------------------------------------

saveRDS(mort_rates_5, paste0(input_data_dir, "processed/mortality_rates_WD13CD.rds"))
saveRDS(fert_rates_5,  paste0(input_data_dir, "processed/fertility_rates_WD13CD.rds"))

saveRDS(fert_rates_5_high,  paste0(input_data_dir, "processed/fertility_rates_WD13CD_HIGH.rds"))
saveRDS(fert_rates_5_low,  paste0(input_data_dir, "processed/fertility_rates_WD13CD_LOW.rds"))

rm(list=ls())
