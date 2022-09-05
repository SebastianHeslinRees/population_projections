library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("Ward 2022 fertility & mortality")

input_data_dir <- "input_data/flexible_area_model/"

ward_pop <- paste0(input_data_dir, "backseries/ward_population_WD22CD.rds") %>% readRDS()
ward_births <- paste0(input_data_dir, "backseries/ward_births_WD22CD.rds") %>% readRDS()
ward_deaths <- paste0(input_data_dir, "backseries/ward_deaths_WD22CD.rds") %>% readRDS()

#-------------------------------------------------------------------------------

denominator_popn <- ward_pop %>%
  popn_age_on(births = ward_births,
              col_aggregation=c("gss_code_ward", "year", "sex", "age"),
              col_geog = "gss_code_ward")

#-------------------------------------------------------------------------------

# Mortality Rates
# Uses ASMR curves and NPP trend

mort_rates <- scaled_mortality_curve(popn = ward_pop,
                                     births = ward_births,
                                     deaths = ward_deaths,
                                     target_curves = "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019, #don't use 2020 because of covid
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

fert_rates <- scaled_fertility_curve(popn = ward_pop,
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

#-------------------------------------------------------------------------------

saveRDS(mort_rates, paste0(input_data_dir, "processed/mortality_rates_WD22CD.rds"))
saveRDS(fert_rates,  paste0(input_data_dir, "processed/fertility_rates_WD22CD.rds"))

rm(list=ls())
