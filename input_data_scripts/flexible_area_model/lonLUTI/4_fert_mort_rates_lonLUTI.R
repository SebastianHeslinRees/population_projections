library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("lonLUTI fertility & mortality")

input_data_dir <- "input_data/flexible_area_model/"

lookup <- paste0(input_data_dir, "lookups/lonLUTI_name_lookup.rds") %>%
  readRDS() %>%
  select(LonLUTI3, gss_code)

lonLUTI_pop <- paste0(input_data_dir, "backseries/lonLUTI_population.rds") %>%
  readRDS()%>%
  left_join(lookup, by = "LonLUTI3")

lonLUTI_births <- paste0(input_data_dir, "backseries/lonLUTI_births.rds") %>%
  readRDS() %>%
  left_join(lookup, by = "LonLUTI3")

lonLUTI_deaths <- paste0(input_data_dir, "backseries/lonLUTI_deaths.rds") %>%
  readRDS() %>%
  left_join(lookup, by = "LonLUTI3")


#-------------------------------------------------------------------------------

# Mortality Rates
# Uses ASMR curves and NPP trend

mort_rates_5 <- scaled_mortality_curve(popn = lonLUTI_pop,
                                     births = lonLUTI_births,
                                     deaths = lonLUTI_deaths,
                                     target_curves = "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "deaths",
                                     output_col = "rate",
                                     col_aggregation=c("gss_code", "LonLUTI3", "year", "sex"),
                                     col_geog = c("gss_code","LonLUTI3")) %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/mortality/npp_mortality_trend.rds",
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#-------------------------------------------------------------------------------

# Fertility rates
# Uses ASFR rates and NPP trend

fert_rates_5 <- scaled_fertility_curve(popn = lonLUTI_pop,
                                     births = lonLUTI_births,
                                     target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2020,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate",
                                     col_geog = c("gss_code","LonLUTI3"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2021,
                    n_proj_yr = 30,
                    npp_var = "2018_principal") %>% 
complete_popn_dataframe()

#-------------------------------------------------------------------------------

saveRDS(mort_rates_5, paste0(input_data_dir, "processed/mortality_rates_lonLUTI.rds"))
saveRDS(fert_rates_5,  paste0(input_data_dir, "processed/fertility_rates_lonLUTI.rds"))

rm(list=ls())
