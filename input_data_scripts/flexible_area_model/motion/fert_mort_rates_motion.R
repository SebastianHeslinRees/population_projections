library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("motion_zone fertility & mortality")

input_data_dir <- "input_data/flexible_area_model/"

lookup <- paste0(input_data_dir, "lookups/motion_zone_name_lookup.rds") %>%
  readRDS() %>%
  select(motion_zone, gss_code)

motion_zone_pop <- paste0(input_data_dir, "backseries/motion_zone_population.rds") %>%
  readRDS()%>%
  left_join(lookup, by = "motion_zone")

motion_zone_births <- paste0(input_data_dir, "backseries/motion_zone_births.rds") %>%
  readRDS() %>%
  left_join(lookup, by = "motion_zone")

motion_zone_deaths <- paste0(input_data_dir, "backseries/motion_zone_deaths.rds") %>%
  readRDS() %>%
  left_join(lookup, by = "motion_zone")

#-------------------------------------------------------------------------------

denominator_popn <- motion_zone_pop  %>% 
  popn_age_on(births = motion_zone_births,
              col_aggregation=c("gss_code","motion_zone", "year", "sex", "age"),
              col_geog = c("gss_code", "motion_zone"))

#-------------------------------------------------------------------------------

# Mortality Rates
# Uses ASMR curves and NPP trend

mort_rates_5 <- scaled_mortality_curve(popn = motion_zone_pop,
                                     births = motion_zone_births,
                                     deaths = motion_zone_deaths,
                                     target_curves = "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "deaths",
                                     output_col = "rate",
                                     col_aggregation=c("gss_code", "motion_zone", "year", "sex"),
                                     col_geog = c("gss_code","motion_zone")) %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/mortality/npp_mortality_trend.rds",
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#-------------------------------------------------------------------------------

# Fertility rates
# Uses ASFR rates and NPP trend

fert_rates_5 <- scaled_fertility_curve(popn = motion_zone_pop,
                                     births = motion_zone_births,
                                     target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2020,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate",
                                     col_geog = c("gss_code","motion_zone"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2021,
                    n_proj_yr = 30,
                    npp_var = "2018_principal") %>% 
complete_popn_dataframe()

#-------------------------------------------------------------------------------

saveRDS(mort_rates_5, paste0(input_data_dir, "processed/mortality_rates_motion_zone.rds"))
saveRDS(fert_rates_5,  paste0(input_data_dir, "processed/fertility_rates_motion_zone.rds"))

rm(list=ls())
