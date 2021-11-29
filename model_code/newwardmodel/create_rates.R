library(dplyr)
library(data.table)
library(assertthat)
devtools::load_all('model_code/popmodules')

data_dir <- "input_data/new_ward_model/"

ward_pop <- paste0(data_dir, "ward_population_WD20CD.rds") %>% readRDS()
ward_births <- paste0(data_dir, "ward_births_WD20CD.rds") %>% readRDS()
ward_deaths <- paste0(data_dir, "ward_deaths_WD20CD.rds") %>% readRDS()
ward_in_mig <- paste0(data_dir, "ward_inflow_WD20CD.rds") %>% readRDS() %>% data.frame()
ward_out_mig <- paste0(data_dir, "ward_outflow_WD20CD.rds") %>% readRDS() %>% data.frame()

#-------------------------------------------------------------------------------
denominator_popn <- ward_pop %>%
  popn_age_on(births = ward_births,
              col_aggregation=c("gss_code_ward", "year", "sex", "age"),
              col_geog = "gss_code_ward")

#-------------------------------------------------------------------------------
ward_pop_x <- filter(ward_pop, gss_code_ward != "E05000026")
mort_rates <- scaled_mortality_curve(popn = ward_pop,
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

fert_rates <- scaled_fertility_curve(popn = ward_pop,
                                     births = ward_births,
                                     target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate",
                                     col_geog = c("gss_code","gss_code_ward"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal") %>% 
complete_popn_dataframe()

#-------------------------------------------------------------------------------

out_mig_rates_5 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5,
                                 last_data_year = 2019,
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  project_forward_flat(2050)

out_mig_rates_10 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10,
                                 last_data_year = 2019,
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

in_mig_flows_5 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5,
                                 last_data_year = 2019,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

in_mig_flows_10 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10,
                                 last_data_year = 2019,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

saveRDS(mort_rates, paste0(data_dir, "mortality_rates_WD20CD.rds"))
saveRDS(fert_rates,  paste0(data_dir, "fertility_rates_WD20CD.rds"))
saveRDS(in_mig_flows_5,  paste0(data_dir, "in_migration_flows_WD20CD_5yr_avg.rds"))
saveRDS(out_mig_rates_5,  paste0(data_dir, "out_migration_rates_WD20CD_5yr_avg.rds"))
saveRDS(in_mig_flows_10,  paste0(data_dir, "in_migration_flows_WD20CD_10yr_avg.rds"))
saveRDS(out_mig_rates_10,  paste0(data_dir, "out_migration_rates_WD20CD_10yr_avg.rds"))
rm(list=ls())
