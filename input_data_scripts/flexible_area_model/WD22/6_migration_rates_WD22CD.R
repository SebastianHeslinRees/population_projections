library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("Ward 2022 migration rates")

input_data_dir <- "input_data/flexible_area_model/"

ward_pop <- paste0(input_data_dir, "backseries/ward_population_WD22CD.rds") %>% readRDS()
ward_births <- paste0(input_data_dir, "backseries/ward_births_WD22CD.rds") %>% readRDS()
ward_in_mig <- paste0(input_data_dir, "backseries/ward_inflow_WD22CD.rds") %>% readRDS() %>% data.frame()
ward_out_mig <- paste0(input_data_dir, "backseries/ward_outflow_WD22CD.rds") %>% readRDS() %>% data.frame()

#-------------------------------------------------------------------------------

denominator_popn <- ward_pop %>%
  popn_age_on(births = ward_births,
              col_aggregation=c("gss_code", "gss_code_ward", "year", "sex", "age"),
              col_geog = c("gss_code", "gss_code_ward")) %>% 
  left_join(ward_in_mig, by = c("gss_code", "gss_code_ward", "year", "sex", "age")) %>% 
  mutate(denom_pop = popn + inflow)

#-------------------------------------------------------------------------------
# Averaged migration flows and rates

out_mig_rates_5 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code", "gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(denom_pop==0, 0, outflow/denom_pop)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5, 
                                 last_data_year = 2021, 
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2022) %>% 
  mutate(out_rate = ifelse(out_rate > 0.8, 0.8, out_rate)) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

in_mig_flows_5 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5, 
                                 last_data_year = 2021,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2022) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

# Averaged migration flows and rates - 10 YEAR

out_mig_rates_10 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code", "gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(denom_pop==0, 0, outflow/denom_pop)) %>%
  calculate_mean_from_backseries(n_years_to_avg = 10, 
                                 last_data_year = 2021, 
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2022) %>% 
  mutate(out_rate = ifelse(out_rate > 0.8, 0.8, out_rate)) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

in_mig_flows_10 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10, 
                                 last_data_year = 2021,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2022) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

saveRDS(in_mig_flows_5,  paste0(input_data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"))
saveRDS(out_mig_rates_5,  paste0(input_data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"))

saveRDS(in_mig_flows_10,  paste0(input_data_dir, "processed/in_migration_flows_WD22CD_10yr_avg.rds"))
saveRDS(out_mig_rates_10,  paste0(input_data_dir, "processed/out_migration_rates_WD22CD_10yr_avg.rds"))

rm(list=ls())
