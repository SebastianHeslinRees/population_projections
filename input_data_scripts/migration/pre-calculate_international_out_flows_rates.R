library(dplyr)
library(popmodules)

output_loc <- "input_data/mye/"
int_out_mye_path <-  "input_data/mye/2019/int_out_ons.rds"
births_path <-  "input_data/mye/2019/births_ons.rds"
popn_path <-  "input_data/mye/2019/population_ons.rds"

#Average FLOWS

flow_10yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_out_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 10,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

flow_15yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_out_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 15,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

flow_5yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_out_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 5,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

#Average RATES

rate_10yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_path,
  births_mye_path = births_path,
  flow_or_rate = "rate",
  component_path = int_out_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 10,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

rate_15yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_path,
  births_mye_path = births_path,
  flow_or_rate = "rate",
  component_path = int_out_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 15,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

rate_5yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_path,
  births_mye_path = births_path,
  flow_or_rate = "rate",
  component_path = int_out_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 5,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

saveRDS(flow_5yr_avg_2019, paste0(output_loc,"2019/int_out_5yr_avg_flow_2019.rds"))
saveRDS(flow_10yr_avg_2019, paste0(output_loc,"2019/int_out_10yr_avg_flow_2019.rds"))
saveRDS(flow_15yr_avg_2019, paste0(output_loc,"2019/int_out_15yr_avg_flow_2019.rds"))

saveRDS(rate_5yr_avg_2019, paste0(output_loc,"2019/int_out_5yr_avg_rate_2019.rds"))
saveRDS(rate_10yr_avg_2019, paste0(output_loc,"2019/int_out_10yr_avg_rate_2019.rds"))
saveRDS(rate_15yr_avg_2019, paste0(output_loc,"2019/int_out_15yr_avg_rate_2019.rds"))

rm(list=ls())
