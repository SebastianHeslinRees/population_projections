library(dplyr)
library(popmodules)

output_loc <- "input_data/mye/"

#2016 Flows
int_in_mye_path <-  "input_data/mye/2016/international_in_gla.rds"

int_in_10yr_avg_2016 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2016,
  n_years_to_avg = 10,
  data_col = "int_in",
  first_proj_yr = 2017,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_5yr_avg_2016 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2016,
  n_years_to_avg = 5,
  data_col = "int_in",
  first_proj_yr = 2017,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

#2018 Flows
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla.rds")

int_in_10yr_avg_2018 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2018,
  n_years_to_avg = 10,
  data_col = "int_in",
  first_proj_yr = 2019,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_15yr_avg_2018 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2018,
  n_years_to_avg = 15,
  data_col = "int_in",
  first_proj_yr = 2019,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_5yr_avg_2018 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2018,
  n_years_to_avg = 5,
  data_col = "int_in",
  first_proj_yr = 2019,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

#-------------------------------

#2019 Flows
int_in_mye_path <-  "input_data/mye/2019/int_in_ons.rds"

#10 year average

int_in_10yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 10,
  data_col = "int_in",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_15yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 15,
  data_col = "int_in",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_5yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 5,
  data_col = "int_in",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_18yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 18,
  data_col = "int_in",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_in_2yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 2,
  data_col = "int_in",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

saveRDS(int_in_5yr_avg_2016, paste0(output_loc,"2016/int_in_5yr_avg_2016.rds"))
saveRDS(int_in_10yr_avg_2016, paste0(output_loc,"2016/int_in_10yr_avg_2016.rds"))

saveRDS(int_in_5yr_avg_2018, paste0(output_loc,"2018/int_in_5yr_avg_2018.rds"))
saveRDS(int_in_10yr_avg_2018, paste0(output_loc,"2018/int_in_10yr_avg_2018.rds"))
saveRDS(int_in_15yr_avg_2018, paste0(output_loc,"2018/int_in_15yr_avg_2018.rds"))

saveRDS(int_in_5yr_avg_2019, paste0(output_loc,"2019/int_in_5yr_avg_2019.rds"))
saveRDS(int_in_10yr_avg_2019, paste0(output_loc,"2019/int_in_10yr_avg_2019.rds"))
saveRDS(int_in_15yr_avg_2019, paste0(output_loc,"2019/int_in_15yr_avg_2019.rds"))
saveRDS(int_in_18yr_avg_2019, paste0(output_loc,"2019/int_in_18yr_avg_2019.rds"))
saveRDS(int_in_2yr_avg_2019, paste0(output_loc,"2019/int_in_2yr_avg_2019.rds"))

rm(list=ls())
