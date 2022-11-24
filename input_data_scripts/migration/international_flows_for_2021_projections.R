#International

library(dplyr)
library(popmodules)

message("scenario international migration flows (2021 projections)")

popn_mye_path <- "input_data/mye/2021/population_gla.rds"
births_mye_path <- "input_data/mye/2021/births_gla.rds"
int_in_mye_path <-  "input_data/mye/2021/int_in_gla.rds"
int_out_mye_path <-  "input_data/mye/2021/int_out_gla.rds"
dir.create("input_data/scenario_data", showWarnings = FALSE)


int_in_avg_2012_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 5,
  data_col = "int_in",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = NULL) %>%
  select(-year)

int_out_avg_2012_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "rate",
  component_path = int_out_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 10,
  data_col = "int_out",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

#-------------------------------------------------------------------------------

saveRDS(int_in_avg_2012_2021, "input_data/scenario_data/2021_int_in_10yr_avg.rds")
saveRDS(int_out_avg_2012_2021, "input_data/scenario_data/2021_int_out_10yr_avg.rds")

#-------------------------------------------------------------------------------

rm(list=ls())
