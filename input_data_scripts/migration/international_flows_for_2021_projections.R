#International

library(dplyr)
library(popmodules)

message("scenario international migration flows (2021 projections)")

popn_mye_path <- "input_data/mye/2021/population_gla.rds"
births_mye_path <- "input_data/mye/2021/births_gla.rds"
int_in_mye_path <-  "input_data/mye/2021/int_in_gla.rds"
int_out_mye_path <-  "input_data/mye/2021/int_out_gla.rds"
dir.create("input_data/scenario_data", showWarnings = FALSE)

#---

# 10 year


int_in_avg_2012_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 10,
  data_col = "int_in",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = NULL) %>%
  select(-year)

int_out_rate_2012_2021 <- calculate_mean_international_rates_or_flows(
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

int_out_flow_2012_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_out_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 10,
  data_col = "int_out",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = NULL) %>%
  select(-year)

#---

# 5 year

int_in_avg_2017_2021 <- calculate_mean_international_rates_or_flows(
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

int_out_rate_2017_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "rate",
  component_path = int_out_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 5,
  data_col = "int_out",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_out_flow_2017_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_out_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 5,
  data_col = "int_out",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = NULL) %>%
  select(-year)

#---

# 15 year

int_in_avg_2007_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 15,
  data_col = "int_in",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = NULL) %>%
  select(-year)

int_out_rate_2007_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "rate",
  component_path = int_out_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 15,
  data_col = "int_out",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

int_out_flow_2007_2021 <- calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_out_mye_path,
  last_data_year = 2021,
  n_years_to_avg = 15,
  data_col = "int_out",
  first_proj_yr = 2022,
  n_proj_yr = 1,
  rate_cap = NULL) %>%
  select(-year)

#-------------------------------------------------------------------------------

saveRDS(int_in_avg_2012_2021, "input_data/scenario_data/2021_int_in_10yr_avg.rds")
saveRDS(int_out_rate_2012_2021, "input_data/scenario_data/2021_int_out_10yr_avg_rate.rds")
saveRDS(int_out_flow_2012_2021, "input_data/scenario_data/2021_int_out_10yr_avg_flow.rds")

saveRDS(int_in_avg_2017_2021, "input_data/scenario_data/2021_int_in_5yr_avg.rds")
saveRDS(int_out_rate_2017_2021, "input_data/scenario_data/2021_int_out_5yr_avg_rate.rds")
saveRDS(int_out_flow_2017_2021, "input_data/scenario_data/2021_int_out_5yr_avg_flow.rds")

saveRDS(int_in_avg_2007_2021, "input_data/scenario_data/2021_int_in_15yr_avg.rds")
saveRDS(int_out_rate_2007_2021, "input_data/scenario_data/2021_int_out_15yr_avg_rate.rds")
saveRDS(int_out_flow_2007_2021, "input_data/scenario_data/2021_int_out_15yr_avg_flow.rds")

#-------------------------------------------------------------------------------

# Scale 2022 flows to match UK LTIM totals
LTIM_in <- 1064000
LTIM_out <- 560000

scaled_2022_in <- int_in_avg_2012_2021 %>% 
  mutate(int_in = int_in*(LTIM_in/sum(int_in)))

scaled_2022_out <- int_out_flow_2012_2021 %>% 
  mutate(int_out = int_out*(LTIM_out/sum(int_out)))

saveRDS(scaled_2022_in, "input_data/scenario_data/int_in_2022.rds")
saveRDS(scaled_2022_out, "input_data/scenario_data/int_out_2022.rds")

rm(list=ls())

