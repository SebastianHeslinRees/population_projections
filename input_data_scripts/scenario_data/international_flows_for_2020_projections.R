#International

library(dplyr)
library(popmodules)

message("scenario international migration flows (2020 projections)")

popn_mye_path <- "input_data/mye/2020/population_ons.rds" #TODO Change to GLA est
int_in_mye_path <-  "input_data/mye/2020/int_in_ons.rds" #TODO Change to GLA est
int_out_mye_path <-  "input_data/mye/2020/int_out_ons.rds" #TODO Change to GLA est
dir.create("input_data/scenario_data", showWarnings = FALSE)

#Don't use 2020 international data
int_in_avg_2015_2019 <- calculate_mean_international_rates_or_flows(
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

int_out_avg_2015_2019 <- calculate_mean_international_rates_or_flows(
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

int_in_avg_2010_2019 <- calculate_mean_international_rates_or_flows(
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

int_out_avg_2010_2019 <- calculate_mean_international_rates_or_flows(
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

#---------------------------------

int_in_scenario_1_yr_2021 <- int_in_avg_2015_2019 %>% mutate(int_in = int_in * 0.2)
int_in_scenario_1_yr_2022 <- int_in_avg_2015_2019 %>% mutate(int_in = int_in * 0.5)

int_out_scenario_1_yr_2021 <- int_out_avg_2015_2019 %>% mutate(int_out = int_out * 0.5)
int_out_scenario_1_yr_2022 <- int_out_avg_2015_2019 %>% mutate(int_out = int_out * 0.3)

#---------------------------------

saveRDS(int_in_scenario_1_yr_2021, "input_data/scenario_data/2020_int_in_scenario_1_yr_2021.rds")
saveRDS(int_in_scenario_1_yr_2022, "input_data/scenario_data/2020_int_in_scenario_1_yr_2022.rds")

saveRDS(int_out_scenario_1_yr_2021, "input_data/scenario_data/2020_int_out_scenario_1_yr_2021.rds")
saveRDS(int_out_scenario_1_yr_2022, "input_data/scenario_data/2020_int_out_scenario_1_yr_2022.rds")

#---------------------------------

saveRDS(int_in_avg_2010_2019, "input_data/scenario_data/2019_int_in_10yr_avg.rds")
saveRDS(int_out_avg_2010_2019, "input_data/scenario_data/2019_int_out_10yr_avg.rds")


#-------------------------------------------------------------------------------

#Calculate gross flows for net scenarios

#long term int
ldn_10_year_in <- sum(filter(int_in_avg_2010_2019, substr(gss_code,1,3)=="E09")$int_in)
ldn_10_year_out <- sum(filter(int_out_avg_2010_2019, substr(gss_code,1,3)=="E09")$int_out)

all_in <- sum(int_in_avg_2010_2019$int_in)
all_out <- sum(int_out_avg_2010_2019$int_out)

in_to_out_ratio <- ldn_10_year_out/ldn_10_year_in

#-------------------------------------------------------------------------------

#125k net
in_125 = 125000 / (1-in_to_out_ratio)
out_125 = in_125 - 125000

in_scaling_125 <- in_125/ldn_10_year_in
out_scaling_125 <- out_125/ldn_10_year_out

scaled_in_125 <- mutate(int_in_avg_2010_2019, int_in = int_in * in_scaling_125)
scaled_out_125 <- mutate(int_out_avg_2010_2019, int_out = int_out * out_scaling_125)

#-------------------------------------------------------------------------------

#50k net
in_50 = 50000 / (1-in_to_out_ratio)
out_50 = in_50 - 50000

in_scaling_50 <- in_50/ldn_10_year_in
out_scaling_50 <- out_50/ldn_10_year_out

scaled_in_50 <- mutate(int_in_avg_2010_2019, int_in = int_in * in_scaling_50)
scaled_out_50 <- mutate(int_out_avg_2010_2019, int_out = int_out * out_scaling_50)

#-------------------------------------------------------------------------------

saveRDS(scaled_in_125, "input_data/scenario_data/2020_int_in_125k.rds")
saveRDS(scaled_out_125, "input_data/scenario_data/2020_int_out_125k.rds")
saveRDS(scaled_in_50, "input_data/scenario_data/2020_int_in_50k.rds")
saveRDS(scaled_out_50, "input_data/scenario_data/2020_int_out_50k.rds")

#-------------------------------------------------------------------------------

rm(list=ls())
