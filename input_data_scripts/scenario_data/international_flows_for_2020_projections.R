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

#-------------------------------------------------------------------------------

saveRDS(int_in_avg_2010_2019, "input_data/scenario_data/2019_int_in_10yr_avg.rds")
saveRDS(int_out_avg_2010_2019, "input_data/scenario_data/2019_int_out_10yr_avg.rds")

#-------------------------------------------------------------------------------

#Calculate gross flows for net scenarios

ldn_10_year_in <- sum(filter(int_in_avg_2010_2019, substr(gss_code,1,3)=="E09")$int_in)
ldn_10_year_out <- sum(filter(int_out_avg_2010_2019, substr(gss_code,1,3)=="E09")$int_out)

all_in <- sum(int_in_avg_2010_2019$int_in)
all_out <- sum(int_out_avg_2010_2019$int_out)

in_to_out_ratio <- ldn_10_year_out/ldn_10_year_in

#-------------------------------------------------------------------------------

#-9k net, 64k in, 73k out
#The net outflow is a level agreed with the Panel
#The 64k in is calculated from visa applications:
#  537,549 visas for the UK in the year to June 2021
#  This translates to around 194k UK migration (by applying the average past relationship of 36%)
#  This UK 194k translates to 64k in London by applying the past relationship of 33%

in_10 = 64000 
out_10 = in_10 + 9000

in_scaling_10 <- in_10/ldn_10_year_in
out_scaling_10 <- out_10/ldn_10_year_out

scaled_in_10 <- mutate(int_in_avg_2010_2019, int_in = int_in * in_scaling_10)
scaled_out_10 <- mutate(int_out_avg_2010_2019, int_out = int_out * out_scaling_10)

saveRDS(scaled_in_10, "input_data/scenario_data/2020_int_in_scenario_1_yr_2021.rds")
saveRDS(scaled_out_10, "input_data/scenario_data/2020_int_out_scenario_1_yr_2021.rds")

#-------------------------------------------------------------------------------

#70k net
#The net outflow is a level agreed with the Panel
#The gross flows are calculated by applying the average ratio in-to-out to the net
in_70 = 70000 / (1-in_to_out_ratio)
out_70 = in_70 - 70000

in_scaling_70 <- in_70/ldn_10_year_in
out_scaling_70 <- out_70/ldn_10_year_out

scaled_in_70 <- mutate(int_in_avg_2010_2019, int_in = int_in * in_scaling_10)
scaled_out_70 <- mutate(int_out_avg_2010_2019, int_out = int_out * out_scaling_10)

saveRDS(scaled_in_70, "input_data/scenario_data/2020_int_in_scenario_1_yr_2022.rds")
saveRDS(scaled_out_70, "input_data/scenario_data/2020_int_out_scenario_1_yr_2022.rds")

#-------------------------------------------------------------------------------

#125k net
#The net outflow is a level agreed with the Panel
#The gross flows are calculated by applying the average ratio in-to-out to the net
in_125 = 125000 / (1-in_to_out_ratio)
out_125 = in_125 - 125000

in_scaling_125 <- in_125/ldn_10_year_in
out_scaling_125 <- out_125/ldn_10_year_out

scaled_in_125 <- mutate(int_in_avg_2010_2019, int_in = int_in * in_scaling_125)
scaled_out_125 <- mutate(int_out_avg_2010_2019, int_out = int_out * out_scaling_125)

saveRDS(scaled_in_125, "input_data/scenario_data/2020_int_in_125k.rds")
saveRDS(scaled_out_125, "input_data/scenario_data/2020_int_out_125k.rds")

#-------------------------------------------------------------------------------

#50k net
#The net outflow is a level agreed with the Panel
#The gross flows are calculated by applying the average ratio in-to-out to the net
in_50 = 50000 / (1-in_to_out_ratio)
out_50 = in_50 - 50000

in_scaling_50 <- in_50/ldn_10_year_in
out_scaling_50 <- out_50/ldn_10_year_out

scaled_in_50 <- mutate(int_in_avg_2010_2019, int_in = int_in * in_scaling_50)
scaled_out_50 <- mutate(int_out_avg_2010_2019, int_out = int_out * out_scaling_50)

saveRDS(scaled_in_50, "input_data/scenario_data/2020_int_in_50k.rds")
saveRDS(scaled_out_50, "input_data/scenario_data/2020_int_out_50k.rds")

#-------------------------------------------------------------------------------

rm(list=ls())
