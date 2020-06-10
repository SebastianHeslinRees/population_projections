library(dplyr)
devtools::load_all('model_code/popmodules')

output_loc <- "input_data/mye/"

#2016 Flows
popn_mye_path <- "input_data/mye/2016/population_gla_.rds"
births_mye_path <-  "input_data/mye/2016/births_gla_.rds"
int_in_mye_path <-  "input_data/mye/2016/international_in_gla_.rds"
int_out_mye_path <- "input_data/mye/2016/international_out_gla_.rds"

int_in_10yr_avg_2016 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
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
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
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
popn_mye_path <- paste0("input_data/mye/2018/population_gla_2019-11-13.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_gla_2019-11-13.rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla_2019-11-13.rds")

int_in_10yr_avg_2018 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
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
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
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
  popn_mye_path = popn_mye_path, births_mye_path = births_mye_path,
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
popn_mye_path <- "input_data/mye/2019/temp_gla_population.rds"
births_mye_path <- "input_data/mye/2019/temp_births.rds"
int_out_mye_path <- "input_data/mye/2019/temp_gla_international_out.rds"
int_in_mye_path <-  "input_data/mye/2019/temp_gla_international_in.rds"

#10 year average

int_in_10yr_avg_2019 <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
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
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
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
  popn_mye_path = popn_mye_path,
  births_mye_path = births_mye_path,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 5,
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

