library(dplyr)
library(popmodules)

int_in_10yr <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2019/int_in_10yr_avg_2019.rds")

int_out_10yr <- calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path =  "input_data/mye/2019/int_out_ons.rds",
  last_data_year = 2019,
  n_years_to_avg = 10,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

ldn_out <- filter(int_out_10yr, substr(gss_code,1,3)=="E09")
out_total <- sum(ldn_out$int_out)

ldn_in <- filter(int_in_10yr, substr(gss_code,1,3)=="E09")
in_total <- sum(ldn_in$int_in)

in_target <- 90000 + out_total
in_scale <- in_target / in_total
sceanrio_90k <- int_in_10yr %>% 
  mutate(int_in = int_in * in_scale)

in_target <- 25000 + out_total
in_scale <- in_target / in_total
sceanrio_25k <- int_in_10yr %>% 
  mutate(int_in = int_in * in_scale)

saveRDS(sceanrio_25k, "input_data/scenario_data/int_in_for_25k_net_scenario.rds")
saveRDS(sceanrio_90k, "input_data/scenario_data/int_in_for_90k_net_scenario.rds")

int_out_10yr <- calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path =  "input_data/mye/2019/int_out_ons.rds",
  last_data_year = 2019,
  n_years_to_avg = 10,
  data_col = "int_out",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>% 
  project_forward_flat(2050)
saveRDS(int_out_10yr, "input_data/scenario_data/int_out_for_net_scenarios.rds")
