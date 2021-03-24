library(dplyr)
library(popmodules)

message("scenario migration rates (2019 bpo)")

gla_popn_mye_path <- "input_data/mye/2019/population_gla.rds"
births_mye_path <-  "input_data/mye/2019/births_ons.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2019/domestic_migration_flows_ons_(2020_geog).rds"
int_in_mye_path <-  "input_data/mye/2019/int_in_gla.rds"
int_out_mye_path <-  "input_data/mye/2019/int_out_gla.rds"

#---------------------------------

#domestic

rates_backseries <- get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                        popn_mye_path = gla_popn_mye_path,
                                        births_mye_path = births_mye_path,
                                        years_backseries = 2002:2019,
                                        col_partial_match = c("gss_out","gss_in"),
                                        col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                        col_component = "value",
                                        rate_cap = NULL)

dom_rates_5_year_2019 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2019,
                                n_years_to_avg = 5,
                                col_rate = "rate",
                                rate_cap = 0.8)

into_london <- dom_rates_5_year_2019 %>% 
  filter(substr(gss_in,1,3)=="E09") %>% 
  filter(substr(gss_out,1,3)!="E09")

outfrom_london <- dom_rates_5_year_2019 %>% 
  filter(substr(gss_out,1,3)=="E09") %>% 
  filter(substr(gss_in,1,3)!="E09")

everywhere_else <- dom_rates_5_year_2019 %>% 
  filter(substr(gss_in,1,3) != "E09") %>% 
  filter(substr(gss_out,1,3) != "E09") %>% 
  rbind(filter(dom_rates_5_year_2019,
               substr(gss_out,1,3) == "E09" & substr(gss_in,1,3)=="E09"))

#---------------------------------

dom_in_80 <- into_london %>% mutate(rate = rate * 0.8)
dom_in_20 <- into_london %>% mutate(rate = rate * 0.2)
dom_in_50 <- into_london %>% mutate(rate = rate * 0.5)
dom_in_30 <- into_london %>% mutate(rate = rate * 0.3)

dom_out_90 <- outfrom_london %>% mutate(rate = rate * 0.9)
dom_out_60 <- outfrom_london %>% mutate(rate = rate * 0.6)
dom_out_50 <- outfrom_london %>% mutate(rate = rate * 0.5)
dom_out_70 <- outfrom_london %>% mutate(rate = rate * 0.7)

everywhere_else <- everywhere_else %>% mutate(rate = rate * 0.7)

#---------------------------------

bpo_scenario_1_yr_2020 <- rbind(dom_in_80, dom_out_90, everywhere_else)
bpo_scenario_1_yr_2021 <- rbind(dom_in_20, dom_out_60, everywhere_else)
bpo_scenario_1_yr_2022 <- rbind(dom_in_50, dom_out_50, everywhere_else)

bpo_scenario_3_yr_2021 <- rbind(dom_in_20, dom_out_70, everywhere_else)
bpo_scenario_3_yr_2022 <- rbind(dom_in_30, dom_out_50, everywhere_else)

#---------------------------------

saveRDS(bpo_scenario_1_yr_2020, "input_data/scenario_data/bpo_dom_scenario_1_yr_2020.rds")
saveRDS(bpo_scenario_1_yr_2021, "input_data/scenario_data/bpo_dom_scenario_1_yr_2021.rds")
saveRDS(bpo_scenario_1_yr_2022, "input_data/scenario_data/bpo_dom_scenario_1_yr_2022.rds")

saveRDS(bpo_scenario_3_yr_2021, "input_data/scenario_data/bpo_dom_scenario_3_yr_2021.rds")
saveRDS(bpo_scenario_3_yr_2022, "input_data/scenario_data/bpo_dom_scenario_3_yr_2022.rds")

#---------------------------------

#International

int_in_5yr_avg_2019 <- calculate_mean_international_rates_or_flows(
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

int_out_5yr_avg_2019 <- calculate_mean_international_rates_or_flows(
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

#---------------------------------

bpo_int_in_scenario_1_yr_2020 <- int_in_5yr_avg_2019 %>% mutate(int_in = int_in * 0.8)
bpo_int_in_scenario_1_yr_2021 <- int_in_5yr_avg_2019 %>% mutate(int_in = int_in * 0.2)
bpo_int_in_scenario_1_yr_2022 <- int_in_5yr_avg_2019 %>% mutate(int_in = int_in * 0.5)

bpo_int_out_scenario_1_yr_2020 <- int_out_5yr_avg_2019 %>% mutate(int_out = int_out * 0.9)
bpo_int_out_scenario_1_yr_2021 <- int_out_5yr_avg_2019 %>% mutate(int_out = int_out * 0.5)
bpo_int_out_scenario_1_yr_2022 <- int_out_5yr_avg_2019 %>% mutate(int_out = int_out * 0.3)

#---------------------------------

#bpo_int_in_scenario_3_yr_2020 <- int_in_5yr_avg_2019 %>% mutate(int_in = int_in * 0.8)
#bpo_int_in_scenario_3_yr_2021 <- int_in_5yr_avg_2019 %>% mutate(int_in = int_in * 0.2)
bpo_int_in_scenario_3_yr_2022 <- int_in_5yr_avg_2019 %>% mutate(int_in = int_in * 0.3)

#bpo_int_out_scenario_3_yr_2020 <- int_out_5yr_avg_2019 %>% mutate(int_out = int_out * 0.9)
bpo_int_out_scenario_3_yr_2021 <- int_out_5yr_avg_2019 %>% mutate(int_out = int_out * 0.7)
#bpo_int_out_scenario_3_yr_2022 <- int_out_5yr_avg_2019 %>% mutate(int_out = int_out * 0.3)

#---------------------------------

saveRDS(bpo_int_in_scenario_1_yr_2020, "input_data/scenario_data/bpo_int_in_scenario_1_yr_2020.rds")
saveRDS(bpo_int_in_scenario_1_yr_2021, "input_data/scenario_data/bpo_int_in_scenario_1_yr_2021.rds")
saveRDS(bpo_int_in_scenario_1_yr_2022, "input_data/scenario_data/bpo_int_in_scenario_1_yr_2022.rds")

saveRDS(bpo_int_out_scenario_1_yr_2020, "input_data/scenario_data/bpo_int_out_scenario_1_yr_2020.rds")
saveRDS(bpo_int_out_scenario_1_yr_2021, "input_data/scenario_data/bpo_int_out_scenario_1_yr_2021.rds")
saveRDS(bpo_int_out_scenario_1_yr_2022, "input_data/scenario_data/bpo_int_out_scenario_1_yr_2022.rds")
#---------------------------------

#saveRDS(bpo_int_in_scenario_3_yr_2020, "input_data/scenario_data/bpo_int_in_scenario_3_yr_2020.rds")
#saveRDS(bpo_int_in_scenario_3_yr_2021, "input_data/scenario_data/bpo_int_in_scenario_3_yr_2021.rds")
saveRDS(bpo_int_in_scenario_3_yr_2022, "input_data/scenario_data/bpo_int_in_scenario_3_yr_2022.rds")

#saveRDS(bpo_int_out_scenario_3_yr_2020, "input_data/scenario_data/bpo_int_out_scenario_3_yr_2020.rds")
saveRDS(bpo_int_out_scenario_3_yr_2021, "input_data/scenario_data/bpo_int_out_scenario_3_yr_2021.rds")
#saveRDS(bpo_int_out_scenario_3_yr_2022, "input_data/scenario_data/bpo_int_out_scenario_3_yr_2022.rds")

rm(list=ls())
