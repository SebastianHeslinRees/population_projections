library(dplyr)
library(popmodules)

dir.create("input_data/scenario_data", showWarnings = FALSE)

gla_popn_mye_path <- "input_data/mye/2019/population_gla.rds"
births_mye_path <-  "input_data/mye/2019/births_ons.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2019/domestic_migration_flows_ons_(2020_geog).rds"
int_in_mye_path <-  "input_data/mye/2019/int_in_gla.rds"
int_out_mye_path <-  "input_data/mye/2019/int_out_gla.rds"

#domestic
rates_backseries <- get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                        popn_mye_path = gla_popn_mye_path,
                                        births_mye_path = births_mye_path,
                                        years_backseries = 2002:2019,
                                        col_partial_match = c("gss_out","gss_in"),
                                        col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                        col_component = "value",
                                        rate_cap = NULL)

dom_rates_10_year_2019 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2019,
                                n_years_to_avg = 10,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_5_year_2019 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2019,
                                n_years_to_avg = 5,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_5_year_2012 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2012,
                                n_years_to_avg = 5,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_2017 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2017,
                                n_years_to_avg = 1,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_2008 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2008,
                                n_years_to_avg = 1,
                                col_rate = "rate",
                                rate_cap = 0.8)

#---------------------------------
#extreme case

#high out from LBs to other LAs (not LBs)
high_out_from_london <- filter(dom_rates_2017, substr(gss_out,1,3) == "E09") %>% 
  filter(substr(gss_in,1,3) != "E09")

#low in from other LAs (not LBs) to LBs
low_in_to_london <- filter(dom_rates_2008, substr(gss_in,1,3) == "E09") %>% 
  filter(substr(gss_out,1,3) != "E09")

#10 year between LBs
betwixt_london <- dom_rates_10_year_2019 %>% 
  filter(substr(gss_out,1,3) == "E09" & substr(gss_in,1,3)=="E09")
  
#10 year between everywhere outside London
other_areas <- dom_rates_10_year_2019 %>% 
  filter(substr(gss_in,1,3) != "E09") %>% 
  filter(substr(gss_out,1,3) != "E09")

#combine into 1 dataset
extreme_dom <- rbind(high_out_from_london, low_in_to_london,
                     betwixt_london, other_areas)

#test no duplicates
nrow(select(extreme_dom, -rate) %>% distinct()) == nrow(extreme_dom)

#---------------------------------

dom_covid_2020 <- dom_rates_5_year_2019 %>% 
  mutate(rate = rate * 0.7)

dom_covid_2021_22 <- dom_rates_5_year_2019 %>% 
  mutate(rate = rate * 0.5)

#---------------------------------

#### International ####

 # year averages of in and out for covid period
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

#10-year averages of in and out

int_in_10yr_avg_2019 <- calculate_mean_international_rates_or_flows(
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

int_out_10yr_avg_2019 <- calculate_mean_international_rates_or_flows(
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

#-------------------------

#covid int in
int_in_covid_2020 <- int_in_5yr_avg_2019 %>% 
  mutate(int_in = int_in * 0.7)

int_in_covid_2021_22 <- int_in_5yr_avg_2019 %>% 
  mutate(int_in = int_in * 0.5)

#covid int out
int_out_covid_2020 <- int_out_5yr_avg_2019 %>% 
  mutate(int_out = int_out * 0.7)

int_out_covid_2021_22 <- int_out_5yr_avg_2019 %>% 
  mutate(int_out = int_out * 0.5)

#---------------------------------------------

#Calculate gross flows for net scenarios

#long term int
ldn_10_year_in <- sum(filter(int_in_10yr_avg_2019, substr(gss_code,1,3)=="E09")$int_in)
ldn_10_year_out <- sum(filter(int_out_10yr_avg_2019, substr(gss_code,1,3)=="E09")$int_out)
ldn_10_year_net <- ldn_10_year_in - ldn_10_year_out

#---------------------------------------------
#125k net

diff_125k_net <- 125000-ldn_10_year_net
add_in_125k <- ldn_10_year_in + (diff_125k_net*(2/3))
sub_out_125k <- ldn_10_year_out - (diff_125k_net*(1/3))
scale_in_125k <- add_in_125k / ldn_10_year_in
scale_out_125k <- sub_out_125k / ldn_10_year_out

int_in_125k_ldn <- int_in_10yr_avg_2019 %>%
  mutate(int_in = int_in * scale_in_125k)

int_out_125k_ldn <- int_out_10yr_avg_2019 %>%
  mutate(int_out = int_out * scale_out_125k)

#---------------------------------------------
#50k net

diff_50k_net <- 50000-ldn_10_year_net
add_in_50k <- ldn_10_year_in + (diff_50k_net*(2/3))
sub_out_50k <- ldn_10_year_out - (diff_50k_net*(1/3))
scale_in_50k <- add_in_50k / ldn_10_year_in
scale_out_50k <- sub_out_50k / ldn_10_year_out

int_in_50k_ldn <- int_in_10yr_avg_2019 %>%
  mutate(int_in = int_in * scale_in_50k)

int_out_50k_ldn <- int_out_10yr_avg_2019 %>%
  mutate(int_out = int_out * scale_out_50k)

#---------------------------------------------
#zero migration scenarios

zero_int_in <- int_in_10yr_avg_2019 %>% 
  mutate(int_in = 0)

zero_int_out <- int_out_10yr_avg_2019 %>% 
  mutate(int_out = 0)

zero_dom <- dom_rates_10_year_2019 %>% 
  mutate(rate = 0)


#-----------------------

#save

saveRDS(dom_covid_2020, "input_data/scenario_data/dom_covid_70_percent.rds")
saveRDS(dom_covid_2021_22, "input_data/scenario_data/dom_covid_50_percent.rds")

#-----

saveRDS(dom_rates_5_year_2019, "input_data/scenario_data/domestic_high_out.rds")
saveRDS(dom_rates_5_year_2012, "input_data/scenario_data/domestic_low_out.rds")

#-----
saveRDS(int_in_covid_2020, "input_data/scenario_data/int_in_covid_2020.rds")
saveRDS(int_in_covid_2021_22, "input_data/scenario_data/int_in_covid_2021_22.rds")

#-----

saveRDS(int_out_covid_2020, "input_data/scenario_data/int_out_flow_covid_2020.rds")
saveRDS(int_out_covid_2021_22, "input_data/scenario_data/int_out_flow_covid_2021_22.rds")

#-----

saveRDS(int_in_125k_ldn, "input_data/scenario_data/international_high_in.rds")
saveRDS(int_in_50k_ldn, "input_data/scenario_data/international_low_in.rds")

#-----

saveRDS(int_out_125k_ldn, "input_data/scenario_data/international_high_out_flow.rds")
saveRDS(int_out_50k_ldn, "input_data/scenario_data/international_low_out_flow.rds")

#-----

saveRDS(int_in_10yr_avg_2019, "input_data/scenario_data/international_10yr_in_flows.rds")
saveRDS(int_out_10yr_avg_2019, "input_data/scenario_data/international_10yr_out_flows.rds")

#-----

saveRDS(extreme_dom, "input_data/scenario_data/domestic_extreme_scenario.rds")

#-----

saveRDS(zero_dom, "input_data/scenario_data/zero_domestic.rds")
saveRDS(zero_int_in, "input_data/scenario_data/zero_int_in.rds")
saveRDS(zero_int_out, "input_data/scenario_data/zero_int_out.rds")

#-----
