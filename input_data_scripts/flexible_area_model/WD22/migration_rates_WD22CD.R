library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("Ward 2022 migration rates")

input_data_dir <- "input_data/flexible_area_model/"

ward_pop <- paste0(input_data_dir, "backseries/ward_population_WD22CD.rds") %>% readRDS()
ward_births <- paste0(input_data_dir, "backseries/ward_births_WD22CD.rds") %>% readRDS()
ward_in_mig <- paste0(input_data_dir, "backseries/ward_inflow_WD22CD.rds") %>% readRDS() %>% data.frame()
ward_out_mig <- paste0(input_data_dir, "backseries/ward_outflow_WD22CD.rds") %>% readRDS() %>% data.frame()

#-------------------------------------------------------------------------------

denominator_popn <- ward_pop %>%
  popn_age_on(births = ward_births,
              col_aggregation=c("gss_code_ward", "year", "sex", "age"),
              col_geog = "gss_code_ward")

#-------------------------------------------------------------------------------
# Averaged migration flows and rates

out_mig_rates_5 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5, 
                                 last_data_year = 2020, 
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2021) %>% 
  mutate(out_rate = ifelse(out_rate > 0.8, 0.8, out_rate)) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

in_mig_flows_5 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5, 
                                 last_data_year = 2020,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2021) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

# Approximate  Covid-affected migration rates so that

trend_projection_migration <- "Q:/Teams/D&PA/Demography/Projections/population_models/outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/"

#Borough covid flows IN
total_in <- readRDS(paste0(trend_projection_migration, 'dom_in.rds')) %>% 
  left_join(readRDS(paste0(trend_projection_migration, 'int_in.rds')),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(total_in = dom_in + int_in) %>% 
  select(-dom_in, -int_in)

total_in_avg <- filter(total_in, year %in% 2016:2020) %>%
  group_by(gss_code, age, sex) %>% 
  summarise(avg = sum(total_in)/5, .groups = 'drop_last') %>% 
  data.frame()

#scaling factors based on ratio of projected IN to average IN

scaling_in_2021 <- filter(total_in, year == 2021) %>% 
  left_join(total_in_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2021 = ifelse(avg == 0, 0, total_in/avg)) %>% 
  select(-total_in, -avg)

scaling_in_2022 <- filter(total_in, year == 2022) %>% 
  left_join(total_in_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2022 = ifelse(avg == 0, 0, total_in/avg)) %>% 
  select(-total_in, -avg)

#apply to ward 5-year avg

scaled_IN_2021 <- scaling_in_2021 %>% 
  left_join(in_mig_flows_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(in_flow = in_flow*scaling_2021) %>% 
  select(names(in_mig_flows_5))

scaled_IN_2022 <- scaling_in_2022 %>% 
  left_join(in_mig_flows_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(in_flow = in_flow*scaling_2022) %>% 
  select(names(in_mig_flows_5))

rm(scaling_in_2021, scaling_in_2022, total_in_avg)
#-------------------------------------------------------------------------------

#Borough covid flows IN OUT
total_out <- readRDS(paste0(trend_projection_migration, 'dom_out.rds')) %>% 
  left_join(readRDS(paste0(trend_projection_migration, 'int_out.rds')),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(total_out = dom_out + int_out) %>% 
  select(-dom_out, -int_out)

total_out_avg <- filter(total_out, year %in% 2016:2020) %>%
  group_by(gss_code, age, sex) %>% 
  summarise(avg = sum(total_out)/5, .groups = 'drop_last') %>% 
  data.frame()

#scaling factors based on ratio of projected OUT to average OUT

scaling_out_2021 <- filter(total_out, year == 2021) %>% 
  left_join(total_out_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2021 = ifelse(avg==0, 0, total_out/avg)) %>% 
  select(-total_out, -avg)

scaling_out_2022 <- filter(total_out, year == 2022) %>% 
  left_join(total_out_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2022 = ifelse(avg==0, 0, total_out/avg)) %>% 
  select(-total_out, -avg)

#apply to ward 5-year avg

scaled_OUT_2021 <- scaling_out_2021 %>% 
  left_join(out_mig_rates_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(out_rate = out_rate*scaling_2021) %>% 
  select(names(out_mig_rates_5))

scaled_OUT_2022 <- scaling_out_2022 %>% 
  left_join(out_mig_rates_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(out_rate = out_rate*scaling_2022) %>% 
  select(names(out_mig_rates_5))

rm(scaling_out_2021, scaling_out_2022, total_out_avg)

#-------------------------------------------------------------------------------

saveRDS(in_mig_flows_5,  paste0(input_data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"))
saveRDS(out_mig_rates_5,  paste0(input_data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"))

saveRDS(scaled_IN_2021,  paste0(input_data_dir, "processed/in_migration_flows_WD22CD_Covid_2021.rds"))
saveRDS(scaled_IN_2022,  paste0(input_data_dir, "processed/in_migration_flows_WD22CD_Covid_2022.rds"))

saveRDS(scaled_OUT_2021,  paste0(input_data_dir, "processed/out_migration_rates_WD22CD_Covid_2021.rds"))
saveRDS(scaled_OUT_2022,  paste0(input_data_dir, "processed/out_migration_rates_WD22CD_Covid_2022.rds"))

rm(list=ls())
