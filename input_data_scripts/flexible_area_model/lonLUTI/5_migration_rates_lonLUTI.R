library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("lonLUTI3 migration rates")

input_data_dir <- "input_data/flexible_area_model/"

lonLUTI3_to_la <- paste0(input_data_dir, "lookups/lonLUTI3_name_lookup.rds") %>%
  readRDS() %>%
  select(lonLUTI3, gss_code)

lonLUTI3_pop <- paste0(input_data_dir, "backseries/lonLUTI3_population.rds") %>%
  readRDS()%>%
  left_join(lonLUTI3_to_la, by = "lonLUTI3")

lonLUTI3_births <- paste0(input_data_dir, "backseries/lonLUTI3_births.rds") %>%
  readRDS() %>%
  left_join(lonLUTI3_to_la, by = "lonLUTI3")

lonLUTI3_in_mig <- paste0(input_data_dir, "backseries/lonLUTI3_inflow.rds") %>% readRDS()

lonLUTI3_out_mig <- paste0(input_data_dir, "backseries/lonLUTI3_outflow.rds") %>% readRDS()

#-------------------------------------------------------------------------------

denominator_popn <- lonLUTI3_pop %>%
  popn_age_on(births = lonLUTI3_births,
              col_aggregation=c("gss_code","lonLUTI3", "year", "sex", "age"),
              col_geog = c("gss_code", "lonLUTI3"))

#-------------------------------------------------------------------------------
# Averaged migration flows and rates

out_mig_rates_5 <- denominator_popn %>% 
  left_join(lonLUTI3_out_mig, by = c("lonLUTI3", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5, 
                                 last_data_year = 2020, 
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "lonLUTI3", "sex", "age"),
                                 project_rate_from = 2021) %>% 
  mutate(out_rate = ifelse(out_rate > 0.8, 0.8, out_rate)) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

in_mig_flows_5 <- lonLUTI3_in_mig %>% 
  left_join(lonLUTI3_to_la, by = "lonLUTI3") %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5, 
                                 last_data_year = 2020,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "lonLUTI3", "sex", "age"),
                                 project_rate_from = 2021) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

# Approximate Covid-affected migration rates so that

trend_projection_migration <- "Q:/Teams/D&PA/Demography/Projections/population_models/outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/"

#Borough covid flows IN
total_in <- readRDS(paste0(trend_projection_migration, 'dom_in.rds')) %>% 
  left_join(readRDS(paste0(trend_projection_migration, 'int_in.rds')),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,2,3) %in% c("06","07","08","09")) %>% 
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

#apply to lonLUTI3 5-year avg

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
  filter(substr(gss_code,2,3) %in% c("06","07","08","09")) %>% 
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

#apply to lonLUTI3 5-year avg

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

saveRDS(in_mig_flows_5,  paste0(input_data_dir, "processed/in_migration_flows_lonLUTI3_5yr_avg.rds"))
saveRDS(out_mig_rates_5,  paste0(input_data_dir, "processed/out_migration_rates_lonLUTI3_5yr_avg.rds"))

saveRDS(scaled_IN_2021,  paste0(input_data_dir, "processed/in_migration_flows_lonLUTI3_Covid_2021.rds"))
saveRDS(scaled_IN_2022,  paste0(input_data_dir, "processed/in_migration_flows_lonLUTI3_Covid_2022.rds"))

saveRDS(scaled_OUT_2021,  paste0(input_data_dir, "processed/out_migration_rates_lonLUTI3_Covid_2021.rds"))
saveRDS(scaled_OUT_2022,  paste0(input_data_dir, "processed/out_migration_rates_lonLUTI3_Covid_2022.rds"))

rm(list=ls())
