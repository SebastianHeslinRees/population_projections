library(dplyr)
library(data.table)
library(assertthat)
library(popmodules)

message("Ward 2013 migration rates")

input_data_dir <- "input_data/smallarea_model/"

ward_pop <- paste0(input_data_dir, "backseries/ward_population_WD13CD.rds") %>% readRDS()
ward_births <- paste0(input_data_dir, "backseries/ward_births_WD13CD.rds") %>% readRDS()
ward_in_mig <- paste0(input_data_dir, "backseries/ward_inflow_WD13CD.rds") %>% readRDS() %>% data.frame()
ward_out_mig <- paste0(input_data_dir, "backseries/ward_outflow_WD13CD.rds") %>% readRDS() %>% data.frame()

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
                                 last_data_year = 2019,
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  project_forward_flat(2050)

out_mig_rates_10 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10,
                                 last_data_year = 2019,
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  project_forward_flat(2050)

out_mig_rates_4 <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 4,
                                 last_data_year = 2019,
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

in_mig_flows_5 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 5,
                                 last_data_year = 2019,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

in_mig_flows_10 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10,
                                 last_data_year = 2019,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

in_mig_flows_4 <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 4,
                                 last_data_year = 2019,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2020) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

# This is likely to change
# I'm trying to approximate some Covid-affected migration rates so that
# the projection I produce for QA are in the right ball park
# The files are coming from my machine

#trend_projection_migration <- "Q:/Teams/D&PA/Demography/Projections/population_models/outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/"
trend_projection_migration <- "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/"

#Covid Rates IN
borough_in <- readRDS(paste0(trend_projection_migration, 'dom_in.rds')) %>% 
  left_join(readRDS(paste0(trend_projection_migration, 'int_in.rds')),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(borough_in = dom_in + int_in) %>% 
  select(-dom_in, -int_in)

borough_in_avg <- filter(borough_in, year %in% 2016:2020) %>%
  group_by(gss_code, age, sex) %>% 
  summarise(avg = sum(borough_in)/5, .groups = 'drop_last') %>% 
  data.frame()

#factors

scaling_in <- borough_in %>% 
  filter(year %in% 2020:2022) %>% 
  left_join(borough_in_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling = ifelse(avg == 0, 0, borough_in/avg)) %>% 
  select(-borough_in, -avg)

scaled_in <- in_mig_flows_5 %>% 
  filter(year %in% 2020:2022) %>% 
  left_join(scaling_in, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(in_flow = in_flow*scaling) %>% 
  select(names(in_mig_flows_5))

scaled_IN_2020 <-filter(scaled_in, year == 2020)
scaled_IN_2021 <-filter(scaled_in, year == 2021)
scaled_IN_2022 <-filter(scaled_in, year == 2022)


rm(scaling_in, scaled_in, borough_in_avg)
#-------------------------------------------------------------------------------

#Covid Rates OUT
borough_out <- readRDS(paste0(trend_projection_migration, 'dom_out.rds')) %>% 
  left_join(readRDS(paste0(trend_projection_migration, 'int_out.rds')),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(borough_out = dom_out + int_out) %>% 
  select(-dom_out, -int_out)

borough_out_avg <- filter(borough_out, year %in% 2016:2020) %>%
  group_by(gss_code, age, sex) %>% 
  summarise(avg = sum(borough_out)/5, .groups = 'drop_last') %>% 
  data.frame()

#factors

scaling_out <- borough_out %>% 
  filter(year %in% 2020:2022) %>% 
  left_join(borough_out_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling = ifelse(avg == 0, 0, borough_out/avg)) %>% 
  select(-borough_out, -avg)

scaled_out <- out_mig_rates_5 %>% 
  filter(year %in% 2020:2022) %>% 
  left_join(scaling_out, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(out_rate = out_rate*scaling) %>% 
  select(names(out_mig_rates_5))

scaled_OUT_2020 <-filter(scaled_out, year == 2020)
scaled_OUT_2021 <-filter(scaled_out, year == 2021)
scaled_OUT_2022 <-filter(scaled_out, year == 2022)

rm(scaling_out, scaled_out, borough_out_avg)

#-------------------------------------------------------------------------------

out_2018_10yr <- denominator_popn %>% 
  left_join(ward_out_mig, by = c("gss_code_ward", "year", "sex", "age")) %>% 
  mutate(out_rate = ifelse(popn==0, 0, outflow/popn)) %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10,
                                 last_data_year = 2018,
                                 data_col = "out_rate",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2019) %>% 
  project_forward_flat(2050)


in_2018_10yr <- ward_in_mig %>% 
  calculate_mean_from_backseries(n_years_to_avg = 10,
                                 last_data_year = 2018,
                                 data_col = "inflow",
                                 col_aggregation = c("gss_code", "gss_code_ward", "sex", "age"),
                                 project_rate_from = 2019) %>% 
  rename(in_flow = inflow) %>% 
  project_forward_flat(2050)

#-------------------------------------------------------------------------------

saveRDS(in_mig_flows_5,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_5yr_avg.rds"))
saveRDS(out_mig_rates_5,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_5yr_avg.rds"))
saveRDS(in_mig_flows_10,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_10yr_avg.rds"))
saveRDS(out_mig_rates_10,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_10yr_avg.rds"))

saveRDS(in_mig_flows_4,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_4yr_avg.rds"))
saveRDS(out_mig_rates_4,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_4yr_avg.rds"))

saveRDS(scaled_IN_2020,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_Covid_2020.rds"))
saveRDS(scaled_IN_2021,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_Covid_2021.rds"))
saveRDS(scaled_IN_2022,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_Covid_2022.rds"))

saveRDS(scaled_OUT_2020,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_Covid_2020.rds"))
saveRDS(scaled_OUT_2021,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_Covid_2021.rds"))
saveRDS(scaled_OUT_2022,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_Covid_2022.rds"))

saveRDS(in_2018_10yr,  paste0(input_data_dir, "processed/in_migration_flows_WD13CD_2018_10yr.rds"))
saveRDS(out_2018_10yr,  paste0(input_data_dir, "processed/out_migration_rates_WD13CD_2018_10yr.rds"))