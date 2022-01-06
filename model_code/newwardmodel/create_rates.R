library(dplyr)
library(data.table)
library(assertthat)
devtools::load_all('model_code/popmodules')

data_dir <- "input_data/new_ward_model/"

ward_pop <- paste0(data_dir, "ward_population_WD20CD.rds") %>% readRDS()
ward_births <- paste0(data_dir, "ward_births_WD20CD.rds") %>% readRDS()
ward_deaths <- paste0(data_dir, "ward_deaths_WD20CD.rds") %>% readRDS()
ward_in_mig <- paste0(data_dir, "ward_inflow_WD20CD.rds") %>% readRDS() %>% data.frame()
ward_out_mig <- paste0(data_dir, "ward_outflow_WD20CD.rds") %>% readRDS() %>% data.frame()

#-------------------------------------------------------------------------------
denominator_popn <- ward_pop %>%
  popn_age_on(births = ward_births,
              col_aggregation=c("gss_code_ward", "year", "sex", "age"),
              col_geog = "gss_code_ward")

#-------------------------------------------------------------------------------
ward_pop_x <- filter(ward_pop, gss_code_ward != "E05000026")
mort_rates <- scaled_mortality_curve(popn = ward_pop,
                                     births = ward_births,
                                     deaths = ward_deaths,
                                     target_curves = "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "deaths",
                                     output_col = "rate",
                                     col_aggregation=c("gss_code", "gss_code_ward", "year", "sex"),
                                     col_geog = c("gss_code","gss_code_ward")) %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/mortality/npp_mortality_trend.rds",
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal")

#-------------------------------------------------------------------------------

fert_rates <- scaled_fertility_curve(popn = ward_pop,
                                     births = ward_births,
                                     target_curves = "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds",
                                     last_data_year = 2019,
                                     n_years_to_avg = 5,
                                     avg_or_trend = "trend",
                                     data_col = "births",
                                     output_col = "rate",
                                     col_geog = c("gss_code","gss_code_ward"))  %>% 
  project_rates_npp(rate_col = "rate",
                    rates_trajectory = "input_data/fertility/npp_fertility_trend.rds",
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    npp_var = "2018_principal") %>% 
complete_popn_dataframe()

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

#Covid Rates IN
total_in <- readRDS('outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/dom_in.rds') %>% 
  left_join(readRDS('outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/int_in.rds'),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(total_in = dom_in + int_in) %>% 
  select(-dom_in, -int_in)

total_in_avg <- filter(total_in, year %in% 2016:2020) %>%
  group_by(gss_code, age, sex) %>% 
  summarise(avg = sum(total_in)/5) %>% 
  data.frame()

#factors

scaling_in_2020 <- filter(total_in, year == 2020) %>% 
  left_join(total_in_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2020 = ifelse(avg == 0, 0, total_in/avg)) %>% 
  select(-total_in, -avg)

scaling_in_2021 <- filter(total_in, year == 2021) %>% 
  left_join(total_in_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2021 = ifelse(avg == 0, 0, total_in/avg)) %>% 
  select(-total_in, -avg)

scaling_in_2022 <- filter(total_in, year == 2022) %>% 
  left_join(total_in_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2022 = ifelse(avg == 0, 0, total_in/avg)) %>% 
  select(-total_in, -avg)

#rates

scaled_IN_2020 <- scaling_in_2020 %>% 
  left_join(in_mig_flows_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(in_flow = in_flow*scaling_2020) %>% 
  select(names(in_mig_flows_5))

scaled_IN_2021 <- scaling_in_2021 %>% 
  left_join(in_mig_flows_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(in_flow = in_flow*scaling_2021) %>% 
  select(names(in_mig_flows_5))

scaled_IN_2022 <- scaling_in_2022 %>% 
  left_join(in_mig_flows_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(in_flow = in_flow*scaling_2022) %>% 
  select(names(in_mig_flows_5))

rm(scaling_in_2020, scaling_in_2021, scaling_in_2022, total_in_avg)
#-------------------------------------------------------------------------------

#Covid Rates OUT
total_out <- readRDS('outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/dom_out.rds') %>% 
  left_join(readRDS('outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/int_out.rds'),
            by = c("year", "gss_code", "age", "sex")) %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(total_out = dom_out + int_out) %>% 
  select(-dom_out, -int_out)

total_out_avg <- filter(total_out, year %in% 2016:2020) %>%
  group_by(gss_code, age, sex) %>% 
  summarise(avg = sum(total_out)/5) %>% 
  data.frame()

#factors

scaling_out_2020 <- filter(total_out, year == 2020) %>% 
  left_join(total_out_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2020 = ifelse(avg==0, 0, total_out/avg)) %>% 
  select(-total_out, -avg)

scaling_out_2021 <- filter(total_out, year == 2021) %>% 
  left_join(total_out_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2021 = ifelse(avg==0, 0, total_out/avg)) %>% 
  select(-total_out, -avg)

scaling_out_2022 <- filter(total_out, year == 2022) %>% 
  left_join(total_out_avg, by = c("gss_code", "age", "sex")) %>% 
  mutate(scaling_2022 = ifelse(avg==0, 0, total_out/avg)) %>% 
  select(-total_out, -avg)

#rates

scaled_OUT_2020 <- scaling_out_2020 %>% 
  left_join(out_mig_rates_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(out_rate = out_rate*scaling_2020) %>% 
  select(names(out_mig_rates_5))

scaled_OUT_2021 <- scaling_out_2021 %>% 
  left_join(out_mig_rates_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(out_rate = out_rate*scaling_2021) %>% 
  select(names(out_mig_rates_5))

scaled_OUT_2022 <- scaling_out_2022 %>% 
  left_join(out_mig_rates_5, by = c("gss_code", "year", "age", "sex")) %>% 
  mutate(out_rate = out_rate*scaling_2022) %>% 
  select(names(out_mig_rates_5))

rm(scaling_out_2020, scaling_out_2021, scaling_out_2022, total_out_avg)

#-------------------------------------------------------------------------------

saveRDS(mort_rates, paste0(data_dir, "mortality_rates_WD20CD.rds"))
saveRDS(fert_rates,  paste0(data_dir, "fertility_rates_WD20CD.rds"))

saveRDS(in_mig_flows_5,  paste0(data_dir, "in_migration_flows_WD20CD_5yr_avg.rds"))
saveRDS(out_mig_rates_5,  paste0(data_dir, "out_migration_rates_WD20CD_5yr_avg.rds"))
saveRDS(in_mig_flows_10,  paste0(data_dir, "in_migration_flows_WD20CD_10yr_avg.rds"))
saveRDS(out_mig_rates_10,  paste0(data_dir, "out_migration_rates_WD20CD_10yr_avg.rds"))

saveRDS(scaled_IN_2020,  paste0(data_dir, "in_migration_flows_WD20CD_Covid_2020.rds"))
saveRDS(scaled_IN_2021,  paste0(data_dir, "in_migration_flows_WD20CD_Covid_2021.rds"))
saveRDS(scaled_IN_2022,  paste0(data_dir, "in_migration_flows_WD20CD_Covid_2022.rds"))

saveRDS(scaled_OUT_2020,  paste0(data_dir, "out_migration_rates_WD20CD_Covid_2020.rds"))
saveRDS(scaled_OUT_2021,  paste0(data_dir, "out_migration_rates_WD20CD_Covid_2021.rds"))
saveRDS(scaled_OUT_2022,  paste0(data_dir, "out_migration_rates_WD20CD_Covid_2022.rds"))

rm(list=ls())
