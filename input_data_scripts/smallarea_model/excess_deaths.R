library(dplyr)
library(popmodules)

message("Excess deaths")

input_data_dir <- "input_data/smallarea_model/"

borough_popn <- readRDS("outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/population.rds")
borough_deaths <-  readRDS("outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/deaths.rds")
borough_mort_rates <- readRDS("input_data/mortality/mort_rates_5yr_trend_2020.rds")

ward_2013_lookup <- readRDS("input_data/smallarea_model/lookups/ward_2013_name_lookup.rds") %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  group_by(gss_code) %>% 
  mutate(n_wards = n())

ward_2022_lookup <- readRDS("input_data/smallarea_model/lookups/ward_2022_name_lookup.rds") %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  group_by(gss_code) %>% 
  mutate(n_wards = n())

#-------------------------------------------------------------------------------

borough_excess_deaths <- apply_rate_to_population(popn = borough_popn,
                                                rates = borough_mort_rates,
                                                col_out = "expected_deaths") %>% 
  filter(year %in% 2020:2021, substr(gss_code,1,3)=="E09") %>%
  left_join(borough_deaths, by = c("year", "gss_code", "sex", "age")) %>% 
  mutate(excess_deaths = deaths - expected_deaths)

excess_deaths_WD13 <- left_join(ward_2013_lookup, borough_excess_deaths, by = "gss_code") %>% 
  mutate(excess_deaths = excess_deaths / n_wards) %>% 
  select(gss_code, gss_code_ward, year, sex, age, excess_deaths)

excess_deaths_WD22 <- left_join(ward_2022_lookup, borough_excess_deaths, by = "gss_code") %>% 
  mutate(excess_deaths = excess_deaths / n_wards) %>% 
  select(gss_code, gss_code_ward, year, sex, age, excess_deaths)

#-------------------------------------------------------------------------------

saveRDS(excess_deaths_WD13, paste0(input_data_dir, "processed/excess_covid_deaths_WD13CD.rds"))
saveRDS(excess_deaths_WD22, paste0(input_data_dir, "processed/excess_covid_deaths_WD22CD.rds"))

rm(list=ls())
