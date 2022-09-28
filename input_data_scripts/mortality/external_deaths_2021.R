library(dplyr)
library(popmodules)

message("External deaths for 2021")

popn_path <- "input_data/mye/2020/population_gla.rds"
mort_rates_path <- "input_data/mortality/mort_rates_5yr_trend_2020.rds"
fert_rates_path <- "input_data/fertility/fert_rates_5yr_trend_2020.rds"
covid_deaths_path <- "input_data/scenario_data/covid19_deaths.rds"

#-------------------------------------------------------------------------------

aged_popn <- readRDS(popn_path) %>%
  filter(year == 2020) %>%
  popn_age_on()

fertility_rates <- readRDS(fert_rates_path) %>%
  filter(year == 2021) %>%
  complete_popn_dataframe(ages = 1:90)

mortality_rates <- readRDS(mort_rates_path) %>%
  filter(year == 2021)

births <- apply_rate_to_population(aged_popn,
                                   fertility_rates,
                                   col_out = "births") %>%
  sum_births_and_split_by_sex_ratio() %>% 
  rename(popn = births)

#TODO when we have additional births for 2021 replace the above with this
#births <- sum_births_and_split_by_sex_ratio(additional_births)

deaths <- rbind(aged_popn, births) %>% 
  apply_rate_to_population(rates = mortality_rates,
                           col_popn = "popn",
                           col_rate = "rate",
                           col_out = "deaths")

covid_deaths <- readRDS(covid_deaths_path) %>% 
  filter(year == 2021) %>% 
  mutate(deaths = upc*-1) %>% 
  select(-upc)

additional_deaths <- rbind(deaths, covid_deaths) %>% 
  group_by(year, gss_code, sex, age) %>% 
  summarise(deaths = sum(deaths),
            .groups = "drop_last") %>% 
  data.frame()

saveRDS(additional_deaths, "input_data/mortality/external_deaths_2021.rds")

rm(list = ls())
