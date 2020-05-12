library(dplyr)
devtools::load_all("model_code/popmodules")

#read in all the other components
gla_popn <- readRDS("input_data/mye/2018/population_gla_2019-11-13.rds") %>%
  select(year, gss_code, sex, age, popn) %>%
  recode_gss_codes(col_aggregation = c("year","gss_code","sex","age"), recode_to_year = 2020)

births <- readRDS("input_data/mye/2019/temp_births.rds") %>%
  filter(year == 2019) %>%
  rename(popn = births)

deaths <- readRDS("input_data/mye/2019/temp_deaths.rds") %>%
  filter(year == 2019) %>%
  mutate(popn = deaths*-1) %>%
  select(-deaths)

gla_international_in <- readRDS("input_data/mye/2019/temp_gla_international_in.rds") %>%
  filter(year == 2019) %>%
  rename(popn = int_in)

gla_international_out <- readRDS("input_data/mye/2019/temp_gla_international_out.rds") %>%
  filter(year == 2019) %>%
  mutate(popn = int_out*-1) %>%
  select(-int_out)

dom_in <- readRDS("input_data/domestic_migration/2019/temp_domestic_migration_in.rds") %>%
  filter(year == 2019) %>%
  rename(popn = dom_in)

dom_out <- readRDS("input_data/domestic_migration/2019/temp_domestic_migration_out.rds") %>%
  filter(year == 2019) %>%
  mutate(popn = dom_out*-1) %>%
  select(-dom_out)

popn_2019 <- gla_popn %>%
  filter(year == 2018) %>%
  rbind(births, deaths, gla_international_in, gla_international_out, dom_in, dom_out) %>%
  filter(gss_code %in% unique(births$gss_code)) %>% 
  group_by(gss_code, sex, age) %>%
  summarise(popn = sum(popn)) %>%
  as.data.frame() %>%
  mutate(year = 2019) %>%
  select(year, gss_code, sex, age, popn) %>%
  check_negative_values(data_col = "popn")

gla_mye <- rbind(gla_popn, popn_2019)

#check geographies match
validate_same_geog(births, gla_international_in)
validate_same_geog(births, gla_international_out)
validate_same_geog(births, deaths)
validate_same_geog(gla_popn, popn_2019)
sum(is.na(popn_2019))

saveRDS(gla_mye, "input_data/mye/2019/temp_gla_population.rds")

#rm(list=ls())