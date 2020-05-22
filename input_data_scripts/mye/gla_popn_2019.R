library(dplyr)
devtools::load_all("model_code/popmodules")

#read in all the other components
gla_popn <- readRDS("input_data/mye/2018/population_gla_2019-11-13.rds") %>%
  select(year, gss_code, sex, age, popn) %>%
  recode_gss_codes(col_aggregation = c("year","gss_code","sex","age"), recode_to_year = 2020)

births <- readRDS("input_data/mye/2019/temp_births.rds") %>%
  filter(year == 2019)

deaths <- readRDS("input_data/mye/2019/temp_deaths.rds") %>%
  filter(year == 2019)

gla_international_in <- readRDS("input_data/mye/2019/temp_gla_international_in.rds") %>%
  filter(year == 2019)

gla_international_out <- readRDS("input_data/mye/2019/temp_gla_international_out.rds") %>%
  filter(year == 2019)

dom_in <- readRDS("input_data/domestic_migration/2019/temp_domestic_migration_in.rds") %>%
  filter(year == 2019) %>%
  filter_to_LAs()

dom_out <- readRDS("input_data/domestic_migration/2019/temp_domestic_migration_out.rds") %>%
  filter(year == 2019) %>%
  filter_to_LAs()

popn_2019 <- gla_popn %>%
  filter(year == 2018) %>%
  construct_popn_from_components(addition_data = list(births, gla_international_in, dom_in),
                                 subtraction_data = list(deaths, gla_international_out, dom_out),
                                 col_aggregation = c("gss_code", "age", "sex")) %>%
  mutate(year = 2019) %>%
  select(year, gss_code, sex, age, popn) %>%
  arrange(year, gss_code, sex, age) %>%
  check_negative_values(data_col = "popn")

gla_mye <- rbind(gla_popn, popn_2019)

#check geographies match
validate_same_geog(births, gla_international_in)
validate_same_geog(births, gla_international_out)
validate_same_geog(births, deaths)
validate_same_geog(gla_popn, popn_2019)
assert_that(!any(is.na(popn_2019)))

saveRDS(gla_mye, "input_data/mye/2019/temp_gla_population.rds")

#rm(list=ls())