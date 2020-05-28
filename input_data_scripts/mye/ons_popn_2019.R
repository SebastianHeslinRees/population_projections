devtools::load_all("model_code/popmodules")
library(dplyr)

female_2019 <- data.table::fread("Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2019/2019_mye_may_2020_release_females.csv",
                     header = TRUE) %>%
  tidyr::pivot_longer(cols = as.character(0:90), names_to = "age", values_to = "popn") %>%
  mutate(sex = "female")

male_2019 <- data.table::fread("Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2019/2019_mye_may_2020_release_males.csv",
                   header = TRUE) %>%
  tidyr::pivot_longer(cols = as.character(0:90), names_to = "age", values_to = "popn") %>%
  mutate(sex = "male")

mye_2019 <- rbind(female_2019, male_2019) %>% 
  mutate(popn = as.numeric(popn)) %>% 
  rename(gss_code = Code) %>% 
  filter(substr(gss_code,1,3) %in% c("E06","E07","E08","E09","W06","N92","S92")) %>%
  recode_gss_codes(data_cols="popn", recode_to_year = 2020) %>%
  mutate(year = 2019) %>% 
  select(year, gss_code, sex, age, popn) 

population_ons <- readRDS("input_data/mye/2018/population_ons.rds")%>%
  select(year, gss_code, sex, age, popn) %>% 
  recode_gss_codes(data_cols = "popn", recode_to_year = 2020) %>%
  rbind(mye_2019)

saveRDS(population_ons, "input_data/mye/2019/temp_ons_popn.rds")

rm(list=ls())