library(dplyr)
library(tidyr)
library(stringr)

#paths
lsoa_births_path <- "Q:/Teams/D&PA/Data/births_and_deaths/lsoa_births_by_aom_deaths_2001_2018/lsoa_births_2001to2018.rds"
lsoa_deaths_path <- "Q:/Teams/D&PA/Data/births_and_deaths/lsoa_births_by_aom_deaths_2001_2018/lsoa_deaths_2001to2018.rds"
lsoa_to_ward_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to ward.rds"


#lookup
#NB: There are 5 wards in Isles of Scilly but only 1 LSOA (E01019077)
lsoa_to_ward <- readRDS(lsoa_to_ward_path)

#births
lsoa_births <- readRDS(lsoa_births_path) %>%
  pivot_longer(c("a24_under","a25to34","a35_over"), names_to = "age_group", values_to = "births") %>%
  mutate(age_group = case_when(age_group == "a24_under" ~ "15_24",
                               age_group == "a25to34" ~ "25_34",
                               age_group == "a35_over" ~ "35_49")) %>%
  select(year, gss_code_lsoa = LSOA11CD, age_group, births) %>%
  filter(gss_code_lsoa != "E01019077")

ward_births <- left_join(lsoa_births, lsoa_to_ward, by="gss_code_lsoa") %>%
  group_by(year, gss_code_ward, age_group) %>%
  summarise(births = sum(births)) %>%
  as.data.frame()

saveRDS(ward_births, "input_data/small_area_model/ward_births_2001_2018.rds")

#deaths
lsoa_deaths <- readRDS(lsoa_deaths_path) %>%
  pivot_longer(c("<1","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39",
                 "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+"),
               names_to = "age_group", values_to = "deaths") %>%
  filter(sex %in% c("male","female")) %>%
  select(year, gss_code_lsoa = LSOA11CD, sex, age_group, deaths) %>%
  mutate(year = as.numeric(year),
         age_group = sub(pattern="-", replacement="_", x=.data$age_group)) %>%
  mutate(age_group = ifelse(age_group == "<1", "0",
                            ifelse(age_group == "01_04", "1_4",
                                   ifelse(age_group == "05_09", "5_9",
                                          age_group)))) %>%
  filter(gss_code_lsoa != "E01019077")

ward_deaths <- left_join(lsoa_deaths, lsoa_to_ward, by="gss_code_lsoa") %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_ward, sex, age_group) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()




                            
                            
                            