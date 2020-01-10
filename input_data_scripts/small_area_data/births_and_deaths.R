library(dplyr)
library(tidyr)
library(stringr)

#TODO is there any point having the model data save all births and deaths back to 2001, they're big files as a result
#TODO conventions around naming of age groups (01_05 or 1_5, 85+)

#paths
lsoa_births_path <- "Q:/Teams/D&PA/Data/births_and_deaths/lsoa_births_by_aom_deaths_2001_2018/lsoa_births_2001to2018.rds"
lsoa_deaths_path <- "Q:/Teams/D&PA/Data/births_and_deaths/lsoa_births_by_aom_deaths_2001_2018/lsoa_deaths_2001to2018.rds"

#lookup
lsoa_to_ward <- readRDS("input_data/lookup/2011_lsoa_to_ward.rds")
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds")%>%
  select(-ward_name)
london_wards <- filter(ward_to_district, str_detect(gss_code, "E09"))
london_wards <- london_wards$gss_code_ward

#NB: There are 5 wards in Isles of Scilly but only 1 LSOA (E01019077)

#births
lsoa_births <- readRDS(lsoa_births_path) %>%
  pivot_longer(c("a24_under","a25to34","a35_over"), names_to = "age_group", values_to = "births") %>%
  mutate(age_group = case_when(age_group == "a24_under" ~ "15_24",
                               age_group == "a25to34" ~ "25_34",
                               age_group == "a35_over" ~ "35_49")) %>%
  select(year, gss_code_lsoa = LSOA11CD, age_group, births) %>%
  filter(gss_code_lsoa != "E01019077")

ward_births <- left_join(lsoa_births, lsoa_to_ward, by="gss_code_lsoa") %>%
  filter(gss_code_ward %in% london_wards) %>%
  .aggregate_city_wards("births" ) %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_ward, age_group) %>%
  summarise(births = sum(births)) %>%
  as.data.frame() %>%
  mutate(year = as.numeric(year))

if(length(unique(ward_births$gss_code_ward))!=625){message("Warning: Wrong number of wards")}

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
  filter(gss_code_ward %in% london_wards) %>%
  .aggregate_city_wards("deaths") %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_ward, sex, age_group) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()

if(length(unique(ward_deaths$gss_code_ward))!=625){message("Warning: Wrong number of wards")}

saveRDS(ward_deaths, "input_data/small_area_model/ward_deaths_2001_2018.rds")

