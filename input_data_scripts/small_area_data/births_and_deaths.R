library(dplyr)
library(tidyr)
library(stringr)
library(popmodules)

#TODO is there any point having the model data save all births and deaths back to 2001, they're big files as a result
#TODO conventions around naming of age groups (01_05 or 1_5, 85+)

#paths
lsoa_births_path <- "Q:/Teams/D&PA/Data/births_and_deaths/lsoa_births_by_aom_deaths_2001_2018/lsoa_births_2001to2018.rds"
lsoa_deaths_path <- "Q:/Teams/D&PA/Data/births_and_deaths/lsoa_births_by_aom_deaths_2001_2018/lsoa_deaths_2001to2018.rds"

#lookups
lsoa_to_ward <- readRDS("input_data/lookup/2011_lsoa_to_ward.rds")
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>%
  select(-ward_name)
london_wards <- filter(ward_to_district, str_detect(gss_code, "E09"))
london_wards <- london_wards$gss_code_ward

lsoa_to_msoa <- readRDS("input_data/lookup/lsoa_to_msoa.rds")
msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds") %>%
  select(-msoa_name)
london_msoas <- filter(msoa_to_district, str_detect(gss_code, "E09"))
london_msoas <- london_msoas$gss_code_msoa

#NB: There are 5 wards in Isles of Scilly but only 1 LSOA (E01019077)

#births
lsoa_births <- readRDS(lsoa_births_path) %>%
  filter(sex == "persons") %>%
  pivot_longer(c("a24_under","a25to34","a35_over"), names_to = "age_group", values_to = "births") %>%
  mutate(age_group = case_when(age_group == "a24_under" ~ "15_24",
                               age_group == "a25to34" ~ "25_34",
                               age_group == "a35_over" ~ "35_49")) %>%
  select(year, gss_code_lsoa = LSOA11CD, age_group, births) %>%
  filter(gss_code_lsoa != "E01019077")

ward_births <- left_join(lsoa_births, lsoa_to_ward, by="gss_code_lsoa") %>%
  filter(gss_code_ward %in% london_wards) %>%
  aggregate_city_wards("births" ) %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_ward, age_group) %>%
  summarise(births = sum(births)) %>%
  as.data.frame() %>%
  mutate(year = as.numeric(year))

if(length(unique(ward_births$gss_code_ward))!=625){message("Warning: Wrong number of wards")}

msoa_births <- left_join(lsoa_births, lsoa_to_msoa, by="gss_code_lsoa") %>%
  filter(gss_code_msoa %in% london_msoas) %>%
  group_by(year, gss_code_msoa, age_group) %>%
  summarise(births = sum(births)) %>%
  as.data.frame() %>%
  mutate(year = as.numeric(year))

if(length(unique(msoa_births$gss_code_msoa))!=983){message("Warning: Wrong number of msoas")}

saveRDS(ward_births, "input_data/small_area_model/ward_births.rds")
saveRDS(msoa_births, "input_data/small_area_model/msoa_births.rds")

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
  aggregate_city_wards("deaths") %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_ward, sex, age_group) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()

if(length(unique(ward_deaths$gss_code_ward))!=625){message("Warning: Wrong number of wards")}

msoa_deaths <- left_join(lsoa_deaths, lsoa_to_msoa, by="gss_code_lsoa") %>%
  filter(gss_code_msoa %in% london_msoas) %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_msoa, sex, age_group) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()

if(length(unique(msoa_deaths$gss_code_msoa))!=983){message("Warning: Wrong number of msoas")}

saveRDS(ward_deaths, "input_data/small_area_model/ward_deaths.rds")
saveRDS(msoa_deaths, "input_data/small_area_model/msoa_deaths.rds")

ward_la_lookup <- readRDS("input_data/lookup/2011_ward_to_district.rds")
msoa_la_lookup <- readRDS("input_data/lookup/msoa_to_district.rds")

borough_deaths <- readRDS("input_data/mye/2018/deaths_ons.rds") %>%
  select(gss_code, year, sex, age, deaths)

#Single year ward births
births_sya_ward <- ward_births %>%
  left_join(ward_la_lookup, by="gss_code_ward") %>%
  group_by(year, gss_code,gss_code_ward) %>%
  summarise(male = sum(births)*(105/205),
            female = sum(births)*(100/205)) %>%
  tidyr::pivot_longer(c("male","female"), names_to = "sex", values_to = "births") %>%
  mutate(age = 0) %>% 
  select(year, gss_code, gss_code_ward, sex, age, births) %>%
  as.data.frame() 

saveRDS(births_sya_ward, "input_data/small_area_model/ward_sya_births.rds")


#Single year ward deaths
deaths_sya_ward <- ward_deaths %>%
  mutate(min = substr(age_group,1,2),
         min = ifelse(min == "1_", "1",
                      ifelse(min == "5_", "5",
                             min)),
         min = as.numeric(min)) %>%
  mutate(max = case_when(min == 0 ~ 0,
                         min == 1 ~ 4,
                         min == 85 ~ 90,
                         TRUE ~ min + 4)) %>%
  left_join(ward_la_lookup, by="gss_code_ward") %>%
  filter(year %in% borough_deaths$year)


past_deaths <- list()
for(i in unique(deaths_sya_ward$age_group)){
  
  a <- filter(deaths_sya_ward, age_group ==i)
  past_deaths[[i]] <- distribute_within_age_band(a, borough_deaths, "deaths", "deaths",
                                                 unique(a$min), unique(a$max),
                                                 col_aggregation=c("year","gss_code","sex"))
}

deaths_sya_ward <- data.table::rbindlist(past_deaths) %>%
  as.data.frame() %>%
  select(year, gss_code, gss_code_ward, sex, age, deaths)

saveRDS(deaths_sya_ward, "input_data/small_area_model/ward_sya_deaths.rds")

#Single year msoa births
births_sya_msoa <- msoa_births %>%
  left_join(msoa_la_lookup, by="gss_code_msoa") %>%
  group_by(year, gss_code, gss_code_msoa) %>%
  summarise(male = sum(births)*(105/205),
            female = sum(births)*(100/205)) %>%
  tidyr::pivot_longer(c("male","female"), names_to = "sex", values_to = "births") %>%
  mutate(age = 0) %>% 
  select(year, gss_code, gss_code_msoa, sex, age, births) %>%
  as.data.frame() 

saveRDS(births_sya_msoa, "input_data/small_area_model/msoa_sya_births.rds")


#Single year msoa deaths
deaths_sya_msoa <- msoa_deaths %>%
  mutate(min = substr(age_group,1,2),
         min = ifelse(min == "1_", "1",
                      ifelse(min == "5_", "5",
                             min)),
         min = as.numeric(min)) %>%
  mutate(max = case_when(min == 0 ~ 0,
                         min == 1 ~ 4,
                         min == 85 ~ 90,
                         TRUE ~ min + 4)) %>%
  left_join(msoa_la_lookup, by="gss_code_msoa") %>%
  filter(year %in% borough_deaths$year)

past_deaths <- list()
for(i in unique(deaths_sya_msoa$age_group)){
  
  a <- filter(deaths_sya_msoa, age_group ==i)
  past_deaths[[i]] <- distribute_within_age_band(a, borough_deaths, "deaths", "deaths",
                                                 unique(a$min), unique(a$max),
                                                 col_aggregation=c("year","gss_code","sex"))
}

deaths_sya_msoa <- data.table::rbindlist(past_deaths) %>%
  as.data.frame() %>%
  select(year, gss_code, gss_code_msoa, sex, age, deaths)

saveRDS(deaths_sya_msoa, "input_data/small_area_model/msoa_sya_deaths.rds")
