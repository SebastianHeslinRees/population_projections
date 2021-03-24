library(dplyr)

#read in the basic shlaa trajectory & ldd backseries
shlaa <- readRDS("input_data/housing_led_model/borough_shlaa_trajectory_2020.rds")
ldd <- readRDS("input_data/housing_led_model/ldd_backseries_dwellings_borough.rds")

#just a look at what the london-level data is like
shlaa_ldn <- group_by(shlaa, year) %>% 
  summarise(units = sum(units))

ldd_ldn <- group_by(ldd, year) %>% 
  summarise(units = sum(units)) %>% 
  data.frame() 

#find the ldd averages for the boroughs for 2012-2019
ldd_avg <- filter(ldd, year > 2011) %>% 
  group_by(gss_code) %>% 
  summarise(units = mean(units)) %>% 
  data.frame() %>% 
  mutate(year = 2020) %>% 
  select(names(ldd)) %>% 
  popmodules::project_forward_flat(2024)

#compare the average data to the shlaa
group_by(ldd_avg, year) %>% 
  summarise(avg = sum(units)) %>% 
  data.frame() %>% 
  left_join(shlaa_ldn, by="year")

#create a new trajectory
#0 units in 2011
#ldd units 2012-2019
#average units 2020-2024
#shlaa units 2025-2050
new_trajectory <- shlaa %>% 
  filter(!year %in% 2020:2024) %>% 
  rbind(ldd_avg) %>% 
  filter(!year %in% 2012:2019) %>% 
  rbind(filter(ldd, year != 2011)) %>% 
  arrange(gss_code, year)

new_london <- new_trajectory %>% 
  group_by(year) %>% 
  summarise(units = sum(units)) %>% 
  data.frame() 

saveRDS(new_trajectory, "input_data/housing_led_model/borough_shlaa_pandemic_adjusted.rds")

#constrain ward data in 2020-2024
ward_shlaa <- readRDS("input_data/small_area_model/ward_shlaa_trajectory_2020.rds")

ward_constrain <- filter(ward_shlaa, year %in% 2020:2024) %>% 
  left_join(readRDS("input_data/lookup/2011_ward_to_district.rds"), by="gss_code_ward") %>% 
  group_by(gss_code, year) %>% 
  mutate(borough_units = sum(units)) %>% 
  data.frame() %>% 
  mutate(scaling = units/borough_units) %>% 
  left_join(new_trajectory, by= c("gss_code","year")) %>% 
  mutate(units = scaling * units.y) %>% 
  select(names(ward_shlaa)) %>% 
  rbind(filter(ward_shlaa, !year %in% 2020:2024)) %>% 
  arrange(gss_code_ward, year)

saveRDS(ward_constrain, "input_data/small_area_model/ward_shlaa_pandemic_adjusted.rds")

rm(list=ls())


# EPC Data
#
# gss_code_to_name <- readRDS("C:/Projects_c/population_projections_c/input_data/lookup/gss_code_to_name.rds") %>% 
#   filter(substr(gss_code, 1, 3) == "E09")
# 
# epc <- data.table::fread("Q:/Teams/D&PA/Data/housing_development/New_Domestic_EPCs.csv",
#                          header = T) %>% data.frame() %>% 
#   filter(Local.Authority %in% gss_code_to_name$gss_name)
# 
# epc_london <- epc %>% 
#   mutate(x = as.numeric(substr(Quarter,1,4))) %>% 
#   mutate(year = ifelse(substr(Quarter,7,7) %in% c("1"), x-1, x)) %>% 
#   group_by(year) %>% 
#   summarise(EPCs = sum(EPCs)) %>% 
#   data.frame() %>% 
#   left_join(ldd_ldn, by="year") %>% 
#   mutate(diff = units - EPCs,
#          prop = (diff / EPCs)*100)

