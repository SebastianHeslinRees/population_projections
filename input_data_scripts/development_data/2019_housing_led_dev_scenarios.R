library(dplyr)

message('Development trajectories - 2019-based housing-led projections')

#-------------------------------------------------------------------------------

shlaa <- readRDS("input_data/housing_led_model/borough_shlaa_trajectory_2020.rds")

#Savills forecast 41.7k for 2020 & 43k for the period 2021-25
backseries <- filter(shlaa, year <= 2019)

adjust_2020 <- filter(shlaa, year == 2020) %>% 
  mutate(distribution = units / sum(units)) %>% 
  mutate(new_units = distribution*41700) %>% 
  select(year, gss_code, new_units) %>% 
  rename(units = new_units)

adjust_2021_2025 <- filter(shlaa, year %in% 2021:2025) %>%
  group_by(year) %>% 
  mutate(distribution = units / sum(units)) %>%
  data.frame() %>% 
  mutate(new_units = distribution*43000) %>% 
  select(year, gss_code, new_units) %>% 
  rename(units = new_units)

savills <- filter(shlaa, year > 2025) %>% 
  rbind(backseries,
        adjust_2020,
        adjust_2021_2025) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

rm(adjust_2020, adjust_2021_2025)

#Low scenario just uses the 8-year LDD average (2012-2019)
mean_ldd <- shlaa %>%
  filter(year %in% 2012:2019) %>% 
  group_by(gss_code) %>% 
  summarise(units = mean(units)) %>% 
  data.frame() %>% 
  mutate(year = 2020) %>% 
  popmodules::project_forward_flat(2042) %>% 
  mutate(units = ifelse(year == 2042, 0, units)) %>% 
  popmodules::project_forward_flat(2050) %>% 
  rbind(backseries) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

#inner is the low scenario, outer is the Savills scenario
inner_ldn <- readRDS("input_data/lookup/inner_and_outer_london.rds") %>% 
  filter(inner == TRUE) %>% 
  .$gss_code

inner <- filter(mean_ldd, gss_code %in% inner_ldn)
outer <- filter(savills, !gss_code %in% inner_ldn)
hybrid <- rbind(inner, outer) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

rm(inner_ldn, inner, outer)

#Save
saveRDS(hybrid, "input_data/housing_led_model/borough_2019_based_hybrid.rds")
saveRDS(savills, "input_data/housing_led_model/borough_2019_based_savills.rds")
saveRDS(mean_ldd, "input_data/housing_led_model/borough_2019_based_low.rds")

#-------------------------------------------------------------------------------

#Wards
ldd_ward <- readRDS("input_data/small_area_model/ldd_backseries_dwellings_ward.rds") %>% 
  filter(year > 2011)

ward_shlaa <- readRDS("input_data/small_area_model/ward_shlaa_trajectory_2020.rds") %>% 
  left_join(readRDS("input_data/lookup/2011_ward_to_district.rds"), by = "gss_code_ward") %>% 
  filter(year %in% 2020:2041) %>% 
  group_by(year, gss_code) %>%
  mutate(distribution = units/sum(units)) %>% 
  select(-units) %>% 
  data.frame()

ward_hybrid <- left_join(ward_shlaa, hybrid, by = c("gss_code", "year")) %>% 
  mutate(units = units*distribution) %>% 
  select(-distribution) %>% 
  popmodules::project_forward_flat(2050) %>% 
  mutate(units = ifelse(year > 2041, 0, units)) %>% 
  select(year, gss_code_ward, units) %>% 
  rbind(ldd_ward) %>% 
  arrange(year, gss_code_ward) %>% 
  data.frame()

ward_ldd_mean <- left_join(ward_shlaa, mean_ldd, by = c("gss_code", "year")) %>% 
  mutate(units = units*distribution) %>% 
  select(-distribution) %>% 
  popmodules::project_forward_flat(2050) %>% 
  mutate(units = ifelse(year > 2041, 0, units)) %>% 
  select(year, gss_code_ward, units) %>% 
  rbind(ldd_ward) %>% 
  arrange(year, gss_code_ward) %>% 
  data.frame()

ward_savills <- left_join(ward_shlaa, savills, by = c("gss_code", "year")) %>% 
  mutate(units = units*distribution) %>% 
  select(-distribution) %>% 
  popmodules::project_forward_flat(2050) %>% 
  mutate(units = ifelse(year > 2041, 0, units)) %>% 
  select(year, gss_code_ward, units) %>% 
  rbind(ldd_ward) %>% 
  arrange(year, gss_code_ward) %>% 
  data.frame()

#Save
saveRDS(ward_hybrid, "input_data/small_area_model/ward_2019_based_hybrid.rds")
saveRDS(ward_savills, "input_data/small_area_model/ward_2019_based_savills.rds")
saveRDS(ward_ldd_mean, "input_data/small_area_model/ward_2019_based_low.rds")

rm(list=ls())
