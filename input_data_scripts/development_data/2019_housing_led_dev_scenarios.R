library(dplyr)
library(rgeos)
library(sp)
library(rgdal)
library(data.table)
library(tidyr)

message('Development trajectories - 2019-based housing-led projections')

#-------------------------------------------------------------------------------

shlaa <- readRDS("input_data/housing_led_model/borough_shlaa_trajectory_2020.rds")
ldd_backseries <- readRDS("input_data/housing_led_model/ldd_backseries_dwellings_borough.rds")
gss_lookup <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(x = substr(gss_name, 1, 4))

#-------------------------------------------------------------------------------

#London Plan Housing Targets

#The London Plan data includes units for OPDC and LLDC
#We need to distribute these to the relevant boroughs
#I'm using the large sites allocation in the SHLAA
#I'm attaching the SHLAA schemes in the OPDC and LLDC areas to MSOAs
#Then calculating how many units of the totals are in each borough
#Then applying that distribution to the London Plan totals
message("ignore OGRSpatialRef warnings")

start_dir <- getwd()

#readOGR requires the directory to be changed
shlaa_data_loc <- "Q:/Teams/D&PA/Data/housing_development/shlaa/shlaa_2017/October 2017 (v2)/final_data/"
setwd(paste0(shlaa_data_loc, "large_sites_v2/"))
large_sites_points <- readOGR(dsn = ".", layer = "SHLAA_large_sites_3Oct2017_point", verbose = FALSE)

setwd(start_dir)
msoa_polygon_loc <- "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/London/Middle/ESRI"
msoa_polygons <- readOGR(dsn = msoa_polygon_loc, layer = "MSOA_2011_London",
                         verbose = FALSE)

proj4string(large_sites_points) <- proj4string(msoa_polygons)

dev_corp_distributions <- cbind(as.data.frame(large_sites_points), over(large_sites_points, msoa_polygons)) %>%
  filter(borough %in% c("OPDC","London Legacy Development Corporation")) %>% 
  mutate(DC = ifelse(borough == "OPDC", "OPDC", "LLDC")) %>% 
  select(LAD11CD, LAD11NM, DC, Phase1, Phase2, Phase3, Phase4, Phase5) %>% 
  pivot_longer(cols = 4:8, names_to = "phase", values_to = "units") %>% 
  group_by(LAD11CD, LAD11NM, DC) %>% 
  summarise(units = sum(units), .groups = 'drop_last') %>% 
  data.frame() %>% 
  group_by(DC) %>% 
  mutate(dist = units/sum(units)) %>% 
  data.frame() %>% 
  select(gss_code = LAD11CD, DC, dist)

london_plan <- fread("Q:/Teams/D&PA/Data/housing_development/2021 London Plan Target (2020-2029).csv",
                     header = TRUE) %>% 
  data.frame() %>% 
  mutate(units = units/10) %>% 
  mutate(x = substr(borough, 1, 4)) %>% 
  left_join(gss_lookup, by="x")

london_plan_opdc <- london_plan %>% 
  select(borough, units) %>%  
  filter(borough == "OPDC") %>% 
  left_join(dev_corp_distributions, by = c("borough"="DC")) %>% 
  mutate(units = units * dist) %>% 
  select(gss_code, units)

london_plan_lldc <- london_plan %>% 
  select(borough, units) %>%  
  filter(borough == "LLDC") %>% 
  left_join(dev_corp_distributions, by = c("borough"="DC")) %>% 
  mutate(units = units * dist) %>% 
  select(gss_code, units)

london_plan_trajectory <- london_plan %>% 
  filter(!borough %in% c("OPDC","LLDC")) %>% 
  select(gss_code, units) %>% 
  rbind(london_plan_opdc, london_plan_lldc) %>% 
  group_by(gss_code) %>% 
  summarise(units = sum(units), .groups = 'drop_last') %>% 
  mutate(year = 2020) %>% 
  popmodules::project_forward_flat(2050) %>% 
  mutate(units = ifelse(year > 2041, 0, units)) %>% 
  rbind(ldd_backseries) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

rm(start_dir, large_sites_points, msoa_polygon_loc, msoa_polygons,
   dev_corp_distributions, london_plan, london_plan_lldc, london_plan_opdc,
   gss_lookup)

#-------------------------------------------------------------------------------

#Savills forecast 41.7k for 2020 & 43k per year for the period 2021-25
#Then distribute the difference between the new trajectory and the SHLAA
#to the later years (2026-41)

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

excess <- rbind(adjust_2020, adjust_2021_2025) %>% 
  rename(savils = units) %>% 
  left_join(shlaa, by=c("gss_code","year")) %>% 
  rename(shlaa = units) %>% 
  group_by(gss_code) %>% 
  summarise(excess = sum(shlaa)-sum(savils)) %>% 
  data.frame()

adjust_2026_2050 <- filter(shlaa, year >= 2026) %>% 
  group_by(gss_code) %>% 
  mutate(dist = units/sum(units)) %>% 
  data.frame() %>% 
  left_join(excess, by="gss_code") %>% 
  mutate(excess_dist = dist*excess,
         new_units = units+excess_dist) %>% 
  select(year, gss_code, units = new_units)

savills <- rbind(ldd_backseries,
                 adjust_2020,
                 adjust_2021_2025,
                 adjust_2026_2050) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

rm(adjust_2020, adjust_2021_2025, adjust_2026_2041)

#-------------------------------------------------------------------------------

#Low scenario just uses the 8-year LDD average (2012-2019)
mean_ldd <- ldd_backseries %>%
  filter(year %in% 2012:2019) %>% 
  group_by(gss_code) %>% 
  summarise(units = mean(units)) %>% 
  data.frame() %>% 
  mutate(year = 2020) %>% 
  popmodules::project_forward_flat(2042) %>% 
  mutate(units = ifelse(year == 2042, 0, units)) %>% 
  popmodules::project_forward_flat(2050) %>% 
  rbind(ldd_backseries) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

#-------------------------------------------------------------------------------

#Save
saveRDS(savills, "input_data/housing_led_model/borough_2019_based_savills.rds")
saveRDS(mean_ldd, "input_data/housing_led_model/borough_2019_based_low.rds")
saveRDS(london_plan_trajectory, "input_data/housing_led_model/borough_london_plan_trajectory.rds")

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

ward_london_plan <- left_join(ward_shlaa, london_plan_trajectory, by = c("gss_code", "year")) %>% 
  mutate(units = units*distribution) %>% 
  select(-distribution) %>% 
  popmodules::project_forward_flat(2050) %>% 
  mutate(units = ifelse(year > 2041, 0, units)) %>% 
  select(year, gss_code_ward, units) %>% 
  rbind(ldd_ward) %>% 
  arrange(year, gss_code_ward) %>% 
  data.frame()

#Save
saveRDS(ward_savills, "input_data/small_area_model/ward_2019_based_savills.rds")
saveRDS(ward_ldd_mean, "input_data/small_area_model/ward_2019_based_low.rds")
saveRDS(ward_london_plan, "input_data/small_area_model/ward_london_plan_trajectory.rds")

rm(list=ls())
