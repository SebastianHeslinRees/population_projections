library(dplyr)
library(rgeos)
library(sp)
library(rgdal)
library(data.table)
library(tidyr)

message('Development trajectories - 2021-based WD22CD')

#-------------------------------------------------------------------------------

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
proj_wd <- getwd()
#readOGR requires the directory to be changed
shlaa_data_loc <- "Q:/Teams/D&PA/Data/housing_development/shlaa/shlaa_2017/October 2017 (v2)/final_data/"
setwd(paste0(shlaa_data_loc, "large_sites_v2/"))
large_sites_points <- readOGR(dsn = ".", layer = "SHLAA_large_sites_3Oct2017_point", verbose = FALSE)

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

setwd(proj_wd)

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
  mutate(units = ifelse(year > 2041, 0, units))  %>% 
  filter(year > 2021) %>% 
  arrange(year, gss_code) %>% 
  data.frame()

rm(start_dir, large_sites_points, msoa_polygon_loc, msoa_polygons,
   dev_corp_distributions, london_plan, london_plan_lldc, london_plan_opdc,
   gss_lookup)

#-------------------------------------------------------------------------------

WD22_to_LAD21 <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds")

shlaa_WD22 <- readRDS("input_data/flexible_area_model/development_data/ward_savills_trajectory_WD22CD.rds")

initial_years <- filter(shlaa_WD22, year <= 2021)

#---

mean_ldd <- shlaa_WD22 %>%
  filter(year %in% 2012:2019) %>% 
  group_by(gss_code_ward) %>% 
  summarise(units = mean(units)) %>% 
  data.frame() %>% 
  mutate(year = 2022) %>% 
  popmodules::project_forward_flat(2042) %>% 
  mutate(units = ifelse(year == 2042, 0, units)) %>% 
  popmodules::project_forward_flat(2050) %>% 
  arrange(year, gss_code_ward) %>% 
  data.frame() %>% 
  bind_rows(initial_years)


#-------------------------------------------------------------------------------

WD22_london_plan <- shlaa_WD22 %>% 
  filter(year > 2021) %>% 
  left_join(WD22_to_LAD21, by = "gss_code_ward") %>% 
  group_by(gss_code, year) %>% 
  mutate(borough_total = sum(units)) %>% 
  data.frame() %>% 
  left_join(london_plan_trajectory, by = c("gss_code", "year")) %>% 
  rename(lp_units = units.y) %>% 
  mutate(sf = lp_units/borough_total,
         units = units.x * sf) %>% 
  select(names(shlaa_WD22)) %>% 
  bind_rows(initial_years)


saveRDS(WD22_london_plan, "input_data/flexible_area_model/development_data/housing_targets_WD22CD.rds")
saveRDS(mean_ldd, "input_data/flexible_area_model/development_data/past_delivery_WD22CD.rds")

rm(list=ls())
