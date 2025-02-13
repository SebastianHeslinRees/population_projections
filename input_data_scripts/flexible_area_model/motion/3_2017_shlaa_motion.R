#Process the 2017 SHLAA into a format for use in housing models

# Note on Years
#       SHLAA year 2017/18 is projection year 2018
#       SHLAA is Financial year, projection mid-year

# The SHLAA data is of three types:
# 1) Large sites
#       These are mapped as points and so can be aggregated to any geography.
#       Data are in phases which are equally distributed to the years within the phase

# 2) Small sites other boroughs
#       Use OA-level intensification data which can be aggregated to any geography.
#       When aggregated numbers differ slightly from published SHLAA due to rounding
#       The numbers in the input file ar the original Oct 2017 data - they must be reduced by 2 thirds now

# 3) Small sites traditional/trend windfall
#       For years after 2029 because the published SHLAA is only for 10 years
#       Only available at borough level
#       Was produced for our use by Andrew Russell

# As SHLAA first year is 2018 and LDD only goes to 2016 so data are needed for 2017
#       # Phase 1 developments spread over 3 years instead of 2


library(dplyr)

message("shlaa development motion_zone")

processed_dir <- "input_data/flexible_area_model/development_data/processed/"

large_sites <- readRDS(paste0(processed_dir, "2017_shlaa_large_sites.rds"))
small_intensification <- readRDS(paste0(processed_dir, "2017_shlaa_small_sites_intensification.rds"))
small_remainder_windfall <- readRDS(paste0(processed_dir, "2017_shlaa_small_sites_remainder_windfall.rds"))
small_trend_windfall <- readRDS(paste0(processed_dir, "2017_shlaa_small_sites_trend_windfall.rds"))


oa_lsoa_msoa_lad11_lad21 <- readRDS("input_data/flexible_area_model/lookups/oa_lsoa_msoa_lad11_lad21.rds") %>% 
  filter(substr(LAD11CD,1,3)=="E09")

lsoa_to_motion_zone <- readRDS("input_data/flexible_area_model/lookups/lsoa_to_motion_zone_proportional.rds") %>% 
  filter(gss_code_lsoa %in% oa_lsoa_msoa_lad11_lad21$LSOA11CD)

oa_to_motion_zone <- readRDS("input_data/flexible_area_model/lookups/oa_to_motion_zones_lookup.rds") %>% 
  filter(OA11CD %in% oa_lsoa_msoa_lad11_lad21$OA11CD) %>% 
  filter(!motion_zone %in% c("15","719")) #these are outside london but because the
# motion shape file has been poorly drawn there is a sliver of an OA from within London
# that is assigned to these zone. Makes no difference to population data but for
# dev data causes issues in the model
#-------------------------------------------------------------------------------

#Large sites
motion_zone_large <- large_sites %>%
  mutate(motion_zone = as.character(motion_zone)) %>% 
  group_by(motion_zone, year) %>%
  summarise(units = sum(dev), .groups = 'drop_last') %>%
  as.data.frame() %>%
  tidyr::complete(motion_zone = unique(lsoa_to_motion_zone$motion_zone),
                  year = 2012:2050,
                  fill = list(units = 0))

#Test for NAs
assertthat::assert_that(sum(is.na(motion_zone_large))==0)

#-------------------------------------------------------------------------------

#Small Sites - Intensification
motion_zone_intense <- small_intensification %>%
  right_join(oa_to_motion_zone, by = c("gss_code_oa"="OA11CD")) %>% 
  group_by(motion_zone) %>%
  summarise(units = sum(intense*scaling_factor), .groups = 'drop_last') %>%
  data.frame() %>% 
  mutate(year = 2020)

motion_zone_intense <- motion_zone_intense %>%
  popmodules::project_forward_flat(2029) %>% 
  select(motion_zone, year, units)

#-------------------------------------------------------------------------------

#Small Sites - Windfall
borough_windfall <- rbind(small_trend_windfall, small_remainder_windfall) %>%
  group_by(year, gss_code) %>%
  summarise(units = sum(units), .groups = 'drop_last') %>%
  data.frame()

# The borough windfall is distributed evenly among the zones
# i.e. not according to how the other development is distributed with the wards/MSOAs
lsoa_windfall <- select(oa_lsoa_msoa_lad11_lad21, gss_code_lsoa = LSOA11CD, gss_code = LAD21CD) %>% 
  select(gss_code_lsoa, gss_code) %>% 
  distinct() %>% 
  group_by(gss_code) %>% 
  mutate(n = n()) %>%
  data.frame() %>% 
  left_join(borough_windfall, by = "gss_code") %>% 
  mutate(lsoa_units = units/n) %>% 
  select(gss_code_lsoa, year, units = lsoa_units)

motion_zone_trend_windfall <- lsoa_to_motion_zone %>% 
  left_join(lsoa_windfall, by = "gss_code_lsoa")  %>%
  group_by(motion_zone, year) %>% 
  summarise(units = sum(units*lsoa_share), .groups = 'drop_last') %>% 
  data.frame()
  #left_join(borough_windfall, by = c("gss_code","year")) #%>% 
  # mutate(units = units/n) %>% 
  # select(motion_zone, year, units)

#Join the small sites data to the large sites data
#Add zeros for years 2012-2019 and 2042-2050
#include 2011 in borough file
motion_zone_shlaa <- rbind(motion_zone_large, motion_zone_intense, motion_zone_trend_windfall) %>%
  group_by(year, motion_zone) %>%
  summarise(units = sum(units), .groups = 'drop_last') %>%
  data.frame() %>% 
  tidyr::complete(year = 2012:2050,
                  motion_zone = unique(.$motion_zone),
                  fill = list(units = 0)) %>% 
  filter(motion_zone %in% unique(lsoa_to_motion_zone$motion_zone))

#Save
saveRDS(motion_zone_shlaa, "input_data/flexible_area_model/development_data/shlaa_trajectory_motion_zone.rds")

#-------------------------------------------------------------------------------

#Adjust initial years based on Svaills assumptions

#Savills forecast 41.7k for 2020 & 43k per year for the period 2021-25
#Then distribute the difference between the new trajectory and the SHLAA
#to the later years (2026-41)
savills <- filter(motion_zone_shlaa, year %in% 2020:2025) %>% 
  group_by(year) %>% 
  summarise(shlaa_units = sum(units)) %>% 
  cbind(data.frame(savills_units = c(41700,rep(43000,5)))) %>% 
  mutate(reduction = shlaa_units - savills_units) %>% 
  select(year, reduction)

short_term_savills <- filter(motion_zone_shlaa, year %in% 2020:2025) %>% 
  group_by(year) %>% 
  mutate(share = units/sum(units)) %>% 
  left_join(savills, by = "year") %>% 
  mutate(distributed_reduction = reduction * share,
         units = units - distributed_reduction) %>% 
  select(names(motion_zone_shlaa))

long_term_savills <- filter(motion_zone_shlaa, year %in% 2026:2041) %>% 
  mutate(share = units/sum(units),
         distributed_increase = sum(savills$reduction) * share,
         units = units + distributed_increase) %>% 
  select(names(motion_zone_shlaa))

ldd <- readRDS("input_data/flexible_area_model/development_data/ldd_backseries_dwellings_motion_zone.rds") %>% 
  filter(year < 2020)

savills_trajectory <-  filter(motion_zone_shlaa, !year %in% 2020:2041) %>% 
  rbind(short_term_savills, long_term_savills) %>% 
  filter(year >= 2020) %>% 
  rbind(ldd) %>% 
  arrange(motion_zone, year) %>% 
  filter(motion_zone %in% unique(lsoa_to_motion_zone$motion_zone))


#-------------------------------------------------------------------------------

#Save
saveRDS(savills_trajectory, "input_data/flexible_area_model/development_data/savills_trajectory_motion_zone.rds")

rm(list=ls())

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#London Plan Housing Targets

#The London Plan data includes units for OPDC and LLDC
#We need to distribute these to the relevant boroughs
#I'm using the large sites allocation in the SHLAA
#I'm attaching the SHLAA schemes in the OPDC and LLDC areas to MSOAs
#Then calculating how many units of the totals are in each borough
#Then applying that distribution to the London Plan totals

message("ignore OGRSpatialRef warnings")
library(rgeos)
library(sp)
library(rgdal)
library(tidyr)
library(data.table)

start_dir <- getwd()

#readOGR requires the directory to be changed
shlaa_data_loc <- "Q:/Teams/D&PA/Data/housing_development/shlaa/shlaa_2017/October 2017 (v2)/final_data/"
setwd(paste0(shlaa_data_loc, "large_sites_v2/"))
large_sites_points <- readOGR(dsn = ".", layer = "SHLAA_large_sites_3Oct2017_point", verbose = FALSE)

gss_lookup <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  mutate(x = substr(gss_name, 1, 4))

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
  mutate(units = ifelse(year > 2041, 0, units))  %>% 
  filter(year > 2021)
  arrange(year, gss_code) %>% 
  data.frame()


motion_shlaa <- readRDS("input_data/flexible_area_model/development_data/savills_trajectory_motion_zone.rds")
motion_london_lookup <- fread("Q:/Teams/D&PA/Data/TfL/motion_zones_in_London.csv") %>% 
  data.frame() %>% 
  mutate(motion_zone = as.character(motion_zone))

constrained <- motion_shlaa %>% 
  left_join(motion_london_lookup, by = "motion_zone") %>% 
  filter(!is.na(LondonBoro)) %>% 
  mutate(x = substr(LondonBoro,1,4)) %>% 
  left_join(gss_lookup, by = "x") %>% 
  select(-x, -LondonBoro, -gss_name) %>% 
  group_by(gss_code, year) %>% 
  mutate(sum_units = sum(units)) %>% 
  data.frame() %>% 
  left_join(london_plan_trajectory, by = c("gss_code","year")) %>% 
  mutate(units.z = units.x*(units.y/sum_units)) %>% 
  mutate(units = case_when(year < 2022 ~ units.x,
                           year > 2041 ~ units.x,
                           TRUE ~ units.z)) %>% 
  select(names(motion_shlaa))

motion_london_plan <- constrained %>% 
  rbind(filter(motion_shlaa, !motion_zone %in% constrained$motion_zone)) %>% 
  arrange(motion_zone, year) %>% 
  left_join(motion_shlaa)

#Save
saveRDS(motion_london_plan, "input_data/flexible_area_model/development_data/housing_targets_motion_zone.rds")

rm(list=ls())
