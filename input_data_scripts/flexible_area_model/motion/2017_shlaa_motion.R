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

lsoa_to_motion_zone <- readRDS("input_data/flexible_area_model/lookups/lsoa_to_motion_zone_best_fit.rds")

# london_wards <- lsoa_to_ward_lookup %>%
#   filter(substr(gss_code,1,3) == "E09") %>% 
#   select(gss_code_ward) %>% 
#   unique()

oa_lookup <- readRDS("input_data/flexible_area_model/lookups/oa_to_motion_zone_lookup.rds")

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
  left_join(oa_lookup, by = c("gss_code_oa"="OA11CD", "gss_code")) %>% 
  group_by(motion_zone) %>%
  summarise(units = sum(intense), .groups = 'drop_last') %>%
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

# The borough windfall is distributed evenly among the wards and MSOAs
# i.e. not according to how the other development is distributed with the wards/MSOAs

motion_zone_trend_windfall <- lsoa_to_motion_zone %>% 
  select(motion_zone, gss_code) %>% 
  distinct() %>% 
  group_by(gss_code) %>% 
  mutate(n = n()) %>%
  data.frame() %>% 
  left_join(borough_windfall, by = "gss_code") %>% 
  mutate(units = units/n) %>% 
  select(motion_zone, year, units)

#Join the small sites data to the large sites data
#Add zeros for years 2012-2019 and 2042-2050
#include 2011 in borough file
motion_zone_shlaa <- rbind(motion_zone_large, motion_zone_intense, motion_zone_trend_windfall) %>%
  group_by(year, motion_zone) %>%
  summarise(units = sum(units), .groups = 'drop_last') %>%
  data.frame() %>% 
  tidyr::complete(year = 2012:2050,
                  motion_zone = unique(lsoa_to_motion_zone$motion_zone),
                  fill = list(units = 0))

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
  arrange(motion_zone, year)

#-------------------------------------------------------------------------------

#Save
saveRDS(savills_trajectory, "input_data/flexible_area_model/development_data/savills_trajectory_motion_zone.rds")

rm(list=ls())
