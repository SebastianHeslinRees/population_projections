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

library(rgeos)
library(sp)
library(rgdal)
library(dplyr)
library(data.table)
library(tidyr)

message("raw shlaa development data")

#-------------------------------------------------------------------------------

#Lookups

msoa_to_district_lookup <-  readRDS("input_data/lookup/msoa_to_district.rds")
oa_to_msoa_lookup <- readRDS("input_data/lookup/oa_to_msoa.rds")
oa_to_ward13_lookup <- readRDS("input_data/flexible_area_model/lookups/oa_to_WD13_lookup.rds") %>%
  select(gss_code_oa, WD13CD = gss_code_ward)
oa_to_ward22_lookup <- readRDS("input_data/flexible_area_model/lookups/oa_to_WD22_lookup_best_fit.rds") %>%
  select(gss_code_oa, WD22CD = gss_code_ward)
ward22_name_lookup <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds")
#-------------------------------------------------------------------------------

####LARGE SITES####

#Join the large sites point data to ward and MSOA polygons
start_dir <- getwd()

#readOGR requires the directory to be changed
shlaa_data_loc <- "Q:/Teams/D&PA/Data/housing_development/shlaa/shlaa_2017/October 2017 (v2)/final_data/"
setwd(paste0(shlaa_data_loc, "large_sites_v2/"))
large_sites_points <- readOGR(dsn = ".", layer = "SHLAA_large_sites_3Oct2017_point",
                              verbose = FALSE)

setwd(start_dir)

msoa_polygon_loc <- "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/London/Middle/ESRI"
msoa_polygons <- readOGR(dsn = msoa_polygon_loc, layer = "MSOA_2011_London",
                         verbose = FALSE)

ward_13_polygon_loc <- "W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2011/ESRI/London"
ward_13_polygons <- readOGR(dsn = ward_13_polygon_loc, layer = "London_Ward_CityMerged",
                         verbose = FALSE)

ward_22_polygon_loc <- "W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2022/ESRI/London"
ward_22_polygons <- readOGR(dsn = ward_22_polygon_loc, layer = "London_Ward_2022_draft",
                         verbose = FALSE)

proj4string(large_sites_points) <- proj4string(msoa_polygons)
proj4string(ward_13_polygons) <- proj4string(msoa_polygons)
proj4string(ward_22_polygons) <- proj4string(msoa_polygons)

msoa_join <- cbind(as.data.frame(large_sites_points), over(large_sites_points, msoa_polygons))%>%
  select(lhcss_ref, MSOA11CD, LAD11CD, LAD11NM,
         Phase1, Phase2, Phase3, Phase4, Phase5)

ward_13_join <- cbind(as.data.frame(large_sites_points), over(large_sites_points, ward_13_polygons)) %>%
  select(lhcss_ref, WD13CD = GSS_CODE)

ward_22_join <- cbind(as.data.frame(large_sites_points), over(large_sites_points, ward_22_polygons)) %>%
  select(lhcss_ref, ward_name, gss_code = la_gsscode) %>% 
  mutate(ward_name = stringr::str_replace_all(ward_name, "\\.", "")) %>% 
  mutate(ward_name = stringr::str_replace_all(ward_name, "Shirly South", "Shirley South")) %>%
  left_join(ward22_name_lookup, by = c("ward_name","gss_code")) %>% 
  select(-la_name) %>% 
  mutate(gss_code_ward = ifelse(gss_code == "E09000001", "E09000001", gss_code_ward)) %>% 
  select(lhcss_ref, WD22CD = gss_code_ward)
         
large_input <- left_join(msoa_join, ward_13_join, by="lhcss_ref") %>%
  left_join(ward_22_join, by="lhcss_ref") %>% 
  select(MSOA11CD, LAD11CD, LAD11NM, WD13CD, WD22CD,
         Phase1, Phase2, Phase3, Phase4, Phase5)%>%
  mutate(gss_code_msoa = as.character(MSOA11CD),
         gss_code_borough = as.character(LAD11CD),
         district = as.character(LAD11NM)) %>%
  select(gss_code_msoa, gss_code_borough, WD13CD, WD22CD, district,
         Phase1, Phase2, Phase3, Phase4, Phase5)


assertthat::assert_that(sum(is.na(large_input))==0)

rm(list=setdiff(ls(), c("large_input","shlaa_data_loc", ls()[stringr::str_detect(ls(), "lookup")])))

####CREATE LARGE SITES TOTALS####
total_units <- large_input %>%
  select(Phase1, Phase2, Phase3, Phase4, Phase5) %>%
  sum()

large_sites <- large_input %>%
  mutate(Y2017 = Phase1 /3,
         Y2018 = Phase1 /3,
         Y2019 = Phase1 /3,
         
         Y2020 = Phase2 /5,
         Y2021 = Phase2 /5,
         Y2022 = Phase2 /5,
         Y2023 = Phase2 /5,
         Y2024 = Phase2 /5,
         
         Y2025 = Phase3 /5,
         Y2026 = Phase3 /5,
         Y2027 = Phase3 /5,
         Y2028 = Phase3 /5,
         Y2029 = Phase3 /5,
         
         Y2030 = Phase4 /5,
         Y2031 = Phase4 /5,
         Y2032 = Phase4 /5,
         Y2033 = Phase4 /5,
         Y2034 = Phase4 /5,
         
         Y2035 = Phase5 /7,
         Y2036 = Phase5 /7,
         Y2037 = Phase5 /7,
         Y2038 = Phase5 /7,
         Y2039 = Phase5 /7,
         Y2040 = Phase5 /7,
         Y2041 = Phase5 /7) %>%
  
  select(-Phase1, -Phase2, -Phase3, -Phase4, -Phase5) %>%
  pivot_longer(names_to = "year", values_to = "dev", cols = starts_with("Y")) %>%
  mutate(year = as.numeric(substr(year,2,5))) %>%
  rename(gss_code = gss_code_borough) %>% 
  data.frame()

#-------------------------------------------------------------------------------

####Small Sites####

#As noted above the small sites are of 2 types:
#       - OA level
#       - Borough/Development Corporation level

# The SHLAA assumed an intensification which is provided at OA level
# Following EiP this has been reduced to 1/3 of the level in the input file

# The other element to small sites is the non-specific windfall development
# This is a borough-wide number and can't be distributed to ward or msoa
# Its therefore only added into the borough trajectory
# This data has provided by planning in 2 files: 1 for TH, Hackney, Islington and
# one for everything else

# All small sites data is one number which is the same in each year of the projection
# Small sites data care applied from the SHLAA start year of 2020


#OA level data

small_intensification <- fread(paste0(shlaa_data_loc,"/Small_Sites_Intensification.csv")) %>%
  as.data.frame() %>% 
  setnames(c("gss_code_oa","intense")) %>%
  left_join(oa_to_msoa_lookup, by="gss_code_oa") %>%
  left_join(oa_to_ward13_lookup, by="gss_code_oa") %>%
  left_join(oa_to_ward22_lookup, by ="gss_code_oa") %>% 
  left_join(msoa_to_district_lookup, by="gss_code_msoa") %>%
  mutate(WD13CD = ifelse(gss_code == "E09000001", "E09000001", WD13CD)) %>% 
  mutate(intense = intense/3) # This is the post-EiP change

#Windfall sites
#Borough Level Only
#file 1 is windfalls for all boroughs except City of London and Islington
#Standard windfall calc is for 2020-41

#Remainder windfall
small_windfall_1 <- fread(paste0(shlaa_data_loc, "/Small_Sites_Windfall.csv")) %>%
  data.frame() %>% 
  filter(!Borough %in% c("City of London","Islington","LLDC","OPDC")) %>%
  select(-Borough) %>%
  rename(units = Windfall) %>% 
  data.frame()

small_windfall_opdc <- data.frame(gss_code=c("E09000012","E09000025","E09000030"),
                                  units=c(0,68,2),
                                  stringsAsFactors = F)

small_windfall_lldc <- data.frame(gss_code="E09000009",
                                  units=5,
                                  stringsAsFactors = F)

small_remainder_windfall <- rbind(small_windfall_1,
                                  small_windfall_opdc,
                                  small_windfall_lldc) %>%
  group_by(gss_code) %>%
  summarise(units = sum(units), .groups = 'drop_last') %>%
  as.data.frame() %>% 
  mutate(year = 2020) %>% 
  popmodules::project_forward_flat(2029) %>% 
  select(gss_code, year, units)

#Trend windfall
small_trend_windfall_1 <- fread(paste0(shlaa_data_loc, "/Small_Sites_Other_Windfall.csv")) %>%
  data.frame() %>% 
  filter(Borough %in% c("City of London","Islington")) %>%
  select(-Borough) %>%
  rename(units = Windfall) %>% 
  mutate(year = 2020) %>% 
  popmodules::project_forward_flat(2029)

small_trend_windfall_2 <- fread(paste0(shlaa_data_loc, "/Small_Sites_Other_Windfall.csv")) %>%
  data.frame() %>% 
  filter(!Borough %in% c("OPDC","LLDC")) %>%
  select(-Borough) %>%
  rename(units = Windfall) %>% 
  mutate(year = 2017) %>% 
  popmodules::project_forward_flat(2041) %>% 
  filter(!year %in% 2020:2029)

small_trend_windfall <- rbind(small_trend_windfall_1,
                              small_trend_windfall_2) %>%
  as.data.frame() %>% 
  select(gss_code, year, units)

#-------------------------------------------------------------------------------

# save

output_dir <- "input_data/flexible_area_model/development_data/processed/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(large_sites, paste0(output_dir, "2017_shlaa_large_sites.rds"))
saveRDS(small_intensification, paste0(output_dir, "2017_shlaa_small_sites_intensification.rds"))
saveRDS(small_remainder_windfall, paste0(output_dir, "2017_shlaa_small_sites_remainder_windfall.rds"))
saveRDS(small_trend_windfall, paste0(output_dir, "2017_shlaa_small_sites_trend_windfall.rds"))

rm(list = ls())
