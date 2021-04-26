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

message("shlaa development data\nignore OGRSpatialRef warnings")

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

ward_polygon_loc <- "W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2011/ESRI/London"
ward_polygons <- readOGR(dsn = ward_polygon_loc, layer = "London_Ward_CityMerged",
                         verbose = FALSE)

proj4string(large_sites_points) <- proj4string(msoa_polygons)

msoa_join <- cbind(as.data.frame(large_sites_points), over(large_sites_points, msoa_polygons))%>%
        select(lhcss_ref, MSOA11CD, LAD11CD, LAD11NM, 
               Phase1, Phase2, Phase3, Phase4, Phase5)

ward_join <- cbind(as.data.frame(large_sites_points), over(large_sites_points, ward_polygons)) %>%
        select(lhcss_ref, GSS_CODE)

large_input <- left_join(msoa_join, ward_join, by="lhcss_ref") %>%
        select(MSOA11CD, LAD11CD, LAD11NM, GSS_CODE,
               Phase1, Phase2, Phase3, Phase4, Phase5)%>%
        mutate(gss_code_msoa = as.character(MSOA11CD),
               gss_code_borough = as.character(LAD11CD),
               district = as.character(LAD11NM),
               gss_code_ward = as.character(GSS_CODE)) %>%
        select(gss_code_msoa, gss_code_borough, gss_code_ward, district,
               Phase1, Phase2, Phase3, Phase4, Phase5)

assertthat::assert_that(sum(is.na(large_input))==0)

rm(list=setdiff(ls(), c("large_input","shlaa_data_loc")))

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


#group data by ward, msoa and borough 
london_wards <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>%
        filter(substr(gss_code,1,3) == "E09") %>% 
        filter(gss_code != "E09000001") %>% 
        select(gss_code_ward) %>% 
        rbind(data.frame(gss_code_ward = "E09000001")) %>% 
        unique()

london_msoas <- readRDS("input_data/lookup/msoa_to_district.rds") %>%
        filter(substr(gss_code,1,3) == "E09") %>% 
        select(gss_code_msoa) %>% 
        unique()

ward_large <- large_sites %>%
        group_by(gss_code_ward, year) %>%
        summarise(units = sum(dev)) %>%
        as.data.frame() %>%
        tidyr::complete(gss_code_ward = london_wards$gss_code_ward,
                        year = 2012:2050,
                        fill = list(units = 0))

msoa_large <- large_sites %>%
        group_by(gss_code_msoa, year) %>%
        summarise(units = sum(dev)) %>%
        as.data.frame() %>% 
        tidyr::complete(gss_code_msoa = london_msoas$gss_code_msoa,
                        year = 2012:2050,
                        fill = list(units = 0))

borough_large <- large_sites %>%
        group_by(gss_code, year) %>%
        summarise(units = sum(dev)) %>%
        as.data.frame() %>% 
        tidyr::complete(gss_code,
                        year = 2011:2050,
                        fill = list(units = 0))


#Test for NAs
assertthat::assert_that(sum(is.na(ward_large))==0)
assertthat::assert_that(sum(is.na(msoa_large))==0)
assertthat::assert_that(sum(is.na(borough_large))==0)

#Test that amount of development in every dataframe is the same
assertthat::assert_that(sum(msoa_large$units)==total_units)
assertthat::assert_that(sum(borough_large$units)==total_units)
assertthat::assert_that(sum(ward_large$units)==total_units)

rm(large_sites, large_input, total_units)



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
oa_to_msoa <- readRDS("input_data/lookup/oa_to_msoa.rds")
oa_to_ward <- readRDS("input_data/lookup/oa_to_ward.rds")
msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds")

small_intensification <- fread(paste0(shlaa_data_loc,"/Small_Sites_Intensification.csv")) %>%
        as.data.frame() %>% 
        setnames(c("gss_code_oa","intense")) %>%
        left_join(oa_to_msoa, by="gss_code_oa") %>%
        left_join(oa_to_ward, by="gss_code_oa") %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        mutate(intense = intense/3) # This is the post-EiP change

#aggregate to ward
ward_intense <- small_intensification %>%
        group_by(gss_code_ward) %>%
        summarise(units = sum(intense)) %>%
        as.data.frame()

#aggregate to msoa
msoa_intense <- small_intensification %>%
        group_by(gss_code_msoa) %>%
        summarise(units = sum(intense)) %>%
        as.data.frame()

#aggregate to borough
borough_intense <- small_intensification %>%
        group_by(gss_code) %>%
        summarise(units = sum(intense)) %>%
        as.data.frame() 

rm(small_intensification, msoa_to_district, oa_to_msoa, oa_to_ward)

#Windfall sites
#Borough Level Only
#file 1 is windfalls for all boroughs except City of London and Islington
#Standard windfall calc is for 2020-41

#Remainder windfall
small_windfall_1 <- fread(paste0(shlaa_data_loc, "/Small_Sites_Windfall.csv")) %>%
        filter(!Borough %in% c("City of London","Islington","LLDC","OPDC")) %>%
        select(-Borough) %>%
        rename(units = Windfall)

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
        summarise(units = sum(units)) %>%
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


rm(small_trend_windfall_1, small_trend_windfall_2, small_windfall_opdc, small_windfall_lldc)


ward_intense <- ward_intense %>% 
        mutate(year = 2020) %>% 
        popmodules::project_forward_flat(2029) %>% 
        select(gss_code_ward, year, units)

msoa_intense <- msoa_intense %>% 
        mutate(year = 2020) %>% 
        popmodules::project_forward_flat(2029) %>% 
        select(gss_code_msoa, year, units)

borough_intense <- borough_intense %>% 
        mutate(year = 2020) %>% 
        popmodules::project_forward_flat(2029) %>% 
        select(gss_code, year, units)

borough_windfall <- rbind(small_trend_windfall, small_remainder_windfall)

#Join the small sites data to the large sites data
#Add zeros for years 2012-2019 and 2042-2050
#include 2011 in borough file
ward_shlaa <- rbind(ward_large, ward_intense) %>%
        filter(!gss_code_ward %in% london_wards$gss_code_ward) %>% 
        popmodules::aggregate_city_wards("units") %>% 
        rbind(ward_large, ward_intense) %>% 
        filter(gss_code_ward %in% london_wards$gss_code_ward) %>%
        group_by(year, gss_code_ward) %>%
        summarise(units = sum(units)) %>%
        data.frame() %>% 
        tidyr::complete(year = 2012:2050,
                        gss_code_ward = unique(london_wards$gss_code_ward),
                        fill = list(units = 0))

msoa_shlaa <- rbind(msoa_large, msoa_intense) %>%
        group_by(year, gss_code_msoa) %>%
        summarise(units = sum(units)) %>%
        data.frame()

borough_shlaa <- rbind(borough_large, borough_intense, borough_windfall) %>%
        group_by(year, gss_code) %>%
        summarise(units = sum(units)) %>%
        data.frame()

#Save
saveRDS(ward_shlaa, "input_data/small_area_model/ward_shlaa_trajectory_2020.rds")
saveRDS(msoa_shlaa, "input_data/small_area_model/msoa_shlaa_trajectory_2020.rds")
saveRDS(borough_shlaa, "input_data/housing_led_model/borough_shlaa_trajectory_2020.rds")

shlaa_breakdown <- left_join(borough_large, borough_intense, by = c('gss_code','year')) %>% 
        left_join(borough_windfall, by = c('gss_code','year')) %>% 
        group_by(year) %>% 
        summarise(large_sites = sum(units.x),
                  intensification = sum(units.y),
                  windfall = sum(units)) %>% 
        data.frame() %>% 
        tidyr::replace_na(list(large_sites = 0,
                               intensification = 0,
                               windfall = 0)) %>% 
        mutate(total = large_sites + intensification + windfall) %>% 
        filter(year %in% 2017:2041)

clipr::write_clip(shlaa_breakdown)

rm(list=ls())

