library(dplyr)

message("motion_zone LDD development data")

input_data_dir <- "input_data/flexible_area_model/"
ldd_lsoa <- paste0(input_data_dir,"development_data/ldd_backseries_dwellings_LSOA11CD.rds")

if(file.exists(ldd_lsoa)){
  lsoa_units <- readRDS(ldd_lsoa)
} else {
  source("input_data_scripts/flexible_area_model/ldd_LSOA11CD.R")
  lsoa_units <- readRDS(ldd_lsoa)
}

#-------------------------------------------------------------------------------

#lookup
oa_lsoa_msoa_lad11_lad21 <- readRDS("input_data/flexible_area_model/lookups/oa_lsoa_msoa_lad11_lad21.rds") %>% 
  filter(substr(LAD11CD,1,3)=="E09")

lsoa_to_motion_zone <- readRDS(paste0(input_data_dir, "lookups/lsoa_to_motion_zone_proportional.rds")) %>% 
  filter(gss_code_lsoa %in% oa_lsoa_msoa_lad11_lad21$LSOA11CD) 
  

#-------------------------------------------------------------------------------

#group it into zones
motion_zone_units <- lsoa_to_motion_zone %>% 
  filter(gss_code_lsoa %in% lsoa_units$gss_code_lsoa) %>% 
  full_join(lsoa_units, by="gss_code_lsoa") %>%
  group_by(year, motion_zone) %>%
  summarise(units = sum(units*lsoa_share), .groups = 'drop_last') %>%
  as.data.frame()

#-------------------------------------------------------------------------------

#save
dir.create(paste0(input_data_dir,"development_data/"), showWarnings = FALSE)
saveRDS(motion_zone_units, paste0(input_data_dir,"development_data/ldd_backseries_dwellings_motion_zone.rds"))

rm(list=ls())

