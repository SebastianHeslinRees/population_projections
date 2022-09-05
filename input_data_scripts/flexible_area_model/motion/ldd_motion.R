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
lsoa_to_motion_zone <- readRDS(paste0(input_data_dir, "lookups/lsoa_to_motion_zone_proportional.rds")) 

#-------------------------------------------------------------------------------

#group it into wards

motion_zone_units <- left_join(lsoa_units, lsoa_to_motion_zone, by=c("gss_code_lsoa","year")) %>%
  group_by(year, motion_zone) %>%
  summarise(units = sum(units*lsoa_share), .groups = 'drop_last') %>%
  as.data.frame()

#-------------------------------------------------------------------------------

#save
dir.create(paste0(input_data_dir,"development_data/"), showWarnings = FALSE)
saveRDS(motion_zone_units, paste0(input_data_dir,"development_data/ldd_backseries_dwellings_motion_zone.rds"))

rm(list=ls())

