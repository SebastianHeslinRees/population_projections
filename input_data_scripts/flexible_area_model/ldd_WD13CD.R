library(dplyr)

message("Ward 2013 LDD development data")

input_data_dir <- "input_data/flexible_area_model/"
ldd_lsoa <- paste0(input_data_dir,"development_data/ldd_backseries_dwellings_LSOA11CD.rds")

if(file.exists(ldd_lsoa)){
  readRDS(ldd_lsoa)
} else {
  source("input_data_scripts/flexible_area_model/ldd_LSOA11CD.R")
  readRDS(ldd_lsoa)
}

#-------------------------------------------------------------------------------

#lookups
lsoa_to_ward <- readRDS(paste0(input_data_dir,"lookups/lsoa_to_WD13_lookup.rds")) %>%
  select(gss_code_lsoa, gss_code_ward)

ward_to_district <- readRDS(paste0(input_data_dir,"lookups/lsoa_to_WD13_lookup.rds")) %>%
  select(gss_code_ward, ward_name, gss_code) %>%
  distinct()

#-------------------------------------------------------------------------------

#group it into different wards

ward_units <- left_join(lsoa_units, lsoa_to_ward, by="gss_code_lsoa") %>%
  left_join(ward_to_district, by = "gss_code_ward") %>%
  mutate(gss_code_ward = ifelse(gss_code == "E09000001", "E09000001", gss_code_ward)) %>%
  group_by(year, gss_code_ward) %>%
  summarise(units = sum(units), .groups = 'drop_last') %>%
  as.data.frame()

#-------------------------------------------------------------------------------

#save
dir.create(paste0(input_data_dir,"development_data/"), showWarnings = FALSE)
saveRDS(ward_units, paste0(input_data_dir,"development_data/ldd_backseries_dwellings_ward_WD13CD.rds"))


rm(list=ls())

