library(dplyr)

message("lonLUTI LDD development data")

input_data_dir <- "input_data/flexible_area_model/"
ldd_lsoa <- paste0(input_data_dir,"development_data/ldd_backseries_dwellings_LSOA11CD.rds")

if(file.exists(ldd_lsoa)){
  lsoa_units <- readRDS(ldd_lsoa)
} else {
  source("input_data_scripts/flexible_area_model/ldd_LSOA11CD.R")
  lsoa_units <- readRDS(ldd_lsoa)
}

#-------------------------------------------------------------------------------

#lookups

lsoa_to_lonLUTI <- readRDS(paste0(input_data_dir, "lookups/lsoa_to_LonLUTI3_lookup.rds")) 

# ward_to_district <- readRDS(paste0(input_data_dir,"lookups/lsoa_to_WD22_lookup_best_fit.rds")) %>%
#   select(gss_code_ward, ward_name, gss_code) %>%
#   distinct()

#-------------------------------------------------------------------------------

#group it into wards

ward_units <- left_join(lsoa_units, lsoa_to_lonLUTI, by=c("gss_code_lsoa","year")) %>%
  #left_join(ward_to_district, by = "gss_code_ward") %>%
  #mutate(gss_code_ward = ifelse(gss_code == "E09000001", "E09000001", gss_code_ward)) %>%
  group_by(year, LonLUTI3) %>%
  summarise(units = sum(units*lsoa_share), .groups = 'drop_last') %>%
  as.data.frame()

#-------------------------------------------------------------------------------

#save
dir.create(paste0(input_data_dir,"development_data/"), showWarnings = FALSE)
saveRDS(ward_units, paste0(input_data_dir,"development_data/ldd_backseries_dwellings_ward_lonLUTI.rds"))

rm(list=ls())

