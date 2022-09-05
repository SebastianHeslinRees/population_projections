library(dplyr)

message("MSOA LDD development data")

input_data_dir <- "input_data/flexible_area_model/"
ldd_lsoa <- paste0(input_data_dir,"development_data/ldd_backseries_dwellings_LSOA11CD.rds")

if(file.exists(ldd_lsoa)){
  lsoa_units <- readRDS(ldd_lsoa)
} else {
  source("input_data_scripts/flexible_area_model/ldd_LSOA11CD.R")
  lsoa_units <- readRDS(ldd_lsoa)
}

# Lookups

lsoa_to_msoa <- readRDS("input_data/lookup/lsoa_to_msoa.rds") %>%
  select(gss_code_lsoa, gss_code_msoa)

msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds") %>%
  select(gss_code_msoa, gss_code)

#-------------------------------------------------------------------------------

# Group it into MSOAs

msoa_units <- left_join(lsoa_units, lsoa_to_msoa, by="gss_code_lsoa") %>%
  left_join(msoa_to_district, by = "gss_code_msoa") %>%
  group_by(year, gss_code_msoa) %>%
  summarise(units = sum(units), .groups = 'drop_last') %>%
  as.data.frame()

#save
dir.create(paste0(input_data_dir,"development_data/"), showWarnings = FALSE)
saveRDS(msoa_units, paste0(input_data_dir,"development_data/ldd_backseries_dwellings_MSOA11CD.rds"))

rm(list=ls())
