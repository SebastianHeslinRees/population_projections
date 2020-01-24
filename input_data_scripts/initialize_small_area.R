#Initialize script for small area model data
#Needs to be done in this order as subsequent steps use data created in precendent steps
#Some scripts read data from Q:/
library(dplyr)
library(data.table)
library(tidyr)
devtools::load_all('model_code/popmodules')

#Lookups
ward_district_lookup <- fread("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/2011_ward_to_district.csv") %>%
  rbind(data.frame(gss_code_ward = "E09000001", ward_name = "City of London", gss_code = "E09000001", stringsAsFactors = FALSE)) %>%
  as.data.frame() %>%
  mutate(ward_name = gsub(",", "", .$ward_name))
saveRDS(ward_district_lookup, "input_data/lookup/2011_ward_to_district.rds")

lsoa_to_ward_lookup <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to ward.rds") %>%
  as.data.frame()
saveRDS(lsoa_to_ward_lookup, "input_data/lookup/2011_lsoa_to_ward.rds")

merged_to_electoral_ward <- data.table::fread("Q:/Teams/D&PA/Census/Lookups/EW/2011 Ward to Merged Ward.csv") %>%
  as.data.frame()
saveRDS(merged_to_electoral_ward, "input_data/lookup/2011_merged_ward_to_electoral_ward.rds")

rm(ward_district_lookup, lsoa_to_ward_lookup, merged_to_electoral_ward)

source('input_data_scripts/small_area_data/ons_small_area_estimates.R')
source('input_data_scripts/small_area_data/communal_establishment_population.R')
source('input_data_scripts/small_area_data/births_and_deaths.R')
source('input_data_scripts/small_area_data/adults_per_dwelling.R')
source('input_data_scripts/small_area_data/ward_migration_data.R')

source('model_code/popmodules/tests/testthat/test-ward_model_inputs.R')
