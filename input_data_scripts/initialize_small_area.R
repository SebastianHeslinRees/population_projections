#Initialize script for small area model data
#Needs to be done in this order as subsequent steps use data created in precendent steps
#Some scripts read data from Q:/
library(dplyr)
library(data.table)
library(tidyr)

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

#LDD Polygon splits file
polygon_splits <- data.table::fread("Q:/Teams/D&PA/Data/LDD/lsoa_polygon_splits_16-01-20.csv")
write.csv(polygon_splits, "input_data/housing_led_model/lsoa_polygon_splits_16-01-20.csv", row.names = FALSE)
rm(polygon_splits)

source('input_data_scripts/ldd/ldd.R')
source('input_data_scripts/small_area_data/ons_small_area_estimates.R')
source('input_data_scripts/small_area_data/communal_establishment_population.R')
source('input_data_scripts/small_area_data/births_and_deaths.R')
source('input_data_scripts/small_area_data/adults_per_dwelling.R')
source('input_data_scripts/small_area_data/ward_migration_data.R')

#TESTS
london_wards <- filter(ward_district_lookup, substr(gss_code,1,3) == "E09") %>%
  filter(gss_code != "E09000001")
london_wards <- sort(c(unique(london_wards$gss_code_ward), "E09000001"))

adults_per_dwelling <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
pop_est <- "input_data/small_area_model/ward_population_estimates_2010_2018.rds"
ce_est <- "input_data/small_area_model/ward_communal_establishment_population.rds"
out_migration_rates <- "input_data/small_area_model/ward_out_migration_rates.rds"
in_migration_characteristics <- "input_data/small_area_model/ward_in_migration_characteristics.rds"
births <- "input_data/small_area_model/ward_births_2001_2018.rds"
deaths <- "input_data/small_area_model/ward_deaths_2001_2018.rds"

test_ward_inputs <- function(data_path, col_aggregation=c("gss_code_ward", "sex", "age")){
  
  data <- readRDS(data_path)
  
  #all wards are present
  assertthat::assert_that(setequal(data$gss_code_ward, london_wards))
  popmodules::validate_population(data, col_aggregation = col_aggregation)
}

test_ward_inputs(data_path = adults_per_dwelling, col_aggregation = c("gss_code_ward", "year")) 
test_ward_inputs(pop_est, c("year", "gss_code_ward", "sex", "age"))
test_ward_inputs(ce_est)
test_ward_inputs(out_migration_rates)
test_ward_inputs(in_migration_characteristics)
test_ward_inputs(births, c("year", "gss_code_ward", "age_group"))
test_ward_inputs(deaths, c("year", "gss_code_ward", "sex", "age_group"))


