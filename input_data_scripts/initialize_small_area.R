#Initialize script for small area model data
#Needs to be done in this order as subsequent steps use data created in precedent steps
#Some scripts read data from Q:/
library(dplyr)
library(data.table)
library(tidyr)

message("small area model data")

#Directory
dir.create("input_data/small_area_model", showWarnings = FALSE)
dir.create("input_data/housing_led_model", showWarnings = FALSE)

#LDD Polygon splits file
file.copy("Q:/Teams/D&PA/Data/LDD/lsoa_polygon_splits.rds",
          "input_data/housing_led_model/lsoa_polygon_splits.rds", overwrite = TRUE)

#source("input_data_scripts/development_data/calc_polygon_splits.R")
source('input_data_scripts/development_data/ldd.R')
source('input_data_scripts/development_data/2017_shlaa.R')
source('input_data_scripts/development_data/shlaa_dev_pandemic_adjustments.R')
source('input_data_scripts/development_data/2019_housing_led_dev_scenarios.R')

source('input_data_scripts/small_area_data/ons_small_area_estimates.R')
source('input_data_scripts/small_area_data/births_and_deaths.R')

source('input_data_scripts/small_area_data/ward_communal_establishment_population.R')
source('input_data_scripts/small_area_data/ward_adults_per_dwelling.R')
source('input_data_scripts/small_area_data/ward_migration_data.R')

source('input_data_scripts/small_area_data/msoa_communal_establishment_population.R')
source('input_data_scripts/small_area_data/msoa_adults_per_dwelling.R')
source('input_data_scripts/small_area_data/msoa_migration_data.R')


#TESTS
rm(list=ls())
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds")

london_wards <- filter(ward_to_district, substr(gss_code,1,3) == "E09") %>%
  filter(gss_code != "E09000001")
london_wards <- sort(c(unique(london_wards$gss_code_ward), "E09000001"))

adults_per_dwelling <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
pop_est <- "input_data/small_area_model/ward_population_estimates.rds"
ce_est <- "input_data/small_area_model/ward_communal_establishment_population.rds"
out_migration_rates <- "input_data/small_area_model/ward_out_migration_rates.rds"
in_migration_characteristics <- "input_data/small_area_model/ward_in_migration_characteristics.rds"
births <- "input_data/small_area_model/ward_births.rds"
deaths <- "input_data/small_area_model/ward_deaths.rds"

test_ward_inputs <- function(data_path, col_aggregation=c("gss_code_ward", "sex", "age")){
  
  data <- readRDS(data_path)
  
  #all wards are present
  assertthat::assert_that(setequal(data$gss_code_ward, london_wards))	
  popmodules::validate_population(data, col_aggregation = col_aggregation,
                                  test_complete = TRUE,
                                  test_unique = TRUE,
                                  check_negative_values = TRUE)
  
}

test_ward_inputs(data_path = adults_per_dwelling, col_aggregation = c("gss_code_ward", "year")) 
test_ward_inputs(pop_est, c("year", "gss_code_ward", "sex", "age"))
test_ward_inputs(ce_est)
test_ward_inputs(out_migration_rates)
test_ward_inputs(in_migration_characteristics)
test_ward_inputs(births, c("year", "gss_code_ward", "age_group"))
test_ward_inputs(deaths, c("year", "gss_code_ward", "sex", "age_group"))

#MSOA
rm(list=ls())
msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds")

london_msoas <- filter(msoa_to_district, substr(gss_code,1,3) == "E09")
london_msoas <- sort(c(unique(london_msoas$gss_code_msoa)))

adults_per_dwelling <- "input_data/small_area_model/msoa_adults_per_dwelling.rds"
pop_est <- "input_data/small_area_model/msoa_population_estimates.rds"
ce_est <- "input_data/small_area_model/msoa_communal_establishment_population.rds"
out_migration_rates <- "input_data/small_area_model/msoa_out_migration_rates.rds"
in_migration_characteristics <- "input_data/small_area_model/msoa_in_migration_characteristics.rds"
births <- "input_data/small_area_model/msoa_births.rds"
deaths <- "input_data/small_area_model/msoa_deaths.rds"

test_msoa_inputs <- function(data_path, col_aggregation=c("gss_code_msoa", "sex", "age")){
  
  data <- readRDS(data_path)
  
  #all msoas are present
  assertthat::assert_that(setequal(data$gss_code_msoa, london_msoas))	
  popmodules::validate_population(data, col_aggregation = col_aggregation,
                                  test_complete = TRUE,
                                  test_unique = TRUE,
                                  check_negative_values = TRUE)
  
}

test_msoa_inputs(data_path = adults_per_dwelling, col_aggregation = c("gss_code_msoa", "year")) 
test_msoa_inputs(pop_est, c("year", "gss_code_msoa", "sex", "age"))
test_msoa_inputs(ce_est)
test_msoa_inputs(out_migration_rates)
test_msoa_inputs(in_migration_characteristics)
test_msoa_inputs(births, c("year", "gss_code_msoa", "age_group"))
test_msoa_inputs(deaths, c("year", "gss_code_msoa", "sex", "age_group"))

rm(list=ls())

message("small area data complete")
