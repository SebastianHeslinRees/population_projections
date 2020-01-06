#Initialize script for small area model data
#Needs to be done in this order as subsequent steps use data created in precendent steps
#Reads data from Q:

source('input_data_scripts/small_area_model/ons_small_area_estimates.R')
source('input_data_scripts/small_area_model/communal_establishment_population.R')
source('input_data_scripts/small_area_model/births_and_deaths.R')
source('input_data_scripts/small_area_model/dwellings.R')
source('input_data_scripts/small_area_model/ward_migration_data.R')