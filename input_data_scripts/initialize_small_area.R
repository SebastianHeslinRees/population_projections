#Initialize script for small area model data
#Needs to be done in this order as subsequent steps use data created in precendent steps
#Reads data from Q:

source('ons_small_area_estimates.R')
source('communal_establishment_population.R')
source('births_and_deaths.R')
source('dwellings.R')
source('in_migration_rates')
source('ward_out_migration_rates.R')