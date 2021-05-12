#Make sure all the correct CRAN/github packages of the correct versions are installed
renv::restore()

#Install the gla models packages
devtools::install('model_code/popmodules', dependencies = FALSE)
popmodules::install_gla_models()

#-------------------------------------------------------------------------------

#Create directories
dir.create("input_data/lookup", showWarnings = FALSE, recursive = TRUE)
dir.create("input_data/small_area_model", showWarnings = FALSE)
dir.create("input_data/housing_led_model", showWarnings = FALSE)
dir.create("model_code/popmodules/tests/testthat/test_data/",
           showWarnings = FALSE, recursive = TRUE)

#-------------------------------------------------------------------------------

#Copy lookups

R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/model_lookups",
                       "input_data/lookup")

file.copy("Q:/Teams/D&PA/Data/code_history_database/district_changes_clean.rds",
          "input_data/lookup/district_changes_clean.rds", overwrite = TRUE)

file.copy("input_data/lookup/district_changes_clean.rds",
          "model_code/popmodules/tests/testthat/test_data/district_changes_clean.rds",
          overwrite = TRUE)

#-------------------------------------------------------------------------------

#### Trend model data

#NPP & SNPP data
source("input_data_scripts/fertility/npp_fertility_trend.R")
source("input_data_scripts/fertility/snpp_2018_fertility_curves.R")
source("input_data_scripts/mortality/npp_mortality_trend.R")
source("input_data_scripts/mortality/snpp_2018_mortality_curves.R")
source("input_data_scripts/constraints/npp_2018_constraint.R")

#2018 MYE & components
source("input_data_scripts/mye/mye_2018.R")
source("input_data_scripts/mye/gla_mye_2018.R")

#2019 MYE & components
source("input_data_scripts/mye/ons_mye_2019.R")
source("input_data_scripts/mye/northern_ireland_mye_2019.R")
source("input_data_scripts/mye/scotland_mye_2019.R")
source("input_data_scripts/domestic_migration/domestic_migration_2019.R")
source("input_data_scripts/mye/gla_mye_2019.R")

#Other 2019 data
source("input_data_scripts/households/household_model_inputs.R")
source("input_data_scripts/households/household_model_inputs_(2020 geog).R")
source("input_data_scripts/households/communal_est_popn_2018_model_update.R")
source('input_data_scripts/fertility/asfr_2020_geography.R')
source('input_data_scripts/mortality/asmr_2020_geography.R')
source("input_data_scripts/fertility/fertility_rates_2019.R")
source("input_data_scripts/mortality/mortality_rates_2019.R")
source('input_data_scripts/fertility/additional_births_2019-based.R')

#2019-based projection rates
source("input_data_scripts/scenario_data/covid_scenario_deaths.R")
source("input_data_scripts/scenario_data/covid_scenario_rates.R")
source("input_data_scripts/scenario_data/migration_rates_for_2019_BPOs.R")
source("input_data_scripts/domestic_migration/pre-calculate_domestic_rates_gla_mye.R")

#-------------------------------------------------------------------------------

#Excel templates
dir.create("input_data/excel_templates", showWarnings = FALSE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/population_models/excel_templates",
                       "input_data/excel_templates", overwrite = TRUE)

#-------------------------------------------------------------------------------

#### Dev Data

#Initialize script for LDD, SHLAA and dev scenario data
#Some scripts read data from Q:/
message("development data")

#LDD Polygon splits file
file.copy("Q:/Teams/D&PA/Data/LDD/lsoa_polygon_splits.rds",
          "input_data/housing_led_model/lsoa_polygon_splits.rds", overwrite = TRUE)
#source("input_data_scripts/development_data/calc_polygon_splits.R") #20min runtime

#Dev data
source('input_data_scripts/development_data/ldd.R')
source('input_data_scripts/development_data/2017_shlaa.R')
source('input_data_scripts/development_data/shlaa_dev_pandemic_adjustments.R')
source('input_data_scripts/development_data/2019_housing_led_dev_scenarios.R')

#-------------------------------------------------------------------------------

#### Small area data

#Needs to be done in this order as subsequent steps use data created in precedent steps
#Some scripts read data from Q:/
message("small area model data")

source('input_data_scripts/small_area_data/ons_small_area_estimates.R')
source('input_data_scripts/small_area_data/births_and_deaths.R')

source('input_data_scripts/small_area_data/ward_communal_establishment_population.R')
source('input_data_scripts/small_area_data/ward_adults_per_dwelling.R')
source('input_data_scripts/small_area_data/ward_migration_data.R')

source('input_data_scripts/small_area_data/msoa_communal_establishment_population.R')
source('input_data_scripts/small_area_data/msoa_adults_per_dwelling.R')
source('input_data_scripts/small_area_data/msoa_migration_data.R')

#-------------------------------------------------------------------------------

source('input_data_scripts/small_area_data/test_small_area_inputs.R')
message("small area data complete")
message("initialize complete")
