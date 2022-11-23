#Make sure all the correct CRAN/github packages of the correct versions are installed
renv::restore()

#install the gla models packages
devtools::document('model_code/popmodules')
devtools::install('model_code/popmodules', dependencies = FALSE)
popmodules::install_gla_models()

#python
reticulate::py_install(c("pandas","openpyxl"))

#-------------------------------------------------------------------------------

#Create directories
dir.create("input_data/lookup", showWarnings = FALSE, recursive = TRUE)
dir.create("model_code/popmodules/tests/testthat/test_data/", showWarnings = FALSE, recursive = TRUE)

#-------------------------------------------------------------------------------

#Copy lookups
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/model_lookups",
                       "input_data/lookup")

assertthat::assert_that(all(
  file.copy("Q:/Teams/D&PA/Data/code_history_database/district_changes_clean.rds",
            "input_data/lookup/district_changes_clean.rds", overwrite = TRUE),
  
  file.copy("input_data/lookup/district_changes_clean.rds",
            "model_code/popmodules/tests/testthat/test_data/district_changes_clean.rds",
            overwrite = TRUE),
  
  file.copy("input_data/lookup/district_to_region.rds",
            "model_code/popmodules/tests/testthat/test_data/district_to_region.rds",
            overwrite = TRUE),
  
  file.copy("input_data/lookup/inner_and_outer_london.rds",
            "model_code/popmodules/tests/testthat/test_data/inner_and_outer_london.rds",
            overwrite = TRUE)),
  
  msg="failed to copy lookups files from Q:")

#-------------------------------------------------------------------------------

#### Trend model data

#NPP & SNPP data
source("input_data_scripts/fertility/npp_fertility_trend.R")
source("input_data_scripts/fertility/snpp_2018_fertility_curves.R")
source("input_data_scripts/mortality/npp_mortality_trend.R")
source("input_data_scripts/mortality/snpp_2018_mortality_curves.R")
source("input_data_scripts/constraints/npp_2018_constraint.R")

#2020 MYE & components
source("input_data_scripts/mye/ons_mye_2020.R")
source("input_data_scripts/mye/gla_mye_2020.R")
source("input_data_scripts/domestic_migration/domestic_migration_2020.R")

#Other 2020 data (2021 geography)
source("input_data_scripts/households/household_model_inputs.R")
source("input_data_scripts/households/household_model_inputs_(2021 geog).R")
source("input_data_scripts/fertility/asfr_2021_geography.R")
source("input_data_scripts/mortality/asmr_2021_geography.R")
source("input_data_scripts/fertility/fertility_rates_2020_(2021 geog).R")
source("input_data_scripts/mortality/mortality_rates_2020_(2021 geog).R")
#source("input_data_scripts/fertility/additional_births_2019-based.R")
source("input_data_scripts/scenario_data/covid_scenario_deaths.R")
source("input_data_scripts/mortality/external_deaths_2021.R")

#2020-based migration rates and flows
source("input_data_scripts/scenario_data/domestic_rates_for_2020_projections.R")
source("input_data_scripts/scenario_data/international_flows_for_2020_projections.R")

#-------------------------------------------------------------------------------

#Excel templates
dir.create("input_data/excel_templates", showWarnings = FALSE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/population_models/excel_templates",
                       "input_data/excel_templates", overwrite = TRUE)

#-------------------------------------------------------------------------------

#### Dev Data

#Initialize script for LDD, SHLAA and dev scenario data
#Some scripts read data from Q:/
# message("development data")
# 
# #LDD Polygon splits file
assertthat::assert_that(
  file.copy("Q:/Teams/D&PA/Data/LDD/lsoa_polygon_splits.rds",
            "input_data/housing_led_model/lsoa_polygon_splits.rds", overwrite = TRUE),
  msg="failed to copy ldd files from Q:")

# #source("input_data_scripts/development_data/calc_polygon_splits.R") #20min runtime
# 
# #Dev data
# source('input_data_scripts/development_data/ldd.R')
# source('input_data_scripts/development_data/2017_shlaa.R')
# source('input_data_scripts/development_data/shlaa_dev_pandemic_adjustments.R')
# source('input_data_scripts/development_data/2020_housing_led_dev_scenarios.R')

#-------------------------------------------------------------------------------

message("initialize complete")
