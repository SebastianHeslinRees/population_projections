#This script installs all package and preps the input data ready for model runs
#Run it line by line rather than sourcing it to ensure each element completes properly

#Make sure all the correct CRAN/github packages of the correct versions are installed
renv::restore()

#run renv again as a check
renv::restore()

#install the gla models packages
devtools::document('model_code/popmodules')
devtools::install('model_code/popmodules', dependencies = FALSE)
popmodules::install_gla_models()

#python packages
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

#### Trend model data - LAD21CD

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

#2021 MYE & components
source("input_data_scripts/mye/gla_mye_2021.R")

#Other data
source("input_data_scripts/households/household_model_inputs.R")
source("input_data_scripts/households/household_model_inputs_(2021 geog).R")
source("input_data_scripts/households/communal_est_popn_2021_census.R")
source("input_data_scripts/fertility/asfr_2021_geography.R")
source("input_data_scripts/mortality/asmr_2021_geography.R")
source("input_data_scripts/fertility/2022_births.R")

#Rates 2020 projections
# source("input_data_scripts/fertility/fertility_rates_2020_(2021 geog).R")
# source("input_data_scripts/mortality/mortality_rates_2020_(2021 geog).R")
# source("input_data_scripts/scenario_data/covid_scenario_deaths.R")
# source("input_data_scripts/mortality/external_deaths_2021.R")
# source("input_data_scripts/scenario_data/domestic_rates_for_2020_projections.R")
# source("input_data_scripts/scenario_data/international_flows_for_2020_projections.R")

#Rates 2021 projections
source("input_data_scripts/fertility/fertility_rates_2021_B.R")
source("input_data_scripts/mortality/mortality_rates_2021.R")
source("input_data_scripts/migration/international_flows_for_2021_projections.R")
source("input_data_scripts/migration/domestic_rates_for_2021_projections.R")

#-------------------------------------------------------------------------------

#Excel templates
dir.create("input_data/excel_templates", showWarnings = FALSE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/population_models/excel_templates",
                       "input_data/excel_templates", overwrite = TRUE)

#-------------------------------------------------------------------------------

#### Dev Data

#LDD Polygon splits file
dir.create("input_data/housing_led_model/")
assertthat::assert_that(
  file.copy("Q:/Teams/D&PA/Data/LDD/lsoa_polygon_splits.rds",
            "input_data/housing_led_model/lsoa_polygon_splits.rds", overwrite = TRUE),
  msg="failed to copy ldd files from Q:")

#-------------------------------------------------------------------------------

message("initialize complete")
