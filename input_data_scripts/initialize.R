#make sure all the correct CRAN/github packages of the correct versions are installed
renv::restore()

#install the gla models packages
devtools::install('model_code/popmodules', dependencies = FALSE)
popmodules::install_gla_models()

#copy lookups
dir.create("input_data/lookup", showWarnings = FALSE, recursive = TRUE)
dir.create("model_code/popmodules/tests/testthat/test_data/",
           showWarnings = FALSE, recursive = TRUE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/model_lookups",
                       "input_data/lookup")
file.copy("Q:/Teams/D&PA/Data/code_history_database/district_changes_clean.rds",
          "input_data/lookup/district_changes_clean.rds", overwrite = TRUE)
file.copy("input_data/lookup/district_changes_clean.rds",
          "model_code/popmodules/tests/testthat/test_data/district_changes_clean.rds",
          overwrite = TRUE)

#run data creation scripts
source("input_data_scripts/mye/mye_2018.R")
source("input_data_scripts/domestic_migration/domestic_migration_2018.R")
source("input_data_scripts/fertility/modified_fert_rates_2017_base_trend_med.R")
source("input_data_scripts/fertility/npp_fertility_trend.R")
source("input_data_scripts/fertility/snpp_2016_fertility_curves.R")
source("input_data_scripts/fertility/snpp_2018_fertility_curves.R")
source("input_data_scripts/mortality/npp_mortality_trend.R")
source("input_data_scripts/mortality/snpp_2016_mortality_curves.R")
source("input_data_scripts/mortality/snpp_2018_mortality_curves.R")
source("input_data_scripts/constraints/npp_2016_constraint.R")
source("input_data_scripts/constraints/npp_2018_constraint.R")
source("input_data_scripts/households/household_model_inputs.R")
source("input_data_scripts/mye/gla_mye_2018.R")
source("input_data_scripts/mye/gla_mye_2016.R")
source('input_data_scripts/fertility/additional_births_2018-based.R')

#2019 MYE & components
source("input_data_scripts/mye/ons_mye_2019.R")
source("input_data_scripts/mye/northern_ireland_mye_2019.R")
source("input_data_scripts/mye/scotland_mye_2019.R")
source("input_data_scripts/domestic_migration/domestic_migration_2019.R")
source("input_data_scripts/mye/gla_mye_2019.R")

#Other 2019 data
source("input_data_scripts/households/household_model_inputs_(2020 geog).R")
source("input_data_scripts/households/communal_est_popn_2018_model_update.R")
source('input_data_scripts/fertility/asfr_2020_geography.R')
source('input_data_scripts/mortality/asmr_2020_geography.R')
source("input_data_scripts/fertility/fertility_rates_2019.R")
source("input_data_scripts/mortality/mortality_rates_2019.R")

#2018-based projections data
source("input_data_scripts/domestic_migration/pre-calculate_domestic_rates.R")
source("input_data_scripts/domestic_migration/pre-calculate_domestic_rates_gla_mye.R")
source("input_data_scripts/migration/pre-calculate_international_in_flows.R")
source("input_data_scripts/migration/pre-calculate_international_out_flows_rates.R")
source('input_data_scripts/domestic_migration/pre-calulate_domestic_rates_2018_bpo.R')

#2019-based projections data
source("input_data_scripts/scenario_data/covid_scenario_deaths.R")
source("input_data_scripts/scenario_data/covid_scenario_rates.R")
source("input_data_scripts/scenario_data/migration_rates_for_2019_BPOs.R")

#Excel templates
dir.create("input_data/excel_templates", showWarnings = FALSE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/population_models/excel_templates",
                       "input_data/excel_templates", overwrite = TRUE)

message("trend and household model data complete")