dir.create("input_data/lookup", showWarnings = FALSE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/model_lookups",
                       "input_data/lookup")
file.copy("Q:/Teams/D&PA/Data/code_history_database/district_changes_clean.rds",
          "input_data/lookup/district_changes_clean.rds")

source("input_data_scripts/mye/mye_2018.R")
source("input_data_scripts/domestic_migration/domestic_migration_2018.R")
source("input_data_scripts/fertility/modified_fert_rates_2017_base_trend_med.R")
source("input_data_scripts/fertility/npp_fertility_trend.R")
source("input_data_scripts/fertility/snpp_2016_fertility_curves.R")
source("input_data_scripts/fertility/snpp_2018_fertility_curves.R")
source("input_data_scripts/migration/modified_int_in_2017_base_trend_med.R")
source("input_data_scripts/migration/modified_int_out_rates_2017_base_trend_med.R")
source("input_data_scripts/mortality/npp_mortality_trend.R")
source("input_data_scripts/mortality/snpp_2016_mortality_curves.R")
source("input_data_scripts/mortality/snpp_2018_mortality_curves.R")
source("input_data_scripts/constraints/npp_2016_constraint.R")
source("input_data_scripts/constraints/npp_2018_constraint.R")
source("input_data_scripts/households/household_model_inputs.R")
source("input_data_scripts/mye/gla_mye_2018.R")
source("input_data_scripts/migration/variant_migration_rates.R")

