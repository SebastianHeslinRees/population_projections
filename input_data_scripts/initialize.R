#check for packages needed to run the input scripts/models  and install any that are missing
pkg <- c("assertthat", "data.table", "dtplyr", "minpack.lm", "purrr", 
         "rmarkdown", "R.utils", "stats", "stringr", "tibble", "tidyr", "utils", "xlsx")

#needs old version of dplyr - waiting on bug fix in dplyr 1.0.0
devtools::install_version("dplyr", version = "0.8.5", repos = "http://cran.us.r-project.org")
if(packageVersion("dplyr")=="1.0.0"){stop("gla models won't run with dplyr 1.0.0. Install version 0.8.5.")}


for(i in seq(pkg)){
  if(!pkg[i] %in% rownames(installed.packages())){
    install.packages(pkg[i])
  }
}

if(!'loggr' %in% rownames(installed.packages())){
  devtools::install_github("smbache/loggr", force=TRUE)
}

#install the gla models packages
devtools::install('model_code/popmodules')
popmodules::install_gla_models()

#copy lookupss
dir.create("input_data/lookup", showWarnings = FALSE, recursive = TRUE)
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/model_lookups",
                       "input_data/lookup")
file.copy("Q:/Teams/D&PA/Data/code_history_database/district_changes_clean.rds",
          "input_data/lookup/district_changes_clean.rds")

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
source('input_data_scripts/fertility/additional_births_data.R')

#2019 MYE & components
source("input_data_scripts/mye/ons_mye_2019.R")
source("input_data_scripts/mye/northern_ireland_mye_2019.R")
source("input_data_scripts/mye/scotland_mye_2019.R")

source("input_data_scripts/domestic_migration/domestic_migration_2019.R")
source("input_data_scripts/households/household_model_inputs_(2020 geog).R")
source('input_data_scripts/fertility/asfr_2020_geography.R')
source('input_data_scripts/fertility/fertility_rates_2019.R')
source('input_data_scripts/mortality/asmr_2020_geography.R')
source('input_data_scripts/mortality/mortality_rates_2019.R')

#pre calc rates
source("input_data_scripts/domestic_migration/pre-calculate_domestic_rates.R")
source("input_data_scripts/migration/pre-calculate_international_in_flows.R")
source('input_data_scripts/domestic_migration/pre-calulate_domestic_rates_2018_bpo.R')

#Excel templates
dir.create("input_data/excel_templates")
R.utils::copyDirectory("Q:/Teams/D&PA/Demography/Projections/population_models/excel_templates",
                       "input_data/excel_templates")
