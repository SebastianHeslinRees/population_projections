# Data for the smallarea model
# Reads data from the Q drive

#Packages
devtools::install('model_code/popmodules')
devtools::install('model_code/smallareamodel2')

#Folders
dir.create("input_data/smallarea_model", showWarnings = FALSE, recursive = TRUE)

#### 2022 Wards ####
source('input_data_scripts/smallarea_model/ward_2022_lookups.R')
source('input_data_scripts/smallarea_model/basic_data_WD22CD.R')
source('input_data_scripts/smallarea_model/fert_mort_rates_WD22CD.R')
source('input_data_scripts/smallarea_model/migration_rates_WD22CD.R')
source('input_data_scripts/smallarea_model/household_model_WD22CD.R')
source('input_data_scripts/smallarea_model/ldd_WD22CD.R')
source('input_data_scripts/smallarea_model/2017_shlaa_WD22CD.R')

#### 2013 Wards ####
source('input_data_scripts/smallarea_model/basic_data_WD13CD.R')
source('input_data_scripts/smallarea_model/fert_mort_rates_WD13CD.R')
source('input_data_scripts/smallarea_model/migration_rates_WD13CD.R')
source('input_data_scripts/smallarea_model/household_model_WD13CD.R')
source('input_data_scripts/smallarea_model/ldd_WD13CD.R')
source('input_data_scripts/smallarea_model/2017_shlaa_WD13CD.R')

#other
source("input_data_scripts/smallarea_model/excess_deaths.R")
