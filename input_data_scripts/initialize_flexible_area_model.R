# Data for the smallarea model
# Reads data from the Q drive

#Packages
#devtools::install('model_code/popmodules')
devtools::install('model_code/flexibleareamodel')

#Folders
dir.create("input_data/flexible_area_model", showWarnings = FALSE, recursive = TRUE)

#### 2022 Wards ####
source('input_data_scripts/flexible_area_model/ward_2022_lookups.R')
source('input_data_scripts/flexible_area_model/basic_data_WD22CD.R')
source('input_data_scripts/flexible_area_model/fert_mort_rates_WD22CD.R')
source('input_data_scripts/flexible_area_model/migration_rates_WD22CD.R')
source('input_data_scripts/flexible_area_model/household_model_WD22CD.R')
source('input_data_scripts/flexible_area_model/ldd_WD22CD.R')
source('input_data_scripts/flexible_area_model/2017_shlaa_WD22CD.R')

#### 2013 Wards ####
source('input_data_scripts/flexible_area_model/basic_data_WD13CD.R')
source('input_data_scripts/flexible_area_model/fert_mort_rates_WD13CD.R')
source('input_data_scripts/flexible_area_model/migration_rates_WD13CD.R')
source('input_data_scripts/flexible_area_model/household_model_WD13CD.R')
source('input_data_scripts/flexible_area_model/ldd_WD13CD.R')
source('input_data_scripts/flexible_area_model/2017_shlaa_WD13CD.R')

#### lonLUTI ####
source('input_data_scripts/flexible_area_model/basic_data_lonLUTI.R')
source('input_data_scripts/flexible_area_model/fert_mort_rates_lonLUTI.R')
source('input_data_scripts/flexible_area_model/migration_rates_lonLUTI.R')
source('input_data_scripts/flexible_area_model/household_model_lonLUTI.R')
source('input_data_scripts/flexible_area_model/ldd_lonLUTI.R')
source('input_data_scripts/flexible_area_model/2017_shlaa_lonLUTI.R')

#other
source("input_data_scripts/flexible_area_model/excess_deaths.R")
source("input_data_scripts/flexible_area_model/nuts2_hma.R")
