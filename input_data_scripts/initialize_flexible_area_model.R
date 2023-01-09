# Data for the flexiblearea model
# Reads data from the Q drive

#Folders
dir.create("input_data/flexible_area_model", showWarnings = FALSE, recursive = TRUE)
dir.create("input_data/flexible_area_model/lookups", showWarnings = FALSE)

#HMA
source("input_data_scripts/flexible_area_model/nuts2_hma.R")

#### 2022 Wards ####
source("input_data_scripts/flexible_area_model/WD22/0_ward_2022_lookups.R")
source("input_data_scripts/flexible_area_model/WD22/1_basic_data_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/2a_hh_to_dwellings_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/2b_communal_establishment_pop_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/3_ldd_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/4_2017_shlaa_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/5_fert_mort_rates_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/6_migration_rates_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/7_household_model_WD22CD.R")


#### lonLUTI ####
source('input_data_scripts/flexible_area_model/lonLUTI/1_basic_data_lonLUTI.R')
source('input_data_scripts/flexible_area_model/lonLUTI/2_fert_mort_rates_lonLUTI.R')
source('input_data_scripts/flexible_area_model/lonLUTI/3_migration_rates_lonLUTI.R')
source('input_data_scripts/flexible_area_model/lonLUTI/4_household_model_lonLUTI.R')
source('input_data_scripts/flexible_area_model/lonLUTI/5_ldd_lonLUTI.R')
source('input_data_scripts/flexible_area_model/lonLUTI/6_2017_shlaa_lonLUTI.R')


