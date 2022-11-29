#TREND DATA
source("input_data_scripts/mye/gla_mye_2021.R")
source("input_data_scripts/fertility/fertility_rates_2021.R")
source("input_data_scripts/mortality/mortality_rates_2021.R")
source("input_data_scripts/migration/international_flows_for_2021_projections.R")

#Example Trend Projection
source("config_scripts/trend/2021/trend_2021_CC.R")

#FLEXIBLE AREA DATA
source("input_data_scripts/flexible_area_model/WD22/0_ward_2022_lookups.R")
source("input_data_scripts/flexible_area_model/WD22/1_basic_data_WD22CD.R")
#source("input_data_scripts/flexible_area_model/WD22/2_ldd_WD22CD.R")
#source("input_data_scripts/flexible_area_model/WD22/3_2017_shlaa_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/4_fert_mort_rates_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/5_migration_rates_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/6_household_model_WD22CD.R")

#Example Flexible area model Projection
source("config_scripts/flexible_area_model/WD22_NUTS2_Upper.R")

