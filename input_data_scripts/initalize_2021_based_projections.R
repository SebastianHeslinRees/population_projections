library(job)
devtools::install("model_code/popmodules", upgrade = F)
devtools::install("model_code/trendmodel", upgrade = F)
devtools::install("model_code/flexibleareamodel", upgrade = F)

#TREND DATA
source("input_data_scripts/mye/gla_mye_2021.R")
source("input_data_scripts/fertility/fertility_rates_2021.R")
source("input_data_scripts/mortality/mortality_rates_2021.R")
source("input_data_scripts/migration/international_flows_for_2021_projections.R")
source("input_data_scripts/migration/domestic_rates_for_2021_projections.R")

#Example Trend Projection
job({source("config_scripts/trend/2021/trend_2021_5yr.R")}, title = "trend 5yr")
job({source("config_scripts/trend/2021/trend_2021_10yr.R")}, title = "trend 10yr")
job({source("config_scripts/trend/2021/trend_2021_15yr.R")}, title = "trend 15yr")

#FLEXIBLE AREA DATA
source("input_data_scripts/flexible_area_model/WD22/0_ward_2022_lookups.R")
source("input_data_scripts/flexible_area_model/WD22/1_basic_data_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/2_census_inputs_WD22CD.R")

#source("input_data_scripts/flexible_area_model/WD22/3_ldd_WD22CD.R")
#source("input_data_scripts/flexible_area_model/WD22/4_2017_shlaa_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/5_fert_mort_rates_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/6_migration_rates_WD22CD.R")
source("input_data_scripts/flexible_area_model/WD22/7_household_model_WD22CD.R")

#Example Flexible area model Projection
job({source("config_scripts/flexible_area_model/2021_based/SHLAA_NUTS2_5yr.R")}, title = "ward 5yr")
job({source("config_scripts/flexible_area_model/2021_based/SHLAA_NUTS2_10yr.R")}, title = "ward 10yr")
#job({source("config_scripts/flexible_area_model/2021_based/SHLAA_NUTS2_15yr.R")}, title = "ward 15yr")
job({source("config_scripts/flexible_area_model/2021_based/SHLAA_Unconstrained_10yr.R")}, title = "ward 10yr unconstrained")

