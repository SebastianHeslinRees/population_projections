library(job)

#Example Trend Projection
job({source("config_scripts/trend/2021/trend_2021_5yr.R")}, title = "trend 5yr")
job({source("config_scripts/trend/2021/trend_2021_10yr.R")}, title = "trend 10yr")
job({source("config_scripts/trend/2021/trend_2021_15yr.R")}, title = "trend 15yr")


#Example Flexible area model Projection
job({source("config_scripts/flexible_area_model/2021_based/SHLAA_NUTS2_5yr.R")}, title = "ward 5yr")
job({source("config_scripts/flexible_area_model/2021_based/SHLAA_NUTS2_10yr.R")}, title = "ward 10yr")
#job({source("config_scripts/flexible_area_model/2021_based/SHLAA_NUTS2_15yr.R")}, title = "ward 15yr")
job({source("config_scripts/flexible_area_model/2021_based/SHLAA_Unconstrained_10yr.R")}, title = "ward 10yr unconstrained")

