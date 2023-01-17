library(job)

#Trend Projections
job::job({source("config_scripts/trend/2021/trend_2021_5yr.R")}, title = "trend 5yr")
job::job({source("config_scripts/trend/2021/trend_2021_10yr.R")}, title = "trend 10yr")
job::job({source("config_scripts/trend/2021/trend_2021_15yr.R")}, title = "trend 15yr")


#Flexible area model Projection
job::job({source("config_scripts/flexible_area_model/2021_based/Identified_Capacity_NUTS2_10yr.R")}, title = "IC")
job::job({source("config_scripts/flexible_area_model/2021_based/Housing_Targets_NUTS2_10yr.R")}, title = "HT")
job::job({source("config_scripts/flexible_area_model/2021_based/Past_Delivery_NUTS2_10yr.R")}, title = "PD")

