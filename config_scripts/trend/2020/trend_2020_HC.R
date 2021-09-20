library(popmodules)
library(trendmodel)

#HC
#High International
#Central Domestic

#High Population


projection_name <- "2020_HC_high"
standard_covid_migration <- TRUE

#-------------------------------------------------------------------------------

int_out_flows_rates <- list(
  '2025' = list(path = "input_data/scenario_data/2020_int_out_125k.rds",
                transition = F))

#-------------------------------------------------------------------------------

int_in  <- list(
  '2025' = list(path = "input_data/scenario_data/2020_int_in_125k.rds",
                transition = F))
#-------------------------------------------------------------------------------

domestic_rates <- list(
  '2025' = list(path = "input_data/scenario_data/2020_dom_10yr_avg.rds",
                transition = F))

#-------------------------------------------------------------------------------

source("config_scripts/trend/2020/standard_trend_parameters.R")

config_list <- standard_trend_parameters(list(projection_name = projection_name,
                                              standard_covid_migration = standard_covid_migration,
                                              int_out_flows_rates = int_out_flows_rates,
                                              int_in = int_in,
                                               domestic_rates = domestic_rates))
rm(list = setdiff(ls(), "config_list"))

# Run the model
projection <- run_trend_model(config_list)
