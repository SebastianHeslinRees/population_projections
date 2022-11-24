# library(popmodules)
# library(trendmodel)

#CC
#Central International
#Central Domestic

#Central Upper
 
projection_name <- "2021_TEST"
standard_covid_migration <- FALSE

#-------------------------------------------------------------------------------

int_out_flows_rates <- list(
  '2022' = list(path = "input_data/scenario_data/2021_int_out_10yr_avg.rds",
                transition = F))

#-------------------------------------------------------------------------------

int_in  <- list(
  '2022' = list(path = "input_data/scenario_data/2021_int_in_10yr_avg.rds",
                transition = F))
#-------------------------------------------------------------------------------

domestic_rates <- list(
  '2022' = list(path = "input_data/scenario_data/2020_dom_10yr_avg.rds",
                transition = F)) # TODO

#-------------------------------------------------------------------------------

source("config_scripts/trend/2021/standard_trend_parameters.R")

config_list <- standard_trend_parameters(list(projection_name = projection_name,
                                              standard_covid_migration = standard_covid_migration,
                                              int_out_flows_rates = int_out_flows_rates,
                                              int_in = int_in,
                                              domestic_rates = domestic_rates))

rm(list = setdiff(ls(), "config_list"))

devtools::load_all('model_code/popmodules')
devtools::load_all('model_code/trendmodel')

# Run the model
projection <- run_trend_model(config_list)
