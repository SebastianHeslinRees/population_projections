#library(popmodules)
#library(trendmodel)

#CC
#Central International
#Central Domestic


projection_name <- "2020_CC"
standard_covid_migration <- TRUE

#-------------------------------------------------------------------------------

int_out_flows_rates <- list(
  '2028' = list(path = "input_data/scenario_data/2019_int_out_10yr_avg.rds",
                transition = F))

#-------------------------------------------------------------------------------

int_in  <- list(
  '2028' = list(path = "input_data/scenario_data/2019_int_in_10yr_avg.rds",
                transition = F))
#-------------------------------------------------------------------------------

domestic_rates <- list(
  '2028' = list(path = "input_data/scenario_data/2020_dom_10yr_avg.rds",
                transition = F))

#-------------------------------------------------------------------------------

source('config_scripts/trend/2020/standard_2020_trend_parameters.R')

# Run the model
devtools::load_all('model_code/popmodules')
devtools::load_all('model_code/trendmodel')
projection <- run_trend_model(config_list)
