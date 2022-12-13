library(trendmodel)

projection_name <- "2021_10yr"
standard_covid_migration <- TRUE
n_proj_yr <- 29

#-------------------------------------------------------------------------------

int_out <- list(
  '2025' = list(path = "input_data/scenario_data/2021_int_out_10yr_avg_flow.rds",
                transition = F))

#-------------------------------------------------------------------------------

int_in  <- list(
  '2025' = list(path = "input_data/scenario_data/2021_int_in_10yr_avg.rds",
                transition = F))
#-------------------------------------------------------------------------------

domestic_rates <- list(
  '2022' = list(path = "input_data/scenario_data/2021_dom_10yr_avg.rds",
                transition = F)) # TODO

#-------------------------------------------------------------------------------

births_2022 <- "input_data/scenario_data/births_mid_22.rds"
  
#-------------------------------------------------------------------------------

source("config_scripts/trend/2021/standard_trend_parameters.R")

config_list <- standard_trend_parameters(list(projection_name = projection_name,
                                              standard_covid_migration = standard_covid_migration,
                                              int_out_flows_rates = int_out,
                                              int_in = int_in,
                                              domestic_rates = domestic_rates,
                                              n_proj_yr = n_proj_yr,
                                              int_out_method = "flow",
                                              external_births_path = births_2022))

rm(list = setdiff(ls(), "config_list"))

# devtools::load_all('model_code/popmodules')
# devtools::load_all('model_code/trendmodel')

# Run the model
projection <- run_trend_model(config_list)
