library(popmodules)
library(housingledmodel)
library(smallareamodel)
rm(list=ls())

n_proj_yr <- 2
projection_name <- "housing_led_scenario_1"

external_trend_path <- "outputs/trend/2020/2020_test_21-08-03_1208"
dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory_2020.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/development_data/ward_shlaa_trajectory_2020.rds"

standard_covid_migration <- TRUE

domestic_rates <- list(
  '2028' = list(path = "input_data/scenario_data/2020_dom_10yr_avg.rds",
                transition = F))

#-------------------------------------------------------------------------------

source('config_scripts/housing_led/2020/standard_2020_housingled_parameters.R')

projection <- run_housing_led_model(config_list)
ward_projection <- run_small_area_model(ward_config_list)
msoa_projection <- run_small_area_model(msoa_config_list)

output_borough_excels(ward_projection[["csvs"]],
                              config_list$output_dir,
                              config_list$projection_name,
                              file_suffix = "_2020.xlsx")

output_small_area_excels(config_list$output_dir,
                         config_list$projection_name,
                         ward = TRUE, msoa = TRUE)
