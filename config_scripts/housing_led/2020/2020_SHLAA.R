library(popmodules)
library(housingledmodel)
library(smallareamodel)

n_proj_yr <- 30
projection_name <- "SHLAA"

external_trend_path <- "outputs/trend/2020/2020_CC_21-08-16_0917"
dev_trajectory_path <- "input_data/housing_led_model/borough_2019_based_savills.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/development_data/ward_2019_based_savills.rds"

standard_covid_migration <- TRUE

domestic_rates <- list(
  '2028' = list(path = "input_data/scenario_data/2020_dom_10yr_avg.rds",
                transition = F))

#-------------------------------------------------------------------------------

source("config_scripts/housing_led/2020/standard_housingled_parameters.R")

config_list <- standard_housingled_parameters(list(projection_name = projection_name,
                                              n_proj_yr = n_proj_yr,
                                              external_trend_path = external_trend_path,
                                              dev_trajectory_path = dev_trajectory_path,
                                              small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                                              domestic_rates = domestic_rates,
                                              standard_covid_migration = standard_covid_migration))
rm(list = setdiff(ls(), "config_list"))

# Run the model
projection <- run_housing_led_model(config_list[[1]])
ward_projection <- run_small_area_model(config_list[[2]])
ward_projection <- run_small_area_model(config_list[[3]])

# output_housing_led_excel_file(ward_projection[["csvs"]],
#                               config_list$output_dir,
#                               config_list$projection_name,
#                               config_list$popn_adjustment_path,
#                               file_suffix = "_2020.xlsx")
