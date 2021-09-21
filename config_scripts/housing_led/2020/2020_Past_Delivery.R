library(popmodules)
library(housingledmodel)
library(smallareamodel)

n_proj_yr <- 30
projection_name <- "Past_Delivery"

dirs <- list.dirs("outputs/trend/2020", recursive = FALSE)
external_trend_path <- dplyr::last(stringr::str_sort(dirs[stringr::str_detect(dirs, "CH")]))

dev_trajectory_path <- "input_data/housing_led_model/borough_2020-based_ldd_mean.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/development_data/ward_2020-based_ldd_mean.rds"

standard_covid_migration <- TRUE

domestic_rates <- list(
  '2025' = list(path = "input_data/scenario_data/2020_dom_5yr_avg.rds",
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
msoa_projection <- run_small_area_model(config_list[[3]])

#-------------------------------------------------------------------------------
#Excel workbooks

output_dir <- config_lis[[1]]$output_dir
wb_filename <- "past_delivery_scenario"
projection_name <- "2020-based Scenario Projection: Past Delivery Scenario"

create_housingled_excels(output_dir, wb_filename, projection_name)
create_small_area_excels(output_dir, wb_filename, projection_name, smallarea = "ward")
create_small_area_excels(output_dir, wb_filename, projection_name, smallarea = "msoa")

