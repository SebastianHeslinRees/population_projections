rm(list=ls())
projection_name <- "test_04"
first_proj_yr <- 2019
dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/ward_shlaa_trajectory.rds"


source('model_code/config_scripts/housing_led/config_function.R')
run_bpo_projection(projection_name,
                   dev_trajectory_path,
                   small_area_dev_trajectory_path,
                   first_proj_yr)
