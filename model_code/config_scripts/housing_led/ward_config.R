projection_name <- "test_shlaa"
first_proj_yr <- 2019
final_proj_yr <- 2020
dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/ward_shlaa_trajectory.rds"

small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates_2010_2018.rds"
small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
small_area_births_backseries_path <- "input_data/small_area_model/ward_births_2001_2018.rds"
small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths_2001_2018.rds"
small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"

external_trend_path <- "outputs/trend/2018/2018_central/"
external_trend_datestamp <- "19-11-13_2056"

housing_led_model_path <- paste0("outputs/housing_led/2018/",projection_name,"/")
housing_led_model_timestamp <- "20-01-22_0951"

borough_fertility_rates_path <- paste0(external_trend_path,"fertility_rates_",
                                       external_trend_datestamp,".rds")
borough_mortality_rates_path <- paste0(external_trend_path,"mortality_rates_",
                                       external_trend_datestamp,".rds")

last_data_year <- 2018
birth_rate_n_years_to_avg <- 5
death_rate_n_years_to_avg <- 5

projection_type <- "ward"

small_area_output_dir <- paste0("outputs/housing_led/2018/",projection_name,"/",projection_type,"/")

ward_config_list <- list(small_area_popn_estimates_path = small_area_popn_estimates_path,
                         small_area_communal_est_popn_path = small_area_communal_est_popn_path,
                         small_area_births_backseries_path = small_area_births_backseries_path,
                         small_area_deaths_backseries_path = small_area_deaths_backseries_path,
                         small_area_ldd_data_path = small_area_ldd_data_path,
                         small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                         
                         housing_led_model_path = housing_led_model_path,
                         housing_led_model_timestamp = housing_led_model_timestamp,
                         
                         borough_fertility_rates_path = borough_fertility_rates_path,
                         borough_mortality_rates_path = borough_mortality_rates_path,
                         
                         last_data_year = last_data_year,
                         first_proj_yr = first_proj_yr,
                         final_proj_yr = final_proj_yr,
                         
                         birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                         death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                         
                         projection_type = projection_type,
                         projection_name = projection_name,
                         
                         small_area_output_dir = small_area_output_dir)

rm(list = setdiff(ls(), "ward_config_list"))

source('model_code/model_scripts/small_area/small_area_control.R')
ward_projection <- run_small_area_model(ward_config_list)
log_warnings(paste0(ward_config_list$small_area_output_dir,ward_config_list$projection_name,"_warnings.txt"))
