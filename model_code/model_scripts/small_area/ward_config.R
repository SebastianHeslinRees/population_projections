small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates_2010_2017.rds"
small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
small_area_births_backseries_path <- "input_data/small_area_model/ward_births_2001_2018.rds"
small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths_2001_2018.rds"
small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"
small_area_development_trajectory <- "input_data/small_area_model/ward_shlaa_trajectory.rds"

housing_led_model_path <- "outputs/housing_led/2018/test"
housing_led_model_timestamp <- "20-01-16_1102"

borough_fertility_rates_path <- "outputs/trend/2018/2018_central/fertility_rates_19-11-13_2056.rds"
borough_mortality_rates_path <- "outputs/trend/2018/2018_central/mortality_rates_19-11-13_2056.rds"

last_data_year <- 2018
first_proj_year <- 2018
final_proj_year <- 2021
birth_rate_n_years_to_avg <- 5
death_rate_n_years_to_avg <- 5

projection_type <- "ward"
projection_name <- "test_01"

config_list <- list(small_area_popn_estimates_path = small_area_popn_estimates_path,
                    small_area_communal_est_popn_path = small_area_communal_est_popn_path,
                    small_area_births_backseries_path = small_area_births_backseries_path,
                    small_area_deaths_backseries_path = small_area_deaths_backseries_path,
                    small_area_ldd_data_path = small_area_ldd_data_path,
                    small_area_development_trajectory = small_area_development_trajectory,
                    housing_led_model_path = housing_led_model_path,
                    housing_led_model_timestamp = housing_led_model_timestamp,
                    borough_fertility_rates_path = borough_fertility_rates_path,
                    borough_mortality_rates_path = borough_mortality_rates_path,
                    last_data_year = last_data_year,
                    first_proj_year = first_proj_year,
                    final_proj_year = final_proj_year,
                    birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                    death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                    projection_type = projection_type,
                    projection_name = projection_name)
  
rm(list = setdiff(ls(), "config_list"))

source('M:/Projects/population_projections/model_code/model_scripts/small_area/small_area_control.R')
ward_projection <- run_small_area_model(config_list)
log_warnings(paste0(copy_dir, "/warnings_", config_list$timestamp, ".txt"))