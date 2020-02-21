devtools::load_all("model_code/popmodules")

projection_name <- "2018_based_shlaa_dev_20-02-05_1808"

first_proj_yr <- 2019
final_proj_yr <- 2050

dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/ward_shlaa_trajectory.rds"

small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates.rds"
small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
small_area_births_backseries_path <- "input_data/small_area_model/ward_births.rds"
small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths.rds"
small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"

adults_per_dwelling_path <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
small_area_to_district_path <- "input_data/lookup/2011_ward_to_district.rds"
out_migration_rates_path <- "input_data/small_area_model/ward_out_migration_rates.rds"
in_migration_characteristics_path <- "input_data/small_area_model/ward_in_migration_characteristics.rds"

external_trend_path <- "outputs/trend/2018/2018_central_19-11-13_2056/"

housing_led_model_path <- paste0("outputs/housing_led/2018/",projection_name,"/")

borough_fertility_rates_path <- paste0(external_trend_path,"fertility_rates.rds")
borough_mortality_rates_path <- paste0(external_trend_path,"mortality_rates.rds")

last_data_year <- 2018
birth_rate_n_years_to_avg <- 5
death_rate_n_years_to_avg <- 5
ldd_final_yr <- 2018

projection_type <- "ward"

ward_config_list <- list(small_area_popn_estimates_path = small_area_popn_estimates_path,
                         small_area_communal_est_popn_path = small_area_communal_est_popn_path,
                         small_area_births_backseries_path = small_area_births_backseries_path,
                         small_area_deaths_backseries_path = small_area_deaths_backseries_path,
                         small_area_ldd_data_path = small_area_ldd_data_path,
                         small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                         
                         adults_per_dwelling_path = adults_per_dwelling_path,
                         small_area_to_district_path = small_area_to_district_path,
                         out_migration_rates_path = out_migration_rates_path,
                         in_migration_characteristics_path = in_migration_characteristics_path,
                         
                         housing_led_model_path = housing_led_model_path,
                         
                         borough_fertility_rates_path = borough_fertility_rates_path,
                         borough_mortality_rates_path = borough_mortality_rates_path,
                         
                         last_data_year = last_data_year,
                         first_proj_yr = first_proj_yr,
                         final_proj_yr = final_proj_yr,
                         
                         birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                         death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                         
                         ldd_final_yr = ldd_final_yr,
                         
                         projection_type = projection_type)

rm(list = setdiff(ls(), "ward_config_list"))

source('model_code/model_scripts/small_area/small_area_control.R')
ward_projection <- run_small_area_model(ward_config_list)
log_warnings(paste0(ward_config_list$housing_led_model_path, ward_config_list$projection_type,"/warnings.txt"))
