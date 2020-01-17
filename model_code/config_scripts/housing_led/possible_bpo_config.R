devtools::load_all('model_code/popmodules')

#Setup
projection_name <- "test_02"
external_trend_path <- "outputs/trend/2018/2018_central/"
external_trend_datestamp <- "19-11-13_2056"
communal_est_file <- "ons_communal_est_population.rds"
trend_households_file <- "ons_stage_1_households.rds"
ldd_backseries_path <- "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds"

dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
external_ahs_trajectory_path <- paste0(external_trend_path,"households_",external_trend_datestamp,"/ons_ahs.rds")

hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
first_proj_yr <- 2019
final_proj_yr <- 2022
ahs_cap_year <- 2019
ldd_max_yr <- 2018


#------------------
#Setup config list
config_list <- list(
  projection_name = projection_name,
  communal_est_file = communal_est_file,
  hma_list = hma_list,
  dev_trajectory_path = dev_trajectory_path,
  external_ahs_trajectory_path = external_ahs_trajectory_path,
  trend_households_file = trend_households_file,
  ldd_backseries_path = ldd_backseries_path,
  ahs_cap_year = ahs_cap_year,
  external_trend_path = external_trend_path,
  external_trend_datestamp = external_trend_datestamp,
  first_proj_yr = first_proj_yr,
  final_proj_yr = final_proj_yr,
  ldd_max_yr = ldd_max_yr,
  timestamp = format(Sys.time(), "%y-%m-%d_%H%M"))

#---------------------
#run projection
source('model_code/model_scripts/housing_led/housing_led_control.R')
borough_projection <- run_housing_led_model(config_list)
log_warnings(paste0("outputs/housing_led/2018/",config_list$projection_name,"/warnings_",config_list$timestamp,".txt"))

#----
####WARD MODEL

small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates_2010_2018.rds"
small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
small_area_births_backseries_path <- "input_data/small_area_model/ward_births_2001_2018.rds"
small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths_2001_2018.rds"
small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"
small_area_dev_trajectory_path <- "input_data/small_area_model/ward_shlaa_trajectory.rds"

housing_led_model_path <- paste0("outputs/housing_led/2018/",config_list$projection_name,"/")
housing_led_model_timestamp <- config_list$timestamp

borough_fertility_rates_path <- paste0(config_list$external_trend_path,"fertility_rates_",
                                       config_list$external_trend_datestamp,".rds")
borough_mortality_rates_path <- paste0(config_list$external_trend_path,"mortality_rates_",
                                       config_list$external_trend_datestamp,".rds")

last_data_year <- 2018
first_proj_yr <- config_list$first_proj_yr
final_proj_yr <- config_list$final_proj_yr
birth_rate_n_years_to_avg <- 5
death_rate_n_years_to_avg <- 5

projection_type <- "ward"
projection_name <- paste0(projection_name,"_",projection_type)

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
                    projection_name = projection_name)

rm(list = setdiff(ls(), "ward_config_list"))

source('M:/Projects/population_projections/model_code/model_scripts/small_area/small_area_control.R')
ward_projection <- run_small_area_model(ward_config_list)
log_warnings(paste0("outputs/ward/",ward_config_list$projection_name,"_warnings.txt"))
