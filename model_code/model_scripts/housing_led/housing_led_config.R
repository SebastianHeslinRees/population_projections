devtools::load_all('model_code/popmodules')

#Setup
projection_name <- "test"
trend_path <- "outputs/trend/2018/2018_central/"
trend_datestamp <- "19-11-13_2056"
communal_est_file <- "ons_communal_est_population.rds"
trend_households_file <- "ons_stage_1_households.rds"

dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
ahs_trajectory_path <- "input_data/housing_led_model/dclg_ahs.rds"

hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
first_proj_yr <- 2019
final_proj_yr <- 2022
ahs_cap_year <- 2020
final_proj_yr <- 2022
ldd_max_yr <- 2019

#-----------------
#Component Constraints
constraint_data_fns <- list(
  list(fn = popmodules::get_data_from_file,
       args = list(
         list(
           birth_constraint = paste0(trend_path,"births_",trend_datestamp,".rds"),
           death_constraint = paste0(trend_path,"deaths_",trend_datestamp,".rds"),
           international_out_constraint = paste0(trend_path,"int_out_",trend_datestamp,".rds")))))

#------------------
#Component Rates
component_rates_fns <- list(
  list(fn = popmodules::get_data_from_file,
       args= list(list(
         fertility_rates = paste0(trend_path,"fertility_rates_",trend_datestamp,".rds"),
         mortality_rates = paste0(trend_path,"mortality_rates_",trend_datestamp,".rds"),
         int_out_flows_rates = paste0(trend_path,"int_out_rates_",trend_datestamp,".rds"),
         int_in_flows = paste0(trend_path,"int_in_",trend_datestamp,".rds"),
         domestic_rates = paste0(trend_path,"domestic_rates_",trend_datestamp,".rds")))))

#-------------------
#Setup config list
config_list <- list(
  projection_name = projection_name,
  constraint_data_fns = constraint_data_fns,
  component_rates_fns = component_rates_fns,
  communal_est_path = paste0(trend_path,"households_",trend_datestamp,"/",communal_est_file),
  hma_constraint = readRDS(paste0(trend_path, "population_", trend_datestamp,".rds")),
  hma_list = hma_list,
  dev_trajectory_path = dev_trajectory_path,
  ahs_trajectory_path = ahs_trajectory_path,
  trend_households_path = paste0(trend_path,"households_",trend_datestamp,"/",trend_households_file),
  ahs_cap_year = ahs_cap_year,
  trend_path = trend_path,
  trend_datestamp = trend_datestamp,
  first_proj_yr = first_proj_yr,
  final_proj_yr = final_proj_yr,
  ldd_max_yr = ldd_max_yr)

#---------------------
#run projection
source('model_code/model_scripts/housing_led/housing_led_control.R')
projection <- run_housing_led_model(config_list)

