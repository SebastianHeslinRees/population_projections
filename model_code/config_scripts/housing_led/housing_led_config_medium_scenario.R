devtools::load_all("model_code/popmodules")

#Setup
projection_name <- "2018_based_shlaa_dev_medium_migration"
external_trend_path <- "outputs/trend/2018/2018_central_19-11-13_2056/"
communal_est_file <- "dclg_communal_est_population.rds"
trend_households_file <- "dclg_stage_1_households.rds"
ldd_backseries_path <- "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds"

dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
external_ahs_trajectory_path <- paste0(external_trend_path, "households/dclg_ahs.rds")

hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
constrain_projection <- FALSE
first_proj_yr <- 2019
last_proj_yr <- 2050
ahs_cap_year <- 2019
ahs_method <- 0

ldd_final_yr <- 2018
last_data_yr <- 2018

output_dir <- paste0("outputs/housing_led/2018/",projection_name,"_",format(Sys.time(), "%y-%m-%d_%H%M"),"/")

dom_rates_loc <- "input_data/domestic_migration/processed_rates/"
housing_led_params$domestic_rates = list('2019' = list(path = paste0(dom_rates_loc,"dom_rates_5yr_avg_2018.rds"),
                                                       transition = F),
                                         '2024'= list(path = paste0(dom_rates_loc,"dom_rates_10yr_avg_2018.rds"),
                                                      transition = F))

additional_births_path <- NULL
fertility_rates_path <- paste0(external_trend_path,"fertility_rates.rds")


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
  first_proj_yr = first_proj_yr,
  last_proj_yr = last_proj_yr,
  ldd_final_yr = ldd_final_yr,
  last_data_yr = last_data_yr,
  output_dir = output_dir,
  domestic_rates = domestic_rates,
  constrain_projection = constrain_projection,
  ahs_method = ahs_method,
  additional_births_path = additional_births_path,
  fertility_rates_path = fertility_rates_path,
  upc_path = NULL)

#---------------------
#run projection
source('model_code/model_scripts/housing_led/housing_led_control.R')
projection <- run_housing_led_model(config_list)
log_warnings(paste0(output_dir,"warnings.txt"))
