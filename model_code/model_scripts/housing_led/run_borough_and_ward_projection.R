#' Run the housing led and ward models with parametrised inputs
#'
#' @param projection_name String containing a name for the run. Used in the
#'   output directory.
#' @param dev_trajectory_path String containing a path to the input development
#'   trajectory.
#' @param small_area_dev_trajectory_path String containing a path the the input
#'   development trajectory at small-area resolution.
#' @param first_proj_year Integer. First year of projection.
#' @param last_proj_year Integer. Last year of projection.
#' @param housing_led_params,small_area_params Lists containing other key-value
#'   pairs of configuration parameters for the two models. These will be written
#'   over the default values, but won't supersede the above parameters. Note
#'   that parameters constructed from other parameters, e.g.
#'   external_ahs_trajectory_path which depends on external_trend_path, will
#'   need to be specified as well.

run_borough_and_ward_projection <- function(projection_name,
                                            dev_trajectory_path,
                                            small_area_dev_trajectory_path,
                                            first_proj_yr,
                                            last_proj_yr,
                                            bpo=FALSE,
                                            housing_led_params = list(),
                                            small_area_params = list(),
                                            fertility_scenario,
                                            ldd_final_yr,
                                            constrain_projection) {
  
  devtools::load_all("model_code/popmodules")
  source('model_code/model_scripts/housing_led/output_bpo_excel_file.R')
  
  external_trend_path <- "outputs/trend/2018/2018_central_19-11-13_2056/"
  
  communal_est_file <- "dclg_communal_est_population.rds"
  trend_households_file <- "dclg_stage_1_households.rds"
  ldd_backseries_path <- "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds"
  
  external_ahs_trajectory_path <- paste0(external_trend_path,"households/dclg_ahs.rds")
  
  hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
  ahs_cap_year <- 2019
  
  ahs_method <- 0
  last_data_yr <- 2018
  
  additional_births_path <- "input_data/fertility/births_2019.rds"
  
  if(fertility_scenario == "average"){
    fertility_rates_path <- "input_data/fertility/fertility_rates_inc_2019_in_london_5yr_avg.rds"
  }
  
  if(fertility_scenario == "trend"){
    fertility_rates_path <- "input_data/fertility/fertility_rates_inc_2019_in_london_5yr_trend.rds"
  }
  
  if(bpo==FALSE){
    output_dir <- paste0("outputs/housing_led/2018/",projection_name,"_",format(Sys.time(), "%y-%m-%d_%H%M"),"/")
  } else {
    output_dir <- paste0("outputs/housing_led/2018/bpo/",projection_name,"_",format(Sys.time(), "%y-%m-%d_%H%M"),"/")
  }
  
  domestic_transition_yr <- NULL
  domestic_long_term_rate_path <- NULL
  
  list2env(housing_led_params, environment())
  
  
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
  borough_projection <- run_housing_led_model(config_list)
  log_warnings(paste0(config_list$output_dir,"warnings.txt"))
  
  #----
  
  ####WARD MODEL
  small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates.rds"
  small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
  small_area_births_backseries_path <- "input_data/small_area_model/ward_births.rds"
  small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths.rds"
  small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"
  small_area_births_sya_path <- "input_data/small_area_model/ward_sya_births.rds"
  small_area_deaths_sya_path <- "input_data/small_area_model/ward_sya_deaths.rds"
  
  adults_per_dwelling_path <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
  small_area_to_district_path <- "input_data/lookup/2011_ward_to_district.rds"
  out_migration_rates_path <- "input_data/small_area_model/ward_out_migration_rates.rds"
  in_migration_characteristics_path <- "input_data/small_area_model/ward_in_migration_characteristics.rds"
  
  housing_led_model_path <- config_list$output_dir
  
  borough_fertility_rates_path <- paste0(config_list$external_trend_path,"fertility_rates.rds")
  borough_mortality_rates_path <- paste0(config_list$external_trend_path,"mortality_rates.rds")
  
  last_data_year <- 2018
  first_proj_yr <- config_list$first_proj_yr
  last_proj_yr <- config_list$last_proj_yr
  birth_rate_n_years_to_avg <- 5
  death_rate_n_years_to_avg <- 5
  
  projection_type <- "ward"
  
  list2env(small_area_params, environment())
  
  ward_config_list <- list(small_area_popn_estimates_path = small_area_popn_estimates_path,
                           small_area_communal_est_popn_path = small_area_communal_est_popn_path,
                           small_area_births_backseries_path = small_area_births_backseries_path,
                           small_area_deaths_backseries_path = small_area_deaths_backseries_path,
                           small_area_ldd_data_path = small_area_ldd_data_path,
                           small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                           small_area_births_sya_path = small_area_births_sya_path,
                           small_area_deaths_sya_path = small_area_deaths_sya_path,
                           
                           adults_per_dwelling_path = adults_per_dwelling_path,
                           small_area_to_district_path = small_area_to_district_path,
                           out_migration_rates_path = out_migration_rates_path,
                           in_migration_characteristics_path = in_migration_characteristics_path,
                           
                           housing_led_model_path = housing_led_model_path,
                           
                           borough_fertility_rates_path = borough_fertility_rates_path,
                           borough_mortality_rates_path = borough_mortality_rates_path,
                           
                           last_data_year = last_data_year,
                           first_proj_yr = first_proj_yr,
                           last_proj_yr = last_proj_yr,
                           
                           birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                           death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                           
                           ldd_final_yr = ldd_final_yr,
                           
                           projection_type = projection_type)
  
  rm(list = setdiff(ls(), c("ward_config_list","config_list","borough_projection","bpo")))
  
  source('model_code/model_scripts/small_area/small_area_control.R')
  ward_projection <- run_small_area_model(ward_config_list)
  log_warnings(paste0(ward_config_list$housing_led_model_path, ward_config_list$projection_type,"/warnings.txt"))
  
  #bpo
  if(bpo != FALSE) {
    output_bpo_excel_file(data = ward_projection[["csvs"]],
                          output_dir = config_list$output_dir,
                          projection_name = config_list$projection_name,
                          bpo_gss_code = bpo)
  }
  
  #Finish
  return(list(borough_projection = borough_projection,
              ward_projection = ward_projection))
  
}
