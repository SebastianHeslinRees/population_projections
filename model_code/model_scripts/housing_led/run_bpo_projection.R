#' Run the borough and ward housing-led models using a BPO dwelling trajectory
#' 
#' A wrapper for create_bpo_trajectory and run_borough_and_ward
#'
#' @param bpo_name String. The name of the dwelling trajectory csv saved in the folder
#'   \code{bpo_dir} folder. The output projection will also have this name.
#' @param shlaa_first_yr Numeric. The first year to use shlaa development data.
#'   Effectively the final year of the supplied trajectory plus 1. \code{Default 2042}.  
#' @param first_proj_yr Numeric. The first projection year \code{default 2019}
#' @param final_proj_yr Numeric. The final projection year \code{default 2050}
#' @param dev_first_yr Numeric. The first year for which development data is provided. \code{Default 2019}.
#' @param ldd_last_yr Numeric. The final year of the LDD dwellings backsries. \code{Default 2018}.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv
#' @migration_sceanrio String. The domestic migration scenario \code{high}, \code{medium} or \code{low}.

run_bpo_projection <- function(bpo_name,
                               shlaa_first_yr = 2042,
                               first_proj_yr = 2019,
                               final_proj_yr = 2050,
                               dev_first_yr = 2019,
                               ldd_final_yr = 2018,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2018_based/",
                               migration_scenario){
  
  #sauce
  library(dplyr)
  source('model_code/model_scripts/housing_led/bpo_template_to_rds.R')
  source('model_code/model_scripts/housing_led/run_borough_and_ward_projection.R')
  
  #domestic migration
  if(migration_scenario == "high"){
    housing_led_params <- list(
      domestic_transition_yr = NULL,
      domestic_initial_rate_path = "input_data/migration/high_domestic_migration_rates_(2016_2018).rds",
      domestic_long_term_rate_path = NULL)
  } else if(migration_scenario == "medium"){
    housing_led_params <- list(
      domestic_transition_yr = 2024,
      domestic_initial_rate_path = "outputs/trend/2018/2018_short_19-11-13_2205/domestic_rates.rds",
      domestic_long_term_rate_path = "outputs/trend/2018/2018_central_19-11-13_2056/domestic_rates.rds")
  } else if(migration_scenario == "low"){
    housing_led_params <- list(
      domestic_transition_yr = 2024,
      domestic_initial_rate_path = "outputs/trend/2018/2018_short_19-11-13_2205/domestic_rates.rds",
      domestic_long_term_rate_path = "input_data/migration/low_domestic_migration_rates_(2009_2012).rds")
  }else if(migration_scenario == "other"){
    housing_led_params <- list(
      domestic_transition_yr = NULL,
      domestic_initial_rate_path = "input_data/migration/low_domestic_migration_rates_(2009_2012).rds",
      domestic_long_term_rate_path = NULL)
  } else {stop("migration scenario must be low, medium or high") }

  #trajectory
  borough_gss <- bpo_template_to_rds(csv_name = bpo_name,
                                     bpo_dir = bpo_dir,
                                     shlaa_first_yr = shlaa_first_yr,
                                     dev_first_yr = dev_first_yr,
                                     ldd_final_yr = ldd_final_yr)
  
  dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_borough_trajectory_",bpo_name,".rds")
  small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds")
  projection_name <- paste0(bpo_name,"_",migration_scenario,"_migration")
  housing_led_params[['constrain_projection']] <- FALSE
  
  #projection
  bpo_projection <- run_borough_and_ward_projection(projection_name = projection_name,
                                                    dev_trajectory_path,
                                                    small_area_dev_trajectory_path,
                                                    first_proj_yr,
                                                    final_proj_yr,
                                                    bpo = borough_gss,
                                                    housing_led_params = housing_led_params)
  
  message(paste(bpo_name, "-",migration_scenario,"complete"))
  
}
