#' Run the borough and ward housing-led models using a BPO dwelling trajectory
#'
#' A wrapper for create_bpo_trajectory and run_borough_and_ward
#'
#' @param bpo_name String. The name of the dwelling trajectory csv saved in the folder
#'   \code{bpo_dir} folder. The output projection will also have this name.
#' @param shlaa_first_yr Numeric. The first year to use shlaa development data.
#'   Effectively the final year of the supplied trajectory plus 1. \code{Default 2042}.
#' @param first_proj_yr Numeric. The first projection year \code{default 2019}
#' @param last_proj_yr Numeric. The final projection year \code{default 2050}
#' @param dev_first_yr Numeric. The first year for which development data is provided. \code{Default 2019}.
#' @param ldd_final_yr Numeric. The final year of the LDD dwellings backseries. \code{Default 2018}.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv
#' @param migration_scenario String. The domestic migration scenario \code{high}, \code{medium} or \code{low}.
#' @param csv_name String. The name of the dwelling trajectory csv saved in the \code{bpo_dir} folder.
#'   With or without the \code{.csv} suffix. Default is \code{bpo_name}
#' @param housing_led_params A list of parameters (a partial config list) to be passed to the housing-led model.
#'   These will overwrite the default parameters for the bpo projection.
#' @param fertility_scenario String. Should the model use fertility rates based on an \code{average} or
#'  a \code{trend} of past data. Default is \code{average}
#'
#' @import dplyr

run_bpo_projection <- function(bpo_name,
                               shlaa_first_yr = 2042,
                               first_proj_yr = 2019,
                               last_proj_yr = 2050,
                               dev_first_yr = 2019,
                               ldd_final_yr = 2018,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2018_based/",
                               migration_scenario,
                               csv_name = bpo_name,
                               housing_led_params = list(),
                               fertility_scenario = "average"){
  
  dom_rates_loc <- "input_data/domestic_migration/processed_rates/"


  if(migration_scenario == "high"){
    housing_led_params$external_trend_path <- "outputs/trend/2018/high_mig_20-03-10_1436/"
    housing_led_params$domestic_rates <- list('2019'  = list(path = paste0(dom_rates_loc,"dom_rates_3yr_avg_2018.rds"),
                                                             transition = F))
    
  } else if(migration_scenario == "medium"){
    housing_led_params$external_trend_path <- "outputs/trend/2018/2018_central_19-11-13_2056/"
    housing_led_params$domestic_rates = list('2019' = list(path = paste0(dom_rates_loc,"dom_rates_5yr_avg_2018.rds"),
                                                           transition = F),
                                             '2024'= list(path = paste0(dom_rates_loc,"dom_rates_10yr_avg_2018.rds"),
                                                          transition = F))
    
  } else if(migration_scenario == "low"){
    housing_led_params$external_trend_path <- "outputs/trend/2018/low_mig_20-03-10_1446/"
    
    housing_led_params$domestic_rates = list('2019' = list(path = paste0(dom_rates_loc,"dom_rates_5yr_avg_2018.rds"),
                                                           transition = F),
                                             '2024'= list(path = paste0(dom_rates_loc,"dom_rates_4yr_avg_2012.rds"),
                                                          transition = F))
  }else if(migration_scenario == "other"){
    housing_led_params$domestic_rates <- list('2019'  = list(path = "input_data/migration/low_domestic_migration_rates_(2009_2012).rds",
                                                             tranition = F))
  } else {stop("migration scenario must be low, medium or high") }
  
  #trajectory
  borough_gss <- bpo_template_to_rds(csv_name = csv_name,
                                     bpo_dir = bpo_dir,
                                     shlaa_first_yr = shlaa_first_yr,
                                     dev_first_yr = dev_first_yr)
  
  dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_borough_trajectory_",csv_name,".rds")
  small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",csv_name,".rds")
  
  projection_name <- paste0(bpo_name,"_",migration_scenario,"_migration")
  if(fertility_scenario == "trend"){
    projection_name <- paste0(projection_name,"_trend_fertility")
  }
 
  #projection
  bpo_projection <- run_borough_and_ward_projection(projection_name = projection_name,
                                                    dev_trajectory_path = dev_trajectory_path,
                                                    small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                                                    first_proj_yr = first_proj_yr,
                                                    last_proj_yr = last_proj_yr,
                                                    bpo = borough_gss,
                                                    housing_led_params = housing_led_params,
                                                    fertility_scenario = fertility_scenario,
                                                    ldd_final_yr = ldd_final_yr,
                                                    constrain_projection = FALSE)
  
  message(paste(bpo_name, "-",migration_scenario,"complete"))

}