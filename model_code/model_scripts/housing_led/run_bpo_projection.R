#' Run the borough and ward housing-led models using a BPO dwelling trajectory
#' 
#' A wrapper for create_bpo_trajectory and run_borough_and_ward
#'
#' @param bpo_name The name of the dwelling trajectory csv saved in the folder
#'   \code{bpo_dir} folder. The output projection will also have this name.
#' @param shlaa_first_year The first year in ehich to use shlaa development data.
#'   Effectively the final year of the supplied trajectory plus 1. \code{Default 2042}.  
#' @param first_proj_year The first projection year \code{default 2019}
#' @param final_proj_year The final projection year \code{default 2050}
#' @param bpo_dir The folder containing the dwelling trajectory csv

run_bpo_projection <- function(bpo_name,
                               shlaa_first_year = 2042,
                               first_proj_yr = 2019,
                               final_proj_yr = 2050,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2018_based/"){
  
  #sauce
  source('model_code/model_scripts/housing_led/bpo_template_to_rds.R')
  source('model_code/model_scripts/housing_led/run_borough_and_ward_projection.R')
  
  #trajectory
  borough_gss <- bpo_template_to_rds(csv_name = bpo_name,
                                     bpo_dir = bpo_dir,
                                     shlaa_first_year = shlaa_first_year)
  
  dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_borough_trajectory_",bpo_name,".rds")
  small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds")
  
  #projection
  bpo_projection <- run_borough_and_ward_projection(projection_name = bpo_name,
                                                    dev_trajectory_path,
                                                    small_area_dev_trajectory_path,
                                                    first_proj_yr,
                                                    final_proj_yr,
                                                    bpo = borough_gss)
  
  message(paste(bpo_name, "complete"))
  
}
