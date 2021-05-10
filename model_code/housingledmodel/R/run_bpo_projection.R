#' Run the borough and ward housing-led models using a BPO dwelling trajectory
#'
#' A wrapper for run_borough_and_ward_projection. Takes as input a csv in the
#' of the GLA housing trajectory template for any borough and converts it into
#' an rds in tidy format. Run the borough housing-led model and the ward small
#' area model using a set of standard model parameters.
#'
#' @param bpo_name String. The name of the dwelling trajectory csv saved in the
#'   folder \code{bpo_dir} folder. The output projection will also have this name.
#' @param trajectory_range Numeric or NULL. The years covered by the input trajectory.
#'  \code{Default 2020:2041}. If NULL the SHLAA trajectory is used.
#' @param projection_range Numeric. The years to project. \code{Default 2020:2050}.
#' @param ldd_final_yr Numeric. The final year of the LDD dwellings backseries.
#'   \code{Default 2019}.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv
#' @param variant String. The domestic migration scenario \code{high},
#'   \code{high} or \code{low}.
#' @param csv_name String. The name of the dwelling trajectory csv saved in the
#'   \code{bpo_dir} folder. With or without the "csv" suffix. Default is \code{bpo_name}
#' @param housing_led_params A list of parameters (a partial config list) to be
#'   passed to the housing-led model. These will overwrite the default parameters
#'   for the bpo projection.
#' @param fertility_scenario String. Should the model use fertility rates based
#'   on an \code{average} or a \code{trend} of past data. Default is \code{average}
#'
#' @import dplyr
#' @export

run_bpo_projection <- function(bpo_name,
                               trajectory_range = 2020:2041,
                               projection_range = 2020:2050,
                               ldd_final_yr = 2019,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2019_based/",
                               variant,
                               csv_name = bpo_name,
                               housing_led_params = list(),
                               fertility_scenario = "trend"){
  
  message(paste(bpo_name, "-", variant))
  
  first_proj_yr <- min(projection_range)
  last_proj_yr <- max(projection_range)
  
  #Some checks on the LDD year
  if(ldd_final_yr > 2019){stop("ldd_final_yr cannot be greater than 2019")}
  if(ldd_final_yr >= min(trajectory_range)){
    message(paste0("Warning message:\n",
                   "ldd_final_year is ", ldd_final_yr, " but the trajectory starts in ",
                   min(trajectory_range), ".\nTrajectory years up to and including ",  ldd_final_yr,
                   " will be overwritten.\nTo use the input trajectory for those years set the ",
                   "ldd_final_year parameter to ", min(trajectory_range)-1))
  }
  
  #Set the domestic migration data and paths for the variant projection that's been selected
  
  if(variant == "scenario_1"){
    external_trend_path <- paste0(list.files(path="outputs/trend/2019/", pattern= "2019_BPO_scenario_1", full.names = TRUE),"/")
    housing_led_params$domestic_rates <- list('2020' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2020.rds",
                                                            transition = F),
                                              '2021' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2021.rds",
                                                            transition = F),
                                              '2022' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2022.rds",
                                                            transition = T),
                                              '2028' = list(path = "input_data/domestic_migration/processed_rates/dom_rates_5yr_avg_2019_gla_mye.rds",
                                                            transition = F))
    
  } else if(variant == "scenario_2"){
    external_trend_path <- paste0(list.files(path="outputs/trend/2019/", pattern= "2019_BPO_scenario_2", full.names = TRUE),"/")
    housing_led_params$domestic_rates = list('2020' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2020.rds",
                                                           transition = F),
                                             '2021' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2021.rds",
                                                           transition = F),
                                             '2022' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2022.rds",
                                                           transition = T),
                                             '2028' = list(path = "input_data/domestic_migration/processed_rates/dom_rates_10yr_avg_2019_gla_mye.rds",
                                                           transition = F))
    
  } else if(variant == "scenario_3"){
    external_trend_path <- paste0(list.files(path="outputs/trend/2019/", pattern= "2019_BPO_scenario_3", full.names = TRUE),"/")
    housing_led_params$domestic_rates = list('2020' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2020.rds",
                                                           transition = F),
                                             '2021' = list(path = "input_data/scenario_data/bpo_dom_scenario_3_yr_2021.rds",
                                                           transition = F),
                                             '2022' = list(path = "input_data/scenario_data/bpo_dom_scenario_3_yr_2022.rds",
                                                           transition = T),
                                             '2028' = list(path = "input_data/domestic_migration/processed_rates/dom_rates_5yr_avg_2019_gla_mye.rds",
                                                           transition = F))
    
  } else {stop("migration scenario must be scenario_1, scenario_2 or scenario_3") }
  
  #process the csv trajectory into an rds file
  #if no trajectory is supplied then use SHLAA
  #Also, figure out the gss code for the borough in question
  if(!is.null(trajectory_range)){
    borough_gss <- bpo_template_to_rds(csv_name = csv_name,
                                       bpo_dir = bpo_dir,
                                       trajectory_range = trajectory_range)
    dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_borough_trajectory_",csv_name,".rds")
    small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",csv_name,".rds")
    
  } else {
    borough_gss <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% 
      mutate(short_name = tolower(substr(gss_name,1,4))) %>% 
      filter(substr(gss_code,1,3)=="E09",
             short_name == substr(bpo_name,1,4)) %>% 
      unique() %>% 
      .$gss_code
    dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_pandemic_adjusted.rds"
    small_area_dev_trajectory_path <- "input_data/small_area_model/development_data/ward_shlaa_pandemic_adjusted.rds"
  }
  
  #give the projection a name
  projection_name <- paste0(bpo_name,"_",variant)

  if(fertility_scenario == "average"){
    projection_name <- paste0(projection_name,"_avg_fertility")
  }
  
  #Standard 80% trend, 20% dclg for the 2019-based BPOs
  housing_led_params$ahs_method <- 0.8
  
  #run the projection
  bpo_projection <- run_borough_and_ward_projection(projection_name = projection_name,
                                                    dev_trajectory_path = dev_trajectory_path,
                                                    small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                                                    external_trend_path = external_trend_path,
                                                    first_proj_yr = first_proj_yr,
                                                    last_proj_yr = last_proj_yr,
                                                    bpo = borough_gss,
                                                    housing_led_params = housing_led_params,
                                                    fertility_scenario = fertility_scenario,
                                                    ldd_final_yr = ldd_final_yr,
                                                    constrain_projection = FALSE)
  
  message("complete")
  
}
