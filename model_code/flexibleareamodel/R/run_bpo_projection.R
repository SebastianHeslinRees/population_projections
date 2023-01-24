#' Run the borough and ward housing-led models using a BPO dwelling trajectory
#'
#' A wrapper for run_borough_and_ward_projection. Takes as input a csv in the
#' of the GLA housing trajectory template for any borough and converts it into
#' an rds in tidy format. Run the borough housing-led model and the ward small
#' area model using a set of standard model parameters.
#'
#' @param bpo_name String. The name of the dwelling trajectory csv saved in the
#'   folder \code{bpo_dir} folder. The output projection will also have this name.
#' @param variant String. Projection scenario or variant.
#' @param trajectory_range Numeric or NULL. The years covered by the input trajectory.
#'  Default 2020:2041.
#' @param projection_range Numeric. The years to project. \code{Default 2020:2050}.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv
#' @param csv_name String. The name of the dwelling trajectory csv saved in the
#'
#' @import dplyr
#' @import stringr
#' @import assertthat
#' @export

run_bpo_projection <- function(bpo_name,
                               variant,
                               trajectory_range = 2012:2041,
                               projection_range = 2022:2041,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2021_based/",
                               csv_name = bpo_name){
  
  first_proj_yr <- min(projection_range)
  last_proj_yr <- max(projection_range)
  
  assert_that(variant %in% c("5-year constrained","5-year unconstrained",
                             "10-year constrained","10-year unconstrained"))
  
  proj_name <- paste(bpo_name, str_replace_all(variant, " ","_"), sep = "_")
  
  #-----------------------------------------------------------------------------
  
  # Standard BPO parameters are pulled from a function saved in the
  # config scripts folder
  
  source("config_scripts/flexible_area_model/2021_based/standard_bpo_parameters.R")
  
  config_list <- standard_bpo_parameters(list(first_proj_yr=first_proj_yr,
                                              last_proj_yr=last_proj_yr,
                                              proj_name= proj_name),
                                         variant)
  
  #-----------------------------------------------------------------------------
  
  # Process the csv trajectory into an rds file
  # And get the gss code for the borough in question
  
  borough_gss <- bpo_template_to_rds(csv_name = csv_name,
                                     bpo_dir = bpo_dir,
                                     trajectory_range = trajectory_range)
  
  config_list$dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",csv_name,".rds")
  
  
  #-----------------------------------------------------------------------------
  
  # Run projections
  
  bpo <- flexmodel_hl_projection(config_list, n_cores = 20)
  rm(bpo)
  gc()
  
  #-----------------------------------------------------------------------------
  
  # Excel output
  
  borough <- .camel(str_replace_all(bpo_name, "_", " "))
  
  s <- case_when(str_detect(variant, "5") ~ "5-year migration trend",
                 str_detect(variant, "10") ~ "10-year migration trend",
                 TRUE ~ "unknown scenario")
  
  c <- ifelse(str_detect(variant, "unconstrained"),
              "(unconstrained projection)",
              "(constrained projection)")
  
  proj_desc <- paste( paste(borough, "BPO"),
                      paste(s,c),
                      "BPO development data",
                      "2022 ward boundaries",
                      sep = " - ")
  
  create_excel(config_list$output_dir, proj_name, proj_desc, borough_gss)
  
  message("complete")
  
}
