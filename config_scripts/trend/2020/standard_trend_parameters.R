#' Create a trend model config list using standard parameters for the 2020-based
#' projections.
#'
#' @param params A named list of trend model parameters which will overwrite the
#'   standard parameters
#'
#' @return A named config list ready for input into `run_trend_model`
#'

standard_trend_parameters <- function(params){

  assertthat::assert_that("projection_name" %in% names(params),
                          msg = "projection_name must be specified")
  
  list2env(params, environment())
  standard <- list()
  
  #-------------------------------------------------------------------------------
  #Basics
  standard$first_proj_yr <- 2021
  standard$n_proj_yr <- 30
  
  #-------------------------------------------------------------------------------
  #Backseries
  
  standard$popn_mye_path <- paste0("input_data/mye/2020/population_gla.rds")
  standard$deaths_mye_path <-  paste0("input_data/mye/2020/deaths_ons.rds")
  standard$births_mye_path <-  paste0("input_data/mye/2020/births_ons.rds")
  standard$int_out_mye_path <-  paste0("input_data/mye/2020/int_out_gla.rds")
  standard$int_in_mye_path <-  paste0("input_data/mye/2020/int_in_gla.rds")
  standard$dom_out_mye_path <- paste0("input_data/domestic_migration/2020/domestic_migration_out_(2021_geog).rds")
  standard$dom_in_mye_path <- paste0("input_data/domestic_migration/2020/domestic_migration_in_(2021_geog).rds")
  standard$upc_mye_path <- "input_data/mye/2020/upc_ons.rds"
  standard$popn_adjustment_path <- NULL
  standard$external_births_path <- NULL
  standard$external_deaths_path <- "input_data/mortality/external_deaths_2021.rds"
  
  #-------------------------------------------------------------------------------
  #Household model
  
  standard$ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates_(2021_geog).rds"
  standard$ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016_(2021_geog).rds"
  standard$communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population_(2021_geog).rds"
  standard$dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014_(2021_geog).rds"
  standard$dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014_(2021_geog).rds"
  
  #-------------------------------------------------------------------------------
  #Mort/fert
  
  standard$mortality_rates <- "input_data/mortality/mort_rates_5yr_trend_2020.rds"
  standard$fertility_rates <- "input_data/fertility/fert_rates_5yr_trend_2020.rds"
  
  #-------------------------------------------------------------------------------
  
  #Other stuff
  
  timestamp <- format(Sys.time(), "%y-%m-%d_%H%M")
  projection_name <- paste0(projection_name,"_",timestamp)
  standard$output_dir <- paste0("outputs/trend/2020/",projection_name,"/")
  standard$write_excel <- TRUE
  standard$constraint_fns <- list(list(fn = function() NULL, args = list()))
  standard$int_out_flow_or_rate <- "flow"
  standard$geog_code_col <- "gss_code"
  
  #-------------------------------------------------------------------------------
  
  #These 2 line ensure that any parameters set in advance of this script running
  #replace the standard parameters
  standard <- standard[!names(standard) %in% names(params)]
  list2env(standard, environment())
  
  #-------------------------------------------------------------------------------
  
  #This runs a separate script to add the standard covid migration assumptions
  #to the 3 migration parameters
  if(standard_covid_migration){
    source('config_scripts/trend/2020/standard_2020_covid_migration.R')
    mig <- covid_migration(int_out_flows_rates, int_in, domestic_rates)
    int_out_flows_rates <- mig[[1]]
    int_in <- mig[[2]]
    domestic_rates <- mig[[3]]
  }
  
  if(!"external_births_path" %in% ls()){
    external_births_path <- NULL
  }
  
  if(!"external_deaths_path" %in% ls()){
    external_deaths_path <- NULL
    
  }
  if(!"popn_adjustment_path" %in% ls()){
    popn_adjustment_path <- NULL
  }
  
  #-------------------------------------------------------------------------------
  
  config_list <- list(
    projection_name = projection_name,
    first_proj_yr = first_proj_yr,
    n_proj_yr = n_proj_yr,
    output_dir = output_dir,
    
    popn_mye_path = popn_mye_path,
    deaths_mye_path = deaths_mye_path,
    births_mye_path = births_mye_path,
    int_out_mye_path = int_out_mye_path,
    int_in_mye_path = int_in_mye_path,
    dom_out_mye_path = dom_out_mye_path,
    dom_in_mye_path = dom_in_mye_path,
    upc_mye_path = upc_mye_path,
    popn_adjustment_path = popn_adjustment_path,
    external_births_path = external_births_path,
    external_deaths_path = external_deaths_path,
    
    mortality_rates = mortality_rates,
    fertility_rates = fertility_rates,
    int_out_flows_rates = int_out_flows_rates,
    int_out_method = int_out_flow_or_rate,
    int_in_flows = int_in,
    domestic_rates = domestic_rates,
    constraint_fns = constraint_fns,
    
    communal_est_pop_path = communal_est_pop_path,
    ons_stage1_file_path = ons_stage1_file_path,
    ons_stage2_file_path = ons_stage2_file_path,
    dclg_stage1_file_path = dclg_stage1_file_path,
    dclg_stage2_file_path = dclg_stage2_file_path,
    
    qa_areas_of_interest = FALSE,
    write_QA = FALSE,
    write_excel = write_excel,
    geog_code_col = geog_code_col
  )
  
  return(config_list)
}
