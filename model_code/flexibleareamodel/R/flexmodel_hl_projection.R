#' Run the small area housing-led model
#' 
#' Given a set of input parameters run a housing-led for small areas
#' 
#' @param config_list A List. A housing-led model configuration list.
#' @param n_cores An integer describing the number of CPU cores to use
#' 
#' @import dplyr
#' @import popmodules
#' @importFrom loggr log_file
#' @import parallel
#' @import doParallel
#'  
#' @export

#-------------------------------------------------------------------------------

#### Constraining ####
# There is constraining in the trend module. This will constrain one or more components
# or the total population to an external previously run borough-level projection

# There is no constraining in the housing-led module at the moment.
# The housing-led uses the births and deaths from the trend module so
# if there is constraining at that step it is passed through

#-------------------------------------------------------------------------------

flexmodel_hl_projection <- function(config_list, n_cores = NULL){
  
  expected_config <- c("projection_name",
                       "first_proj_yr",
                       "n_proj_yr",
                       "output_dir",
                       "population_path",
                       "deaths_path",
                       "births_path",
                       "out_migration_path",
                       "in_migration_path",
                       "mortality_rates",
                       "fertility_rates",
                       "in_migration",
                       "out_migration",
                       "constraint_list",
                       "communal_est_path",
                       "dev_trajectory_path", 
                       "ldd_backseries_path",
                       "dwellings_to_households_path",
                       "hhr_path",
                       "ahs_mix",
                       "hhr_static_or_projected",
                       "lookup_path",
                       "excess_deaths_path",
                       "geog_code_col",
                       "geog_name_col")
  
  validate_config_list(config_list, expected_config)
  
  #initalize the model outputs and log file
  message(config_list$projection_name)
  config_list$output_dir <- .add_slash(config_list$output_dir)
  dir.create(config_list$output_dir, recursive = T, showWarnings = F)
  loggr::log_file(paste0(config_list$output_dir,"warnings.log"))
  write_model_config(config_list)
  
  #browser()
  #-------------------------------------------------------------------------------
  
  #Validate paths
  .validate_input_paths(config_list)
  
  #------------------------------------------------------------------------------
  # Parallel processing
  detected_cores <- detectCores()
  if(is.null(n_cores)){n_cores <- detected_cores}
  if(n_cores > detected_cores){n_cores <- detected_cores}
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  #-----------------------------------------------------------------------------
  
  #projection years
  first_proj_yr <- config_list$first_proj_yr
  last_proj_yr <-  first_proj_yr + config_list$n_proj_yr -1
  lookup <- readRDS(config_list$lookup_path) %>% .standardise_df(config_list$geog_code_col)
  
  #Get backseries - 5 secs
  message("get backseries")
  population <- get_component_from_file(config_list$population_path, config_list$first_proj_yr - 1) %>%
    .standardise_df(config_list$geog_code_col, "popn")
  
  deaths <- get_component_from_file(config_list$deaths_path, config_list$first_proj_yr - 1) %>% 
    .standardise_df(config_list$geog_code_col, "deaths")
  
  births <- get_component_from_file(config_list$births_path, config_list$first_proj_yr - 1) %>% 
    .standardise_df(config_list$geog_code_col, "births") %>%
    filter(age == 0)
  
  in_migration <- get_component_from_file(config_list$in_migration_path, config_list$first_proj_yr - 1) %>% 
    .standardise_df(config_list$geog_code_col, "inflow")
  
  out_migration <- get_component_from_file(config_list$out_migration_path, config_list$first_proj_yr - 1) %>% 
    .standardise_df(config_list$geog_code_col, "outflow")
  
 
    # get the projected rates - 10 secs
  mortality_rates <- get_component_from_file(config_list$mortality_rates, last_proj_yr) %>% 
    .standardise_df(config_list$geog_code_col, "rate")
  
  fertility_rates <- get_component_from_file(config_list$fertility_rates, last_proj_yr) %>% 
    .standardise_df(config_list$geog_code_col, "rate")
  
  in_flow_info <- get_rates_flows_info(config_list$in_migration, first_proj_yr, last_proj_yr)
  in_migration_flows <- NULL
  
  out_rate_info <- get_rates_flows_info(config_list$out_migration, first_proj_yr, last_proj_yr)
  out_migration_rates <- NULL
  
  if(!is.null(config_list$excess_deaths_path)){
    excess_deaths <- get_component_from_file(filepath = config_list$excess_deaths_path, 
                                             max_yr = last_proj_yr)
  } else {
    excess_deaths <- NULL
  }
  
  # Constraints - 30 secs
  if(!is.null(config_list$constraint_list)){
    message("get constraints")
    constraint_list <- get_constraints(config_list$constraint_list, last_proj_yr)
  }
  #-------------------------------------------------------------------------------
  
  # Housing-led-specific stuff
  household_rep_rates <- readRDS(config_list$hhr_path) %>%
    .standardise_df(config_list$geog_code_col, "hh_rep_rate" , col_agg = c("year", config_list$geog_code_col, "age_group", "sex"))
  
  communal_establishment_population <- readRDS(config_list$communal_est_path) %>%
    .standardise_df(config_list$geog_code_col, "ce_popn")
  
  trajectory <- readRDS(config_list$dev_trajectory_path) %>% .standardise_df(config_list$geog_code_col, "units")
  
  dwellings <- trajectory %>% 
    arrange(area_code, year) %>% 
    group_by(area_code) %>% 
    mutate(units = cumsum(units)) %>% 
    data.frame()
  
  #Convert to dwellings to households
  dwellings_2_hh <- readRDS(config_list$dwellings_to_households_path) %>%  .standardise_df(config_list$geog_code_col, "d2hh_ratio")
  households <- dwellings %>%
    left_join(dwellings_2_hh, by="area_code") %>% 
    mutate(households = units * d2hh_ratio) %>% 
    select(-units, -d2hh_ratio)
  
  #Output lists
  hl_projection <- list()
  trend_projection <- list()
  
  #-------------------------------------------------------------------------------
  
  #Run projection - 2 mins
  
  curr_yr_popn <- filter(population, year == first_proj_yr-1)
  hhr_ahs_uplift <- NULL
  curr_yr_hhr <- filter(household_rep_rates, year == 2011)
  
  message("projecting")
  for(projection_year in first_proj_yr:last_proj_yr){
    browser()
    #fertility & mortality
    curr_yr_fertility <- filter(fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(mortality_rates, year == projection_year)
    
    #in migration
    in_migration_flows <- get_rates_or_flows(in_migration_flows, in_flow_info,
                                             projection_year, first_proj_yr,
                                             col_aggregation = c("year", "gss_code", config_list$geog_code_col, "sex", "age"),
                                             data_col = "in_flow") %>% 
      .standardise_df(config_list$geog_code_col, data_col = c("value_2","value_1","in_flow"))
    
    
    curr_yr_in_flows <- filter(in_migration_flows, year == projection_year) %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, area_code, sex, age, in_flow)
    
    #out migration
    out_migration_rates <- get_rates_or_flows(out_migration_rates, out_rate_info,
                                              projection_year, first_proj_yr,
                                              col_aggregation = c("year", "gss_code", config_list$geog_code_col, "sex", "age"),
                                              data_col = "out_rate") %>% 
      .standardise_df(config_list$geog_code_col, data_col = c("value_2","value_1","out_rate"))
    
    curr_yr_out_rates <- filter(out_migration_rates, year == projection_year) %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, area_code, sex, age, out_rate)
    
    #households
    curr_yr_households <- filter(households, year == projection_year)
    
    if(config_list$hhr_static_or_projected == "projected"){
      curr_yr_hhr <- filter(household_rep_rates, year == projection_year)
    } else {
      curr_yr_hhr <- curr_yr_hhr %>% mutate(year = projection_year)
    }
    
    
    if(!is.null(excess_deaths) & projection_year %in% excess_deaths$year){
      curr_yr_excess_deaths <- filter(excess_deaths, year == projection_year)
    } else {
      curr_yr_excess_deaths <- NULL
    }
    
    #project
    trend_projection[[projection_year]] <- trend_core(start_population = curr_yr_popn,
                                                      fertility_rates = curr_yr_fertility,
                                                      mortality_rates = curr_yr_mortality,
                                                      out_rates = curr_yr_out_rates,
                                                      in_flows = curr_yr_in_flows,
                                                      projection_year = projection_year,
                                                      constraint_list = constraint_list,
                                                      excess_deaths = curr_yr_excess_deaths)
    
    hl_projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                         trend_projection = trend_projection[[projection_year]],
                                                         communal_establishment_population = communal_establishment_population,
                                                         household_rep_rates = curr_yr_hhr,
                                                         households = curr_yr_households,
                                                         projection_year = projection_year,
                                                         ahs_mix = config_list$ahs_mix,
                                                         hhr_ahs_uplift = hhr_ahs_uplift,
                                                         constraint_list = constraint_list,
                                                         n_cores,
                                                         lookup)
    
    curr_yr_popn <- hl_projection[[projection_year]]$population
    hhr_ahs_uplift <- hl_projection[[projection_year]]$hhr_ahs_uplift
    
  }
  
  rm(list = setdiff(ls(), c("hl_projection",
                            "population", "births", "deaths",
                            "in_migration", "out_migration",
                            "fertility_rates", "mortality_rates",
                            "in_migration_flows", "out_migration_rates",
                            "trajectory", "dwellings",
                            "first_proj_yr", "last_proj_yr",
                            "config_list","n_cores","cl")))
  
  #-------------------------------------------------------------------------------
  
  #Arrange - 4 secs
  message('')
  message("arrange outputs")
  hl_projection <- arrange_flexmodel_outputs(hl_projection,
                                              population, births, deaths,
                                              in_migration, out_migration,
                                              fertility_rates, mortality_rates,
                                              in_migration_flows,
                                              out_migration_rates,
                                              trajectory, dwellings,
                                              first_proj_yr, last_proj_yr,
                                              config_list,
                                              "housing-led",
                                              n_cores)
  
  rm(list=setdiff(ls(), c("hl_projection","config_list","cl")))
  #-------------------------------------------------------------------------------
  
  #Output - 2 mins
  output_flexmodel_projection(hl_projection, output_dir = config_list$output_dir, "housing-led", config_list)
  
  #Close log
  message("complete")
  parallel::stopCluster(cl)
  deactivate_log(paste0(config_list$output_dir, "warnings.log"))
  
  return(hl_projection)
  
}




