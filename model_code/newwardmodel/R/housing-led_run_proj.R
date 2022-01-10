#' Run the small area housing-led model
#' 
#' Given a set of input parameters run a housing-led for small areas
#' 
#' @param config_list A List. A housing-led model configuration list.
#' 
#' @import dplyr
#' @import popmodules
#' @importFrom loggr log_file
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

run_small_area_hl_model <- function(config_list){
  
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
                       "hhr_path",
                       "ahs_mix",
                       "hhr_static_or_projected")
  
  validate_config_list(config_list, expected_config)
  
  #initalize the model outputs and log file
  message(config_list$projection_name)
  config_list$output_dir <- .add_slash(config_list$output_dir)
  dir.create(config_list$output_dir, recursive = T, showWarnings = F)
  loggr::log_file(paste0(config_list$output_dir,"warnings.log"))
  write_model_config(config_list)
  
  #-------------------------------------------------------------------------------
  
  # TODO validate paths
  # validate_input_paths(config_list)
  
  #projection years
  first_proj_yr <- config_list$first_proj_yr
  last_proj_yr <-  first_proj_yr + config_list$n_proj_yr -1
  
  #Get backseries - 5 secs
  message("get backseries")
  population <- get_component_from_file(filepath = config_list$population_path, 
                                        max_yr = config_list$first_proj_yr - 1) %>%
    select(year, gss_code, gss_code_ward, age, sex, popn)
  
  deaths <- get_component_from_file(filepath = config_list$deaths_path,
                                    max_yr = config_list$first_proj_yr - 1) 
  
  births <- get_component_from_file(filepath = config_list$births_path,
                                    max_yr = config_list$first_proj_yr - 1)
  
  in_migration <- get_component_from_file(filepath = config_list$in_migration_path,
                                          max_yr = config_list$first_proj_yr - 1) 
  
  out_migration <- get_component_from_file(filepath = config_list$out_migration_path,
                                           max_yr = config_list$first_proj_yr - 1)
  
  
  # #Prep backseries
  population <- population %>% select(year, gss_code, gss_code_ward, age, sex, popn)
  deaths <- deaths %>% select(year, gss_code, gss_code_ward, age, sex, deaths)
  births <- births %>% select(year, gss_code, gss_code_ward, age, sex, births) %>% filter(age == 0)
  out_migration <- out_migration %>% select(year, gss_code, gss_code_ward, age, sex, outflow)
  in_migration <- in_migration %>% select(year, gss_code, gss_code_ward, age, sex, inflow)
  
  # get the projected rates - 10 secs
  mortality_rates <- get_component_from_file(filepath = config_list$mortality_rates, 
                                             max_yr = last_proj_yr)
  
  fertility_rates <- get_component_from_file(filepath = config_list$fertility_rates, 
                                             max_yr = last_proj_yr)
  
  in_flow_info <- get_rates_flows_info(config_list$in_migration, first_proj_yr, last_proj_yr)
  in_migration_flows <- NULL
  
  out_rate_info <- get_rates_flows_info(config_list$out_migration, first_proj_yr, last_proj_yr)
  out_migration_rates <- NULL
  
  
  # Constraints - 30 secs
  if(!is.null(constraint_list)){
    message("get constraints")
    constraint_list <- get_constraints(constraint_list, last_proj_yr)
  }
  #-------------------------------------------------------------------------------
  
  # Housing-led-specific stuff
  household_rep_rates = readRDS(config_list$hhr_path)
  communal_establishment_population <- readRDS(config_list$communal_est_path)
  
  # Get the dwelling trajectory
  dwellings <- readRDS("input_data/new_ward_model/development_data/ldd_backseries_dwellings_ward_WD20CD.rds") %>% 
    select(year, gss_code_ward, units) %>% 
    filter(year == 2011) %>% 
    rbind(
      readRDS(config_list$dev_trajectory_path)) %>% 
    arrange(gss_code_ward, year) %>% 
    group_by(gss_code_ward) %>% 
    mutate(units = cumsum(units)) %>% 
    data.frame()
  
  #Convert to dwellings to households
  dwelling_2_hh <- readRDS(paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD20CD.rds")) 
  households <- dwellings %>%
    left_join(dwelling_2_hh, by="gss_code_ward") %>% 
    mutate(households = units * d2hh_ratio) %>% 
    select(-units, -d2hh_ratio)
  
  #Output lists
  hl_projection <- list()
  trend_projection <- list()
  
  #-------------------------------------------------------------------------------
  
  #Run projection - 2 mins
  
  curr_yr_popn <- filter(population, year == first_proj_yr-1)
  
  for(projection_year in first_proj_yr:last_proj_yr){
    
    #fertility & mortality
    curr_yr_fertility <- filter(fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(mortality_rates, year == projection_year)
    
    #in migration
    in_migration_flows <- get_rates_or_flows(in_migration_flows, in_flow_info,
                                             projection_year, first_proj_yr,
                                             col_aggregation = c("year", "gss_code", "gss_code_ward", "sex", "age"),
                                             data_col = "in_flow")
    
    
    curr_yr_in_flows <- filter(in_migration_flows, year == projection_year) %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, gss_code_ward, sex, age, in_flow)
    
    #out migration
    out_migration_rates <- get_rates_or_flows(out_migration_rates, out_rate_info,
                                              projection_year, first_proj_yr,
                                              col_aggregation = c("year", "gss_code", "gss_code_ward", "sex", "age"),
                                              data_col = "out_rate")
    
    curr_yr_out_rates <- filter(out_migration_rates, year == projection_year) %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, gss_code_ward, sex, age, out_rate)
    
    #households
    curr_yr_households <- filter(households, year == projection_year)
    
    if(config_list$hhr_static_or_projected == "projected"){
      curr_yr_hhr <- filter(household_rep_rates, year == projection_year)
    } else {
      curr_yr_hhr <- filter(household_rep_rates, year == 2011) %>% 
        mutate(year = projection_year)
    }
    
    #project
    trend_projection[[projection_year]] <- trend_core(start_population = curr_yr_popn,
                                                      fertility_rates = curr_yr_fertility,
                                                      mortality_rates = curr_yr_mortality,
                                                      out_rates = curr_yr_out_rates,
                                                      in_flows = curr_yr_in_flows,
                                                      projection_year = projection_year,
                                                      constraint_list = constraint_list)
    
    hl_projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                         trend_projection = trend_projection[[projection_year]],
                                                         communal_establishment_population = communal_establishment_population,
                                                         household_rep_rates = curr_yr_hhr,
                                                         households = curr_yr_households,
                                                         projection_year = projection_year,
                                                         ahs_mix = config_list$ahs_mix)
    
    
    curr_yr_popn <- hl_projection[[projection_year]]$population
    
  }
  
  #-------------------------------------------------------------------------------
  
  #Arrange - 4 secs
  message('')
  message("arrange outputs")
  hl_projection <- arrange_housing_led_outputs(hl_projection,
                                               population, births, deaths,
                                               in_migration, out_migration,
                                               fertility_rates, mortality_rates,
                                               in_migration_flows,
                                               out_migration_rates,
                                               first_proj_yr, last_proj_yr)
  
  #-------------------------------------------------------------------------------
  
  #Output - 2 mins
  output_housing_led(hl_projection, output_dir = config_list$output_dir)
  
}

