library(data.table)
library(dplyr)
library(assertthat)
devtools::load_all('model_code/popmodules')

source("model_code/newwardmodel/trend_loop.R")
source("model_code/newwardmodel/trend_arrange_outputs.R")
source("model_code/newwardmodel/trend_outputs.R")
source("model_code/newwardmodel/get_constraints.R")

#-------------------------------------------------------------------------------

run_new_ward_model <- function(config_list){
  
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
                       "constraint_list")
  
  validate_config_list(config_list, expected_config)
  
  #-------------------------------------------------------------------------------
  #initalize the model outputs and log file
  message(config_list$projection_name)
  config_list$output_dir <- .add_slash(config_list$output_dir)
  dir.create(config_list$output_dir, recursive = T, showWarnings = F)
  loggr::log_file(paste0(config_list$output_dir,"warnings.log"))
  write_model_config(config_list)
  
  # validate paths
  validate_input_paths(config_list)
  
  #projection years
  first_proj_yr <- config_list$first_proj_yr
  last_proj_yr <-  first_proj_yr + config_list$n_proj_yr -1
  
  #Get backseries - 5 secs
  message("get backseries")
  population <- get_component_from_file(filepath = config_list$population_path, 
                                        max_yr = config_list$first_proj_yr - 1)
  
  
  deaths <- get_component_from_file(filepath = config_list$deaths_path,
                                    max_yr = config_list$first_proj_yr - 1) 
  
  births <- get_component_from_file(filepath = config_list$births_path,
                                    max_yr = config_list$first_proj_yr - 1)
  
  in_migration <- get_component_from_file(filepath = config_list$in_migration_path,
                                          max_yr = config_list$first_proj_yr - 1) 
  
  out_migration <- get_component_from_file(filepath = config_list$out_migration_path,
                                           max_yr = config_list$first_proj_yr - 1) 
  
  
  # if(!is.null(config_list$upc_mye_path)){
  #   upc_mye <- readRDS(config_list$upc_mye_path)
  # } else {
  #   upc_mye <- NULL
  # }
  # 
  # if(!is.null(config_list$popn_adjustment_path)){
  #   popn_adjustment <- readRDS(config_list$popn_adjustment_path)
  # } else {
  #   popn_adjustment <- NULL
  # }
  # 
  # if(!is.null(config_list$external_births_path)){
  #   external_births <-  readRDS(config_list$external_births_path)
  # } else {
  #   external_births <- NULL
  # }
  # 
  # 
  # if(!is.null(config_list$external_deaths_path)){
  #   external_deaths <- readRDS(config_list$external_deaths_path)
  # } else {
  #   external_deaths <- NULL
  # }
  
  # #Prep backseries
  population <- population %>% select(year, gss_code, gss_code_ward, age, sex, popn)
  deaths <- deaths %>% select(year, gss_code, gss_code_ward, age, sex, deaths)
  births <- births %>% select(year, gss_code, gss_code_ward, age, sex, births) %>% filter(age == 0)
  out_migration <- out_migration %>% select(year, gss_code, gss_code_ward, age, sex, outflow)
  in_migration <- in_migration %>% select(year, gss_code, gss_code_ward, age, sex, inflow)
  
  # get the projected rates - 10 secs
  message("get projected rates")
  
  mortality_rates <- get_component_from_file(filepath = config_list$mortality_rates, 
                                             max_yr = last_proj_yr)
  
  fertility_rates <- get_component_from_file(filepath = config_list$fertility_rates, 
                                             max_yr = last_proj_yr)
  
  # projected_in_migration <- get_component_from_file(filepath = config_list$in_migration, 
  #                                         max_yr = last_proj_yr)
  # 
  # projected_out_migration <- get_component_from_file(filepath = config_list$out_migration, 
  #                                          max_yr = last_proj_yr)
  
  in_flow_info <- get_rates_flows_info(config_list$in_migration, first_proj_yr, last_proj_yr)
  projected_in_migration <- NULL
  
  out_rate_info <- get_rates_flows_info(config_list$out_migration, first_proj_yr, last_proj_yr)
  projected_out_migration <- NULL
  
  
  #Constraints - 30 secs
  if(!is.null(constraint_list)){
    message("get constraints")
    constraint_list <- get_constraints(constraint_list, last_proj_yr)
  }
  
  
  # # set up projection
  
  # validate_trend_core_inputs(population, births, deaths, int_out, int_in,
  #                            dom_out, dom_in, popn_adjustment, upc_mye,
  #                            fertility_rates, mortality_rates,
  #                            external_births,
  #                            external_deaths,
  #                            int_out_flows_rates,
  #                            first_proj_yr, config_list$n_proj_yr,
  #                            config_list$int_out_method)
  
  projection <- list()
  curr_yr_popn <- filter(population, year == first_proj_yr-1)
  
  # 2 seconds per year, 5 when constraining
  for(projection_year in first_proj_yr:last_proj_yr){
    
    curr_yr_fertility <- filter(fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(mortality_rates, year == projection_year)
    
    projected_in_migration <- get_rates_or_flows(projected_in_migration, in_flow_info,
                                           projection_year, first_proj_yr,
                                           col_aggregation = c("year", "gss_code", "gss_code_ward", "sex", "age"),
                                           data_col = "in_flow")
    
    
    curr_yr_in_flows <- filter(projected_in_migration, year == projection_year) %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, gss_code_ward, sex, age, in_flow)
    
    projected_out_migration <- get_rates_or_flows(projected_out_migration, out_rate_info,
                                                 projection_year, first_proj_yr,
                                                 col_aggregation = c("year", "gss_code", "gss_code_ward", "sex", "age"),
                                                 data_col = "out_rate")
    
    curr_yr_out_rates <- filter(projected_out_migration, year == projection_year) %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, gss_code_ward, sex, age, out_rate)

    projection[[projection_year]] <- projection_loop(start_population = curr_yr_popn,
                                                     fertility_rates = curr_yr_fertility,
                                                     mortality_rates = curr_yr_mortality,
                                                     out_rates = curr_yr_out_rates,
                                                     in_flows = curr_yr_in_flows,
                                                     projection_year = projection_year,
                                                     constraint_list = constraint_list)
    
    curr_yr_popn <- projection[[projection_year]]$population
  }
  
  
  #Arrange - 4 secs
  message('')
  message("arrange outputs")
  projection <- arrange_core_outputs(projection,
                                     population, births, deaths,
                                     in_migration, out_migration,
                                     fertility_rates, mortality_rates,
                                     projected_in_migration,
                                     projected_out_migration,
                                     first_proj_yr, last_proj_yr)
  
  #Output - 60 secs
  message("write outputs")
  output_projection(projection,
                    config_list$output_dir)
  
  #Close log
  message("complete")
  deactivate_log(paste0(config_list$output_dir,"warnings.log"))
  
  return(projection)
  
}


validate_input_paths <- function(config_list){
  
  paths <- c("population_path",
             "deaths_path",
             "births_path",
             "out_migration_path",
             "in_migration_path",
             "mortality_rates",
             "fertility_rates")
  
  paths <- config_list[names(config_list) %in% paths]
  
  lapply(seq(paths),
         function(i) {
           if(!is.null(paths[[i]])){
             assert_that(file.exists(paths[[i]]),
                         msg = paste0(names(paths)[[i]], ": ", paths[[i]], "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(paths[[i]], split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(names(paths)[[i]], ": ", paths[[i]], "\nFile is not .rds format"))
           }
         })
  
  lapply(config_list$out_migration,
         function(x) {
           assert_that(file.exists(x$path),
                       msg = paste0(x$path, "\nFile does not exist at specified path"))
           
           file_ext <- tolower(strsplit(x$path, split="\\.")[[1]][[2]])
           assert_that(file_ext == "rds",
                       msg = paste0(x$path, "\nFile is not .rds format"))
         })
  
  lapply(config_list$in_migration,
         function(x) {
           assert_that(file.exists(x$path),
                       msg = paste0(x$path, "\nFile does not exist at specified path"))
           
           file_ext <- tolower(strsplit(x$path, split="\\.")[[1]][[2]])
           assert_that(file_ext == "rds",
                       msg = paste0(x$path, "\nFile is not .rds format"))
         })
  
  if(!is.null(constraint_list$constraint_path)){
    assert_that(dir.exists(config_list$constraint_list$constraint_path),
                msg = "constraint folder does not exist at specified path")
  }
  
  invisible()
}
