run_trend_model <- function(config_list) {
  # input args:
  # first_proj_yr 
  # popn file path
  # deaths file path
  # n_proj_year
  # data source file paths - popn_mye, births_mye, deaths_mye
  # mortality modules list with args and outputs?
  # fertility modules list with args and outputs?
  # (migration modules - worry about these later)
  
  
  library(tidyverse)
  library(popmodules)
  source("model_code/model_scripts/trend/01_get_inputs.R")
  source("model_code/model_scripts/trend/02_core.R")
  source("model_code/model_scripts/trend/03_output.R")
  
  # check that the config_list contains expected variables 
  # TODO: change this to look for a config template file?
  expected_config <- c("first_proj_yr", 
                       "n_proj_yr",
                       "popn_mye_path", 
                       "deaths_mye_path", 
                       "births_mye_path", 
                       "int_out_mye_path", 
                       "int_in_mye_path",
                       "dom_out_mye_path",
                       "dom_in_mye_path",
                       "dom_origin_destination_path",
                       "upc_path",
                       "outputs_dir", 
                       "mortality_fns", 
                       "fertility_fns",
                       "int_out_rate_fns",
                       "int_in_fns",
                       "dom_rate_fns",
                       "constraint_fns",
                       "projection_name",
                       "write_excel",
                       "hh_rep_rates_path",
                       "communal_est_pop_path",
                       "stage1_file_path",
                       "stage2_file_path",
                       "qa_areas_of_interest", 
                       "timestamp")
  
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  # get the MYEs
  # TODO: should this be done all together?
  message("get components")
  population <- get_component(filepath = config_list$popn_mye_path, 
                              max_yr = config_list$first_proj_yr - 1)
  
  deaths <- get_component(filepath = config_list$deaths_mye_path, 
                          max_yr = config_list$first_proj_yr - 1)
  
  births <- get_component(filepath = config_list$births_mye_path,
                          max_yr = config_list$first_proj_yr - 1) %>%
    filter(age == 0)
  
  int_out <- get_component(filepath = config_list$int_out_mye_path,
                           max_yr = config_list$first_proj_yr - 1)
  
  int_in <- get_component(filepath = config_list$int_in_mye_path,
                          max_yr = config_list$first_proj_yr - 1)
  
  dom_out <- get_component(filepath = config_list$dom_out_mye_path,
                           max_yr = config_list$first_proj_yr - 1)
  
  dom_in <- get_component(filepath = config_list$dom_in_mye_path,
                          max_yr = config_list$first_proj_yr - 1)
  
  if(is.na(config_list$upc_path)){
    upc <- NULL
  } else { 
    upc <- readRDS(config_list$upc_path)
  }
  
  
  # get the projected rates
  # strings together 'building blocks' which can be swapped out and replaced in config file
  message("get projected rates")
  #browser()
  fertility <- evaluate_fns_list(config_list$fertility_fns) %>%
    complete_fertility(population)
  
  mortality <- evaluate_fns_list(config_list$mortality_fns)
  
  int_out_rate <- evaluate_fns_list(config_list$int_out_rate_fns) 
  
  int_in_proj <- evaluate_fns_list(config_list$int_in_fns)
  
  dom_rate <- evaluate_fns_list(config_list$dom_rate_fns)
  
  constraints <- evaluate_fns_list(config_list$constraint_fns)
  
  # TODO work out how to handle this better.  For now strip out everything from components dfs to make joining safer
  
  population <- population %>% select(year, gss_code, age, sex, popn)
  deaths <- deaths %>% select(year, gss_code, age, sex, deaths)
  births <- births %>% select(year, gss_code, age, sex, births)
  int_out <- int_out %>% select(year, gss_code, age, sex, int_out)
  int_in <- int_in %>% select(year, gss_code, age, sex, int_in)
  #TODO Figure out why the core outputs these in a different order to other components
  #For now just switch the order here
  dom_out <- dom_out %>% select(year, gss_code, sex, age, dom_out)
  dom_in <- dom_in %>% select(year, gss_code, sex, age, dom_in)
  
  
  fertility <- fertility %>% select(year, gss_code, age, sex, rate)
  mortality <- mortality %>% select(year, gss_code, age, sex, rate)
  int_out_rate <- int_out_rate %>% select(year, gss_code, age, sex, int_out)
  int_in_proj <- int_in_proj %>% select(year, gss_code, age, sex, int_in)
  dom_rate <- dom_rate %>% select(gss_out, gss_in, age, sex, rate)
  
  
  # TODO fix the fertility data so we don't have to do this
  if(any(fertility$rate > 1)) {
    warning("Setting mortality rates > 1 to be 1")
    fertility$rate <- sapply(fertility$rate, function(x) min(x,1))
  }
  if(any(fertility$rate < 0)) {
    warning("Setting mortality rates < 0 to be 0")
    fertility$rate <- sapply(fertility$rate, function(x) max(x,0))
  }
  
  ## run the core
  projection <- trend_core(population, births, deaths, int_out, int_in, 
                           fertility, mortality, int_out_rate, int_in_proj,
                           dom_in, dom_out, dom_rate,
                           config_list$first_proj_yr, config_list$n_proj_yr,
                           constraints, upc)
  
  ## household models
  message('running household models')
  ons_households <- ons_household_model(popn = projection$population,
                                        hh_rep_rates_path = config_list$hh_rep_rates_path,
                                        communal_est_pop_path = config_list$communal_est_pop_path)
  
  dclg_households <- dclg_household_model(population, config_list$stage1_file_path, config_list$stage2_file_path)
  
  
  ## write the output data
  message("running outputs")
  
  #TODO I'm moving this directory creation stuff here for now
  if (!grepl("/$", config_list$outputs_dir)) config_list$outputs_dir <- paste0(config_list$outputs_dir, "/")
  output_dir <- paste0(config_list$outputs_dir, config_list$projection_name,"/")
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  output_projection(projection, output_dir, timestamp = config_list$timestamp, write_excel = config_list$write_excel)
  ons_hh_model_outputs(ons_households, output_dir)
  dclg_hh_model_outputs(dclg_households, output_dir)
  
  ## output the QA
  # TODO: is this the right place to call the QA? The QA might be changed more often than the rest of the model code. 
  rmarkdown::render("model_code/qa/population_qa.Rmd",
                    output_file = paste0("population_qa",config_list$timestamp,".html"),
                    output_dir = output_dir,
                    params = list(qa_areas_of_interest = config_list$qa_areas_of_interest,
                                  popn_proj_fp =   paste0(output_dir,"/population",config_list$timestamp,".rds"),
                                  deaths_proj_fp = paste0(output_dir,"/deaths",config_list$timestamp,".rds"),
                                  int_in_proj_fp = paste0(output_dir,"/int_in",config_list$timestamp,".rds"),
                                  int_out_proj_fp = paste0(output_dir,"/int_out",config_list$timestamp,".rds"),
                                  dom_in_proj_fp = paste0(output_dir,"/dom_in",config_list$timestamp,".rds"),
                                  dom_out_proj_fp = paste0(output_dir,"/dom_out",config_list$timestamp,".rds"),
                                  births_proj_fp = paste0(output_dir,"/births",config_list$timestamp,".rds"),
                                  output_files_dir = paste0(output_dir,"population_qa",config_list$timestamp,"_files/"),
                                  first_proj_yr = config_list$first_proj_yr))
}
