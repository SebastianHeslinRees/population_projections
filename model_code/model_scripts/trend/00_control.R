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
                       "n_dom_averaging_yr",
                       "popn_mye_path", 
                       "deaths_mye_path", 
                       "births_mye_path", 
                       "int_out_mye_path", 
                       "int_in_mye_path",
                       "dom_out_mye_path",
                       "dom_in_mye_path",
                       "dom_origin_destination_path",
                       "dom_rate_fns",
                       "outputs_dir", 
                       "mortality_fns", 
                       "fertility_fns",
                       "int_out_rate_fns",
                       "int_in_fns",
                       "qa_areas_of_interest", 
                       "timestamp")
 
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  
  # get the MYEs
  # TODO: should this be done all together?
  population <- get_component(filepath = config_list$popn_mye_path, 
                           max_yr = config_list$first_proj_yr - 1)
  
  deaths <- get_component(filepath = config_list$deaths_mye_path, 
                                max_yr = config_list$first_proj_yr - 1)
  
  births <- get_component(filepath = config_list$births_mye_path,
                                  max_yr = config_list$first_proj_yr - 1)
  
  int_out <- get_component(filepath = config_list$int_out_mye_path,
                          max_yr = config_list$first_proj_yr - 1)
  
  int_in <- get_component(filepath = config_list$int_in_mye_path,
                          max_yr = config_list$first_proj_yr - 1)
  
  # TODO: sooooo these MYE dom_out/dom_in don't match the ONS flows below. Might want to think about why.
#  dom_out <- get_component(filepath = config_list$dom_out_mye_path,
#                          max_yr = config_list$first_proj_yr - 1)
  
#  dom_in <- get_component(filepath = config_list$dom_in_mye_path,
#                          max_yr = config_list$first_proj_yr - 1)
  
#  dom_origin_dest <- readRDS(config_list$dom_origin_destination_path)

  
  # TODO: check that deaths and births have same geography, age, and sex coverage as population
  
  
  # get the projected rates
  # strings together 'building blocks' which can be swapped out and replaced in config file
  fertility <- evaluate_fns_list(config_list$fertility_fns) %>%
    complete_fertility(population)
    
  mortality <- evaluate_fns_list(config_list$mortality_fns)
  
  int_out_rate <- evaluate_fns_list(config_list$int_out_rate_fns) 

  int_in_proj <- evaluate_fns_list(config_list$int_in_fns)
  
  dom_rate <- evaluate_fns_list(config_list$dom_rate_fns)
  
  
  # TODO work out how to handle this better.  For now strip out everything from components dfs to make joining safer
  # TODO is this the best way to handle the Wales problem
  population <- population %>% select(year, gss_code, age, sex, popn)
  deaths <- deaths %>% select(year, gss_code, age, sex, deaths)
  births <- births %>% select(year, gss_code, age, sex, births)
  int_out <- int_out %>% select(year, gss_code, age, sex, int_out)
  int_in <- int_in %>% select(year, gss_code, age, sex, int_in)
  
  fertility <- fertility %>% select(year, gss_code, age, sex, rate)
  mortality <- mortality %>% select(year, gss_code, age, sex, rate)
  int_out_rate <- int_out_rate %>% select(year, gss_code, age, sex, rate)
  int_in_proj <- int_in_proj %>% select(year, gss_code, age, sex, int_in)

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
                           dom_historic, dom_rate,
                           config_list$first_proj_yr, config_list$n_proj_yr)
  
  ## write the output data
  output_projection(projection, config_list$outputs_dir, timestamp = config_list$timestamp)
  
  ## output the QA
  # TODO: is this the right place to call the QA? The QA might be changed more often than the rest of the model code. 
  rmarkdown::render("model_code/qa/population_qa.Rmd",
                    output_file = paste0("population_qa",config_list$timestamp,".html"),
                    output_dir = config_list$outputs_dir,
                    params = list(qa_areas_of_interest = config_list$qa_areas_of_interest,
                                  popn_proj_fp =   paste0(config_list$outputs_dir,"/population",config_list$timestamp,".rds"),
                                  deaths_proj_fp = paste0(config_list$outputs_dir,"/deaths",config_list$timestamp,".rds"),
                                  births_proj_fp = paste0(config_list$outputs_dir,"/births",config_list$timestamp,".rds"),
                                  output_files_dir = paste0(config_list$outputs_dir,"population_qa",config_list$timestamp,"_files/"),
                                  first_proj_yr = config_list$first_proj_yr))
}
