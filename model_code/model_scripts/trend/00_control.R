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
  expected_config <- c("first_proj_yr", "n_proj_yr", "popn_mye_path", "deaths_mye_path", "births_mye_path", "outputs_dir", "mortality_fns", "fertility_fns", "qa_areas_of_interest", "timestamp")
 
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  
  # get the MYEs
  # TODO: should this be done all together?
  population <- get_component(filepath = config_list$popn_mye_path, 
                           max_yr = config_list$first_proj_yr - 1)
  
  deaths <- get_component(filepath = config_list$deaths_mye_path, 
                                max_yr = config_list$first_proj_yr - 1)
  
  births <- get_component(filepath = config_list$births_mye_path,
                                  max_yr = config_list$first_proj_yr - 1)
  
  # TODO: check that deaths and births have same geography, age, and sex coverage as population
  
  
  # get the projected rates
  # strings together 'building blocks' which can be swapped out and replaced in config file
  fertility <- evaluate_fns_list(config_list$fertility_fns) %>%
    complete_fertility(population)
    
  #browser()
  mortality <- evaluate_fns_list(config_list$mortality_fns)
  
  # TODO work out how to handle this better.  For now strip out everything from components dfs to make joining safer
  # TODO is this the best way to handle the Wales problem
  population <- population %>% select(year, gss_code, age, sex, popn) %>%
    popmodules::consolidate_national_data("popn")
  deaths <- deaths %>% select(year, gss_code, age, sex, deaths)%>%
    popmodules::consolidate_national_data("deaths")
  births <- births %>% select(year, gss_code, age, sex, births)%>%
    popmodules::consolidate_national_data("births")
  fertility <- fertility %>% select(year, gss_code, age, sex, rate)
  mortality <- mortality %>% select(year, gss_code, age, sex, rate)

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
  projection <- trend_core(population, births, deaths, fertility, mortality, config_list$first_proj_yr, config_list$n_proj_yr)
  
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
                                  output_files_dir = paste0(config_list$outputs_dir,"population_qa",config_list$timestamp,"_files/")))
}
