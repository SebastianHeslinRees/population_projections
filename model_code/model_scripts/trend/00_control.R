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
  source("model_code/model_scripts/trend/01_get_inputs.R")
  source("model_code/model_scripts/trend/02_core.R")
  source("model_code/model_scripts/trend/03_output.R")
  
  # check that the config_list contains expected variables 
  # TODO: change this to look for a config template file?
  expected_config <- c("first_proj_yr", "n_proj_yr", "popn_mye_path", "deaths_mye_path", "outputs_dir", "mortality_fns", "timestamp")
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  
  # get the MYEs
  # TODO: should this be done all together?
  popn_mye <- get_mye_component(filepath = config_list$popn_mye_path, 
                           max_yr = config_list$first_proj_yr - 1)
  
  deaths_mye <- get_mye_component(filepath = config_list$de, 
                                max_yr = config_list$first_proj_yr - 1)

  
  # get the projected rates
  # strings together 'building blocks' which can be swapped out and replaced in config file
  mortality <- evaluate_fns_list(config_list$mortality_fns)
  

  ## by here we need the inputs to the core. These are:
  
  # start year
  
  # population estimates
  # deaths estimates
  # mortality rates
  # births estimates
  # fertility estimates
  # male:female births ratio
  # international out migration estimates
  # Age specific out migration probabilities
  # international in migration estimates
  # domestic out migration
  # domestic in migration
  # domestic net migration
  # domestic migration rates matrix
  
  ## run the core
  projection <- trend_core(popn_mye, mortality, config_list$n_proj_yr)
  
  ## write the output data
  output_projection(projection, config_list$outputs_dir, timestamp = config_list$timestamp)
  
  ## output the QA
  # TODO: is this the right place to call the QA? The QA might be changed more often than the rest of the model code. 
  rmarkdown::render("model_code/qa/population_qa.Rmd",
                    output_file = paste0("population_qa",config_list$timestamp,".html"),
                    params = list(popn_proj_fp =   paste0(config_list$outputs_dir,"/population",config_list$timestamp,".rds"),
                                  deaths_proj_fp = paste0(config_list$outputs_dir,"/deaths",config_list$timestamp,".rds")))
  
  # move the QA output. Output is not writtedn here directly because of compications in referencing the gif file from pandoc notebook. See https://github.com/rstudio/rmarkdown/issues/587#issuecomment-168437646
  file.rename(paste0("model_code/qa/population_qa",config_list$timestamp,".html"), paste0(config_list$outputs_dir,"/population_qa",config_list$timestamp,".html")) %>% invisible()
  file.remove("model_code/qa/ldn_age_by_yr.gif") %>% invisible()
}
