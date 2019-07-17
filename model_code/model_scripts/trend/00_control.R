run_trend_model <- function(config_list) {
# input args:
  # start_year (start_year = 2018)
  # popn file path (popn_mye_path ="input_data/mye/2018/population_ons_2019-07-05.rds")
  # n_proj_year / end_year
  # data source file paths - popn_mye, births_mye, deaths_mye
  # mortality modules list with args and outputs?
  # fertility modules list with args and outputs?
  # (migration modules - worry about these later)
  
  
  library(tidyverse)
  source("model_code/model_scripts/trend/01_get_inputs.R")
  source("model_code/model_scripts/trend/02_core.R")
  source("model_code/model_scripts/trend/03_output.R")
  
  # check that the config_list contains expected variables 
  # TODO: change this to look for a config template file
  expected_config <- c("start_yr", "n_proj_yr", "popn_mye_path", "outputs_dir")
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  
  ## get the input data
  # TODO: test that start year is in the mye
  popn_mye <- get_popn_mye(config_list$popn_mye_path) %>%
    filter(year <= config_list$start_yr)
  
  # ## prepare the core inputs
  # # this strings together 'building blocks' which can be swapped out and replaced to change the model
  # 
  # # TODO think about generalising creating the mortality/fertility/etc chain e.g. below
  # # for (n in length(mortality_modules_list)) {
  # #   out_n <- mort_function_n(args_n)
  # # }
  # 
  # # get the jump-off estimates
  # popn_mye <- filter(popn_mye, year <= start_year)
  # births_mye <- filter(births_mye, year <= start_year)
  # deaths_mye <- filter(deaths_mye, year <= start_year)
  # 
  # # calculate fertility
  # fertility <- fertility_from_births_by_mothers_age(mye_popn, SNPP_proj_births_by_ma) %>%
  #   smooth_fertility_hadwiger() %>%
  #   scale_fertility(mye_popn, mye_births) %>%
  #   project_fertility(NPP_2016_based)
  # 
  # # calculate mortality
  # mortality <- mortality_backseries_from_deaths_popn(mye_popn, mye_deaths) %>%
  #   calc_next_yr_mortality(backyears_mortality) %>%
  #   project_mortality(NPP_2016_based)
  # 
  
  # calculate age specific out migration probabilities
  
  # calculate domestic out migration
  
  # calculate domestic in migration
  
  # calculate domestic net migration
  
  # calculate domestic migration rates matrix
  
  
  
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
  projection <- trend_core(popn_mye, config_list$n_proj_yr)
  
  ## write the output data
  output_projection(projection, config_list$outputs_dir)
  
  ## output the QA
  
}