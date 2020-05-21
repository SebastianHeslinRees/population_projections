run_trend_model <- function(config_list) {
  
  library(tidyverse)
  library(popmodules)
  source("model_code/model_scripts/trend/02_core.R")
  source("model_code/model_scripts/trend/03_output.R")
  source("model_code/model_scripts/trend/arrange_trend_core_outputs.R")
  source("model_code/model_scripts/trend/household_model_dclg.R")
  source("model_code/model_scripts/trend/household_model_ons.R")
  source("model_code/model_scripts/trend/household_model_outputs.R")
  source("model_code/model_scripts/trend/trend_datastore_outputs.R")
  source('model_code/model_scripts/trend/return_domestic_rates.R')
  
  region_lookup <- readRDS("input_data/lookup/district_to_region.rds")
  
  expected_config <- c("projection_name",
                       "first_proj_yr", 
                       "n_proj_yr",
                       "popn_mye_path", 
                       "deaths_mye_path", 
                       "births_mye_path", 
                       "int_out_mye_path", 
                       "int_in_mye_path",
                       "dom_out_mye_path",
                       "dom_in_mye_path",
                       "upc_path",
                       "output_dir", 
                       "mortality_fns", 
                       "fertility_fns",
                       "int_out_fns",
                       "int_in_fns",
                       "domestic_rates",
                       "constraint_fns",
                       "int_out_method",
                       "qa_areas_of_interest",
                       "write_excel",
                       "write_QA",
                       "ons_stage1_file_path",
                       "ons_stage2_file_path",
                       "communal_est_pop_path",
                       "dclg_stage1_file_path",
                       "dclg_stage2_file_path")
  
  validate_config_list(config_list, expected_config)
  
  #Create output directory
  dir.create(config_list$output_dir, recursive = T, showWarnings = F)
  
  #Validate file paths
  file_list <- config_list[stringr::str_detect(names(config_list), "path")]
  validate_paths <- lapply(seq(file_list),
                           function(i) {
                             if(!is.null(file_list[[i]])){
                               assertthat::assert_that(file.exists(file_list[[i]]),
                                                       msg = paste0(names(file_list)[[i]], ": ", file_list[[i]], "\nFile does not exist at specified path"))
                               file_ext <- tolower(strsplit(file_list[[i]], split="\\.")[[1]][[2]])
                               assertthat::assert_that(file_ext == "rds",
                                                       msg = paste0(names(file_list)[[i]], ": ", file_list[[i]], "\nFile is not .rds format"))
                             }
                           })
  rm(file_list, validate_paths)      
  
  #
  first_proj_yr <- config_list$first_proj_yr
  last_proj_yr <-  first_proj_yr + config_list$n_proj_yr -1
  
  # get the MYEs
  message("get components backseries")
  population <- get_component_from_file(filepath = config_list$popn_mye_path, 
                                        max_yr = config_list$first_proj_yr - 1)
  
  deaths <- get_component_from_file(filepath = config_list$deaths_mye_path, 
                                    max_yr = config_list$first_proj_yr - 1)
  
  births <- get_component_from_file(filepath = config_list$births_mye_path,
                                    max_yr = config_list$first_proj_yr - 1) %>%
    filter(age == 0)
  
  int_out <- get_component_from_file(filepath = config_list$int_out_mye_path,
                                     max_yr = config_list$first_proj_yr - 1)
  
  int_in <- get_component_from_file(filepath = config_list$int_in_mye_path,
                                    max_yr = config_list$first_proj_yr - 1)
  
  dom_out <- get_component_from_file(filepath = config_list$dom_out_mye_path,
                                     max_yr = config_list$first_proj_yr - 1)
  
  dom_in <- get_component_from_file(filepath = config_list$dom_in_mye_path,
                                    max_yr = config_list$first_proj_yr - 1)

  if(!is.null(config_list$upc_path)){
    upc <- readRDS(config_list$upc_path)
  }
  
  # get the projected rates
  message("get projected rates")
  fertility_rates <- evaluate_fns_list(config_list$fertility_fns) %>% complete_fertility(population)
  mortality_rates <- evaluate_fns_list(config_list$mortality_fns)
  int_out_flows_rates <- evaluate_fns_list(config_list$int_out_fns) 
  int_in_flows <- evaluate_fns_list(config_list$int_in_fns)
  #browser()
  #FIXME
  #domestic_rates <- evaluate_fns_list(config_list$dom_rate_fns)
  domestic_rates_info <- .get_domestic_rates_info(config_list$domestic_rates, first_proj_yr, last_proj_yr)
  domestic_rates <- NULL
  
  constraints <- evaluate_fns_list(config_list$constraint_fns)
  
  #Prep backseries
  population <- population %>% select(year, gss_code, age, sex, popn)
  deaths <- deaths %>% select(year, gss_code, age, sex, deaths)
  births <- births %>% select(year, gss_code, age, sex, births)
  int_out <- int_out %>% select(year, gss_code, age, sex, int_out)
  int_in <- int_in %>% select(year, gss_code, age, sex, int_in)
  dom_out <- dom_out %>% select(year, gss_code, age, sex, dom_out)
  dom_in <- dom_in %>% select(year, gss_code, age, sex, dom_in)
  
  fertility_rates <- fertility_rates %>% select(year, gss_code, age, sex, rate)
  mortality_rates <- mortality_rates %>% select(year, gss_code, age, sex, rate)
  int_out_flows_rates <- int_out_flows_rates %>% select(year, gss_code, age, sex, int_out)
  int_in_flows <- int_in_flows %>% select(year, gss_code, age, sex, int_in)
  
  curr_yr_popn <- filter(population, year == first_proj_yr-1)

  # set up projection
  validate_trend_core_inputs(population, births, deaths, int_out, int_in,
                             dom_out, dom_in, fertility_rates, mortality_rates,
                             int_out_flows_rates, int_in_flows, domestic_rates,
                             first_proj_yr, config_list$n_proj_yr,
                             config_list$int_out_method)
  
    
  
  ## run the core
  projection <- list()
  for(projection_year in first_proj_yr:last_proj_yr){
    
    curr_yr_fertility <- filter(fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(mortality_rates, year == projection_year)
    curr_yr_int_out <- filter(int_out_flows_rates, year == projection_year)
    curr_yr_int_in_flows <- int_in_flows %>% filter(year == projection_year)
   
    if(is.null(config_list$upc_path)){
      curr_yr_upc <- NULL
    } else { 
      curr_yr_upc <- upc %>% filter(year == projection_year)
    }
    
    if(is.data.frame(domestic_rates)){
      curr_yr_domestic_rates <- select(domestic_rates, gss_out, gss_in, age, sex, rate)
    }
    else {

      domestic_rates <- return_domestic_rates(domestic_rates, domestic_rates_info,
                                              projection_year, first_proj_yr)
      
      curr_yr_domestic_rates <- select(domestic_rates, gss_in, gss_out, sex, age, rate)
    }
    
    projection[[projection_year]] <- trend_core(
      start_population = curr_yr_popn, 
      fertility_rates = curr_yr_fertility, 
      mortality_rates = curr_yr_mortality,
      int_out_flows_rates = curr_yr_int_out,
      int_in_flows = curr_yr_int_in_flows,
      domestic_rates = curr_yr_domestic_rates,
      int_out_method = config_list$int_out_method,
      constraints = constraints,
      upc = curr_yr_upc,
      projection_year = projection_year,
      region_lookup = region_lookup)
    
    curr_yr_popn <- projection[[projection_year]][['population']]
  }
  
  projection <- arrange_trend_core_outputs(projection,
                                           population, births, deaths, int_out, int_in, dom_in, dom_out,
                                           fertility_rates, mortality_rates,
                                           int_out_flows_rates, int_in_flows, domestic_rates,
                                           first_proj_yr, last_proj_yr)
  
  validate_trend_core_outputs(projection)
  
  ## household models
  message('')
  message('running household models')
  
  projection$ons_households <- household_model_ons(population = projection$population,
                                                   stage1_file_path = config_list$ons_stage1_file_path,
                                                   stage2_file_path = config_list$ons_stage2_file_path,
                                                   communal_est_pop_path = config_list$communal_est_pop_path,
                                                   first_proj_yr = config_list$first_proj_yr)
  
  projection$dclg_households <- household_model_dclg(population = projection$population,
                                                     stage1_file_path = config_list$dclg_stage1_file_path,
                                                     stage2_file_path = config_list$dclg_stage2_file_path)
  
  ## write the output data
  message("running outputs")
  output_projection(projection, config_list$output_dir, write_excel = config_list$write_excel,
                    n_csv_elements=8, projection_name = config_list$projection_name)
  
  household_model_outputs(projection$ons_households, model = "ons", config_list$output_dir,
                          write_excel = config_list$write_excel, projection_name = config_list$projection_name)
  household_model_outputs(projection$dclg_households, model = "dclg", config_list$output_dir,
                          write_excel = config_list$write_excel, projection_name = config_list$projection_name)
  
  ## output the QA
  if(config_list$write_QA){
    rmarkdown::render("model_code/qa/population_qa.Rmd",
                      output_file = paste0("population_qa.html"),
                      output_dir = config_list$output_dir,
                      params = list(qa_areas_of_interest = config_list$qa_areas_of_interest,
                                    popn_proj_fp = paste0(config_list$output_dir,"/population.rds"),
                                    deaths_proj_fp = paste0(config_list$output_dir,"/deaths.rds"),
                                    int_in_proj_fp = paste0(config_list$output_dir,"/int_in.rds"),
                                    int_out_proj_fp = paste0(config_list$output_dir,"/int_out.rds"),
                                    dom_in_proj_fp = paste0(config_list$output_dir,"/dom_in.rds"),
                                    dom_out_proj_fp = paste0(config_list$output_dir,"/dom_out.rds"),
                                    births_proj_fp = paste0(config_list$output_dir,"/births.rds"),
                                    output_files_dir = paste0(config_list$output_dir,"population_qa_files/"),
                                    first_proj_yr = config_list$first_proj_yr))
  }
  
  return(projection)
}


#===============================================================================

# do checks on the input data
validate_trend_core_inputs <- function(population, births, deaths, int_out, int_in, dom_out, dom_in,
                                       fertility_rates, mortality_rates, int_out_flows_rates, int_in_flows, domestic_rates,
                                       first_proj_yr, n_proj_yr, int_out_method) {
  
  popmodules::validate_population(population, col_data = "popn")
  popmodules::validate_population(births, col_data = "births")
  popmodules::validate_population(deaths, col_data = "deaths")
  popmodules::validate_population(int_out, col_data = "int_out")
  popmodules::validate_population(int_in, col_data = "int_in")
  popmodules::validate_population(dom_out, col_data = c("dom_out"), test_complete = TRUE, test_unique = TRUE)
  popmodules::validate_population(dom_in, col_data = c("dom_in"), test_complete = TRUE, test_unique = TRUE)
  
  popmodules::validate_population(fertility_rates, col_data = "rate")
  popmodules::validate_population(mortality_rates, col_data = "rate")
  
  assert_that(int_out_method %in% c("flow", "rate"),
              msg = "the config variable int_out_method must be either 'flow' or 'rate'")
  popmodules::validate_population(int_out_flows_rates, col_data = ifelse(int_out_method == "flow", "int_out", "int_out"))
  popmodules::validate_population(int_in_flows, col_data = "int_in")
  
  if(is.data.frame(domestic_rates)){
    popmodules::validate_population(domestic_rates, col_aggregation = c("gss_out","gss_in","sex","age"), col_data = "rate", test_complete = FALSE, test_unique = TRUE)
    assert_that(max(domestic_rates$rate) <= 1 & min(domestic_rates$rate) >= 0, msg = "projected domestic migration rate contains rates outside the range 0-1")
  }else{
    # for(i in seq(domestic_rates)){
    #   assert_that(file.exists(domestic_rates[[i]]$path),
    #               msg = paste0("File not found: ",domestic_rates[[i]]$path))
    # }
  }
  
  # check that the rates join onto the population
  ## TODO make the aggregations columns flexible. Make this more elegant.
  popmodules::validate_join_population(population, mortality_rates, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, fertility_rates, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, int_out_flows_rates, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, int_in_flows, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  # TODO fix these checks for domestic_rates double geography.  Currently the below both fail
  #popmodules::validate_join_population(population, domestic_rates, cols_common_aggregation = c("gss_code"="gss_out","sex","age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  #popmodules::validate_join_population(domestic_rates, population, cols_common_aggregation = c("gss_out"="gss_code","sex","age"), pop1_is_subset = TRUE, many2one = TRUE, one2many = FALSE)
  
  # check that the coverage of years is correct
  last_proj_yr <- first_proj_yr + n_proj_yr -1
  assert_that((first_proj_yr - 1) %in% unique(population$year), msg = paste0("the population backseries doesn't contain the projection jump-off year (", first_proj_yr-1,")"))
  assert_that(all(first_proj_yr:last_proj_yr %in% fertility_rates$year), msg = "the projected fertility_rates data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% mortality_rates$year), msg = "the projected mortality_rates data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% int_out_flows_rates$year), msg = "the projected int_out_flows_rates data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% int_in_flows$year), msg = "the projected int_in data doesn't contain all the projection years")
  
  # check that the rates values are always between 0 and 1 
  assert_that(max(fertility_rates$rate) <= 1 & min(fertility_rates$rate) >= 0, msg = "projected fertility_rates contains rates outside the range 0-1")
  assert_that(max(mortality_rates$rate) <= 1 & min(mortality_rates$rate) >= 0, msg = "projected mortality_rates contains rates outside the range 0-1")
  if(int_out_method == "rate") {
    assert_that(max(int_out_flows_rates$int_out) <= 1 & min(int_out_flows_rates$int_out) >= 0, msg = "projected international out migration rate contains rates outside the range 0-1")
  }
  
  invisible(TRUE)
}


#===============================================================================

validate_trend_core_outputs <- function(projection) {
  
  components <- names(projection)
  expected_components <- c("population", "deaths","births", "int_out", "int_in",
                           "dom_out", "dom_in", "births_by_mothers_age", "natural_change",
                           "fertility_rates", "mortality_rates", "int_out_rates", "domestic_rates")
  
  assert_that(identical(components, expected_components))
  
  warning("Skipping tests on domestic trend output until aggregated domestic backseries are implemented")
  #sapply(projection[1:12], validate_population)
  sapply(projection[c(1,2,3,4,5,8,9,10,11,12)], validate_population)
  
  if(is.data.frame(projection$domestic_migration)){
    validate_population(projection$domestic_rates,
                        col_aggregation = c("gss_out", "gss_in", "age", "sex"),
                        col_data = "rate",
                        test_complete = FALSE)
  }
}
