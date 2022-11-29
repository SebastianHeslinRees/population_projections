#' Run a trend model config file to produce a population projection
#' Previously 00_control
#'
#' Read in, validate and manage model input data and then run the \code{trend_core}
#' function to produce a population projection. Run the \code{household model} functions
#' as specified in the config list. Run model output functions and, if specified,
#' rmarkdown QA scripts.
#'
#' @param config_list A List. A trend model configuration list.
#'
#' @import dplyr
#' @import popmodules
#' @importFrom rmarkdown render
#' @import assertthat
#' @importFrom loggr log_file
#'
#' @export

run_trend_model <- function(config_list) {
  
  region_lookup <- readRDS("input_data/lookup/district_to_region.rds")
  
  expected_config <- c("projection_name",
                       "first_proj_yr", 
                       "n_proj_yr",
                       "output_dir",
                       "popn_mye_path",
                       "deaths_mye_path",
                       "births_mye_path",
                       "int_out_mye_path",
                       "int_in_mye_path",
                       "dom_out_mye_path",
                       "dom_in_mye_path",
                       "upc_mye_path",
                       "popn_adjustment_path",
                       "external_births_path",
                       "external_deaths_path",
                       "mortality_rates",
                       "fertility_rates",
                       "int_out_flows_rates",
                       "int_out_method",
                       "int_in_flows",
                       "domestic_rates",
                       "constraint_fns",
                       "ons_stage1_file_path",
                       "ons_stage2_file_path",
                       "communal_est_pop_path",
                       "dclg_stage1_file_path",
                       "dclg_stage2_file_path",
                       "qa_areas_of_interest",
                       "write_QA",
                       "write_excel",
                       "geog_code_col")
  
  validate_config_list(config_list, expected_config)
  
  #Create output directory, warnings log and config log
  message(config_list$projection_name)
  dir.create(config_list$output_dir, recursive = T, showWarnings = F)
  loggr::log_file(paste0(config_list$output_dir,"warnings.log"))
  write_model_config(config_list)
  
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
  
  if(!is.null(config_list$upc_mye_path)){
    upc_mye <- readRDS(config_list$upc_mye_path)
  } else {
    upc_mye <- NULL
  }
  
  if(!is.null(config_list$popn_adjustment_path)){
    popn_adjustment <- readRDS(config_list$popn_adjustment_path)
  } else {
    popn_adjustment <- NULL
  }
  
  if(!is.null(config_list$external_births_path)){
    external_births <-  readRDS(config_list$external_births_path)
  } else {
    external_births <- NULL
  }
  

  if(!is.null(config_list$external_deaths_path)){
    external_deaths <- readRDS(config_list$external_deaths_path)
  } else {
    external_deaths <- NULL
  }

  # get the projected rates
  message("get projected rates")
  
  eval_or_read <- function(string_or_list){
    if(is.list(string_or_list)){
      rates <- evaluate_fns_list(string_or_list)
    }
    if(is.string(string_or_list)){
      rates <- readRDS(string_or_list)
    }
    return(rates)
  }
  
  fertility_rates <- eval_or_read(config_list$fertility_rates) %>% complete_popn_dataframe()
  mortality_rates <- eval_or_read(config_list$mortality_rates)
  
  international_out_flow_info <- get_rates_flows_info(config_list$int_out_flows, first_proj_yr, last_proj_yr)
  int_out_flows_rates <- NULL
  
  domestic_rates_info <- get_rates_flows_info(config_list$domestic_rates, first_proj_yr, last_proj_yr)
  domestic_rates <- NULL
  
  international_in_flow_info <- get_rates_flows_info(config_list$int_in_flows, first_proj_yr, last_proj_yr)
  int_in_flows <- NULL
  
  constraints <- eval_or_read(config_list$constraint_fns)
  
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
  international_in <- data.frame()
  international_out <- data.frame()
  
  curr_yr_popn <- filter(population, year == first_proj_yr-1)
  
  # set up projection
  validate_trend_core_inputs(population, births, deaths, int_out, int_in,
                             dom_out, dom_in, popn_adjustment, upc_mye,
                             fertility_rates, mortality_rates,
                             external_births,
                             external_deaths,
                             int_out_flows_rates,
                             first_proj_yr, config_list$n_proj_yr,
                             config_list$int_out_method)
  
  ## run year loop
  message("projecting")
  projection <- list()
  for(projection_year in first_proj_yr:last_proj_yr){
    
    cat('\r',projection_year)
    utils::flush.console()
    
    curr_yr_fertility <- filter(fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(mortality_rates, year == projection_year)
    
    if(!is.null(external_births) & projection_year %in% unique(external_births$year)){
      curr_yr_external_births <- filter(external_births, year == projection_year)
    } else {
      curr_yr_external_births <- NULL
    }
    
    if(!is.null(external_deaths) & projection_year %in% unique(external_deaths$year)){
      curr_yr_external_deaths <- filter(external_deaths, year == projection_year)
    } else {
      curr_yr_external_deaths <- NULL
    }
    
    if(is.null(popn_adjustment)){
      curr_yr_popn_adjustment <- NULL
    } else { 
      curr_yr_popn_adjustment <- popn_adjustment %>% filter(year == projection_year)
    }
    
    domestic_rates <- get_rates_or_flows(domestic_rates, domestic_rates_info,
                                         projection_year, first_proj_yr,
                                         col_aggregation = c("gss_in","gss_out","sex","age"),
                                         data_col = "rate",
                                         geog_code_col = config_list$geog_code_col,
                                         standardise_geog = FALSE)
    
    curr_yr_domestic_rates <- select(domestic_rates, gss_in, gss_out, sex, age, rate)
    
    int_in_flows <- get_rates_or_flows(int_in_flows, international_in_flow_info,
                                       projection_year, first_proj_yr,
                                       col_aggregation = c("gss_code","sex","age"),
                                       data_col = "int_in",
                                       geog_code_col = config_list$geog_code_col,
                                       standardise_geog = FALSE)
    
    curr_yr_int_in <- int_in_flows %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, sex, age, int_in)
    
    int_out_flows_rates <- get_rates_or_flows(int_out_flows_rates, international_out_flow_info,
                                              projection_year, first_proj_yr,
                                              col_aggregation = c("gss_code","sex","age"),
                                              data_col = "int_out",
                                              geog_code_col = config_list$geog_code_col,
                                              standardise_geog = FALSE)
    
    curr_yr_int_out <- int_out_flows_rates %>% 
      mutate(year = projection_year) %>% 
      select(year, gss_code, sex, age, int_out)
    
    international_out <- rbind(international_out, curr_yr_int_out)
    
    #run core
    projection[[projection_year]] <- trend_core(
      start_population = curr_yr_popn,
      fertility_rates = curr_yr_fertility,
      mortality_rates = curr_yr_mortality,
      external_births = curr_yr_external_births,
      external_deaths = curr_yr_external_deaths,
      int_out_flows_rates = curr_yr_int_out,
      int_in_flows = curr_yr_int_in,
      domestic_rates = curr_yr_domestic_rates,
      int_out_method = config_list$int_out_method,
      constraints = constraints,
      popn_adjustment = curr_yr_popn_adjustment,
      projection_year = projection_year,
      region_lookup = region_lookup)
    
    curr_yr_popn <- projection[[projection_year]][['population']]
  }
  
  message(" ")
  message("arrange outputs")
  projection <- arrange_trend_core_outputs(projection,
                                           population, births, deaths, int_out,
                                           int_in, dom_in, dom_out,
                                           upc_mye, popn_adjustment,
                                           fertility_rates, mortality_rates,
                                           international_out,
                                           first_proj_yr, last_proj_yr)
  
  validate_trend_core_outputs(projection, first_proj_yr)
  
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
  output_trend_projection(projection, config_list$output_dir, write_excel = config_list$write_excel,
                          n_csv_elements=11, projection_name = config_list$projection_name)
  
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
  
  deactivate_log(paste0(config_list$output_dir,"warnings.log"))
  
  return(projection)

}
  



#===============================================================================

# do checks on the input data
validate_trend_core_inputs <- function(population, births, deaths, int_out, int_in, dom_out, dom_in, popn_adjustment,
                                       upc_mye, fertility_rates, mortality_rates, external_births, external_deaths,
                                       int_out_flows_rates, first_proj_yr, n_proj_yr, int_out_method) {


  validate_population(population, col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  
  validate_population(births, col_data = "births",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = filter(population, age==0, year %in% births$year),
                      col_comparison = c("year","gss_code", "sex","age"))
  
  validate_population(deaths, col_data = "deaths",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = filter(population, year %in% deaths$year),
                      col_comparison =  c("year","gss_code", "sex","age"))
  
  validate_population(int_out, col_data = "int_out",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = filter(population, year %in% int_out$year),
                      col_comparison =  c("year","gss_code", "sex","age"))
  
  validate_population(int_in, col_data = "int_in",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = filter(population, year %in% int_in$year),
                      col_comparison =  c("year","gss_code", "sex","age"))
  
  validate_population(filter_to_LAs(dom_out), col_data = c("dom_out"),
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = filter(population, year %in% dom_out$year),
                      col_comparison =  c("year","gss_code", "sex","age"))
  
  validate_population(filter_to_LAs(dom_in), col_data = c("dom_in"),
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = filter(population, year %in% dom_in$year),
                      col_comparison =  c("year","gss_code", "sex","age"))
  
  if(!is.null(popn_adjustment)) {
    validate_population(popn_adjustment, col_data = "upc",
                        test_complete = FALSE, test_unique = TRUE, check_negative_values = FALSE)
  }
  
  if(!is.null(upc_mye)) {
    validate_population(upc_mye, col_data = "upc",
                        test_complete = FALSE, test_unique = TRUE, check_negative_values = FALSE)
  }
  

  if(!is.null(external_births)){
    validate_population(external_births, col_data = "births",
                        test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
  }
  

  if(!is.null(external_deaths)){
    validate_population(external_deaths, col_data = "deaths",
                        test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
  }
  
  validate_population(fertility_rates, col_data = "rate",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  
  validate_population(mortality_rates, col_data = "rate",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  
  assert_that(int_out_method %in% c("flow", "rate"),
              msg = "the config variable int_out_method must be either 'flow' or 'rate'")
  
  # check that the rates join onto the population
  validate_join_population(population, mortality_rates,
                           cols_common_aggregation = c("gss_code", "sex", "age"),
                           aggregation_levels_match = FALSE,
                           warn_unused_shared_cols = FALSE)
  validate_join_population(population, fertility_rates,
                           cols_common_aggregation = c("gss_code", "sex", "age"),
                           aggregation_levels_match = FALSE,
                           warn_unused_shared_cols = FALSE)
  
  # TODO move these checks for rates now that they're loaded in dynamically. Also the domestic checks never worked.
  #validate_join_population(population, int_out_flows_rates, cols_common_aggregation = c("gss_code", "sex", "age"), aggregation_levels_match = FALSE, warn_unused_shared_cols = FALSE)
  #validate_join_population(population, int_in_flows, cols_common_aggregation = c("gss_code", "sex", "age"), aggregation_levels_match = FALSE, warn_unused_shared_cols = FALSE)
  #validate_join_population(population, domestic_rates, cols_common_aggregation = c("gss_code"="gss_out","sex","age"), aggregation_levels_match = FALSE, warn_unused_shared_cols = FALSE)
  #validate_join_population(domestic_rates, population, cols_common_aggregation = c("gss_out"="gss_code","sex","age"), aggregation_levels_match = TRUE, many2one = TRUE, one2many = FALSE)
  
  # check that the coverage of years is correct
  last_proj_yr <- first_proj_yr + n_proj_yr -1
  assert_that((first_proj_yr - 1) %in% unique(population$year), msg = paste0("the population backseries doesn't contain the projection jump-off year (", first_proj_yr-1,")"))
  assert_that(all(first_proj_yr:last_proj_yr %in% fertility_rates$year), msg = "the projected fertility_rates data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% mortality_rates$year), msg = "the projected mortality_rates data doesn't contain all the projection years")
  #assert_that(all(first_proj_yr:last_proj_yr %in% int_out_flows_rates$year), msg = "the projected int_out_flows_rates data doesn't contain all the projection years")
  
  # check that the rates values are always between 0 and 1 
  assert_that(max(fertility_rates$rate) <= 1 & min(fertility_rates$rate) >= 0, msg = "projected fertility_rates contains rates outside the range 0-1")
  assert_that(max(mortality_rates$rate) <= 1 & min(mortality_rates$rate) >= 0, msg = "projected mortality_rates contains rates outside the range 0-1")
  # if(int_out_method == "rate") {
  #   assert_that(max(int_out_flows_rates$int_out) <= 1 & min(int_out_flows_rates$int_out) >= 0, msg = "projected international out migration rate contains rates outside the range 0-1")
  # }
  
  invisible(TRUE)
}


#===============================================================================

validate_trend_core_outputs <- function(projection, first_proj_yr) {
  
  components <- names(projection)
  expected_components <- c("population", "deaths","births", "int_out", "int_in",
                           "dom_out", "dom_in", "int_net", "dom_net", "total_net",
                           "births_by_mothers_age", "natural_change",
                           "fertility_rates", "mortality_rates",
                           "int_out_rates_flows", "popn_adjustment")
  
  assert_that(identical(components, expected_components))
  
  validate_dataframes <- c("population", "deaths", "births", "int_out", "int_in",
                           "int_net", "total_net", "births_by_mothers_age",
                           "natural_change", "fertility_rates", "mortality_rates",
                           "int_out_rates_flows")
  
  for(i in validate_dataframes){ 
    validate_population(projection[[i]],
                        test_complete = TRUE,
                        test_unique = TRUE,
                        check_negative_values = TRUE)
  }
  
  validate_domestic_components(projection[["dom_out"]],"domestic out", first_proj_yr)
  validate_domestic_components(projection[["dom_in"]],"domestic in", first_proj_yr)
  
  if(!is.null(projection$popn_adjustment)){
    validate_population(projection$popn_adjustment,
                        test_complete = TRUE,
                        test_unique = TRUE,
                        check_negative_values = TRUE)
  }
  
}

validate_domestic_components <- function(data, component, first_proj_yr){
  
  dom_codes <- setdiff(unique(filter(data, year>=first_proj_yr)$gss_code),
                       unique(filter(data, year<first_proj_yr)$gss_code))
  
  if(length(dom_codes) > 0){
    warning(paste0("In the ", component ," component there are ",length(dom_codes),
                   " code(s) present in the projection that are not in the backseries: ",
                   paste(dom_codes, collapse = ", ")))
  }
  
  dom_codes <- setdiff(unique(filter(data, year<first_proj_yr)$gss_code),
                       unique(filter(data, year>=first_proj_yr)$gss_code))
  
  if(length(dom_codes) > 0){
    warning(paste0("In the ", component ," component there are ",length(dom_codes),
                   " code(s) present in the backseries that are not in the projection: ",
                   paste(dom_codes, collapse = ", ")))
  }
  
}
