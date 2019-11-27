run_trend_model <- function(config_list) {
  
  library(tidyverse)
  library(popmodules)
  source("model_code/model_scripts/trend/02_core.R")
  source("model_code/model_scripts/trend/03_output.R")
  
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
                       "int_out_flow_or_rate",
                       "qa_areas_of_interest",
                       "projection_name",
                       "write_excel",
                       "write_QA",
                       "ons_stage1_file_path",
                       "ons_stage2_file_path",
                       "communal_est_pop_path",
                       "dclg_stage1_file_path",
                       "dclg_stage2_file_path",
                       "timestamp")
  
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  #Create output directory
  if (!grepl("/$", config_list$outputs_dir)) config_list$outputs_dir <- paste0(config_list$outputs_dir, "/")
  output_dir <- paste0(config_list$outputs_dir, config_list$projection_name,"/")
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  #Validate file paths
  file_list <- config_list[stringr::str_detect(names(config_list), "path")]
  validate_paths <- lapply(seq(file_list),
         function(i) {
           if(!is.null(file_list[[i]])){
             assertthat::assert_that(file.exists(file_list[[i]]),
                                     msg = paste0(names(file_list)[[i]], ": ", file_list[[i]], "\nFile does not exist at specific path"))
             file_ext <- tolower(strsplit(file_list[[i]], split="\\.")[[1]][[2]])
             assertthat::assert_that(file_ext == "rds",
                                     msg = paste0(names(file_list)[[i]], ": ", file_list[[i]], "\nFile is not .rds format"))
            }
           })
  rm(file_list, validate_paths)      
  
  
  # get the MYEs
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
  
  if(is.null(config_list$upc_path)){
    upc <- NULL
  } else { 
    upc <- readRDS(config_list$upc_path)
  }
  
  
  # get the projected rates
  # strings together 'building blocks' which can be swapped out and replaced in config file
  message("get projected rates")
  fertility <- evaluate_fns_list(config_list$fertility_fns) %>%
    complete_fertility(population)
  
  mortality <- evaluate_fns_list(config_list$mortality_fns)
  
  int_out_rate <- evaluate_fns_list(config_list$int_out_rate_fns) 
  
  int_in_proj <- evaluate_fns_list(config_list$int_in_fns)
  
  dom_rate <- evaluate_fns_list(config_list$dom_rate_fns)
  
  constraints <- evaluate_fns_list(config_list$constraint_fns)
  
  
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
  
  ## run the core
  projection <- trend_core(population, births, deaths, int_out, int_in, 
                           fertility, mortality, int_out_rate, int_in_proj,
                           dom_in, dom_out, dom_rate,
                           config_list$first_proj_yr, config_list$n_proj_yr,
                           config_list$int_out_flow_or_rate,
                           constraints, upc)
  
  ## household models
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
  output_projection(projection, output_dir, timestamp = config_list$timestamp, write_excel = config_list$write_excel, n_csv_elements=8)
  household_model_outputs(projection$ons_households, model = "ons", output_dir, timestamp = config_list$timestamp, write_excel = config_list$write_excel)
  household_model_outputs(projection$dclg_households, model = "dclg", output_dir, timestamp = config_list$timestamp, write_excel = config_list$write_excel)
  
  ## output the QA
  if(config_list$write_QA){
    rmarkdown::render("model_code/qa/population_qa.Rmd",
                      output_file = paste0("population_qa",config_list$timestamp,".html"),
                      output_dir = output_dir,
                      params = list(qa_areas_of_interest = config_list$qa_areas_of_interest,
                                    popn_proj_fp = paste0(output_dir,"/population_",config_list$timestamp,".rds"),
                                    deaths_proj_fp = paste0(output_dir,"/deaths_",config_list$timestamp,".rds"),
                                    int_in_proj_fp = paste0(output_dir,"/int_in_",config_list$timestamp,".rds"),
                                    int_out_proj_fp = paste0(output_dir,"/int_out_",config_list$timestamp,".rds"),
                                    dom_in_proj_fp = paste0(output_dir,"/dom_in_",config_list$timestamp,".rds"),
                                    dom_out_proj_fp = paste0(output_dir,"/dom_out_",config_list$timestamp,".rds"),
                                    births_proj_fp = paste0(output_dir,"/births_",config_list$timestamp,".rds"),
                                    output_files_dir = paste0(output_dir,"population_qa",config_list$timestamp,"_files/"),
                                    first_proj_yr = config_list$first_proj_yr))
  }
  
  return(projection)
}
