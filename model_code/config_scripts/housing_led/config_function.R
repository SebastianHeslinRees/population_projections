run_bpo_projection <- function(projection_name,
                               dev_trajectory_path,
                               small_area_dev_trajectory_path,
                               first_proj_yr,
                               final_proj_yr,
                               bpo){
  tm <- Sys.time()
  #Setup
  external_trend_path <- "outputs/trend/2018/2018_central/"
  external_trend_datestamp <- "19-11-13_2056"
  communal_est_file <- "ons_communal_est_population.rds"
  trend_households_file <- "ons_stage_1_households.rds"
  ldd_backseries_path <- "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds"
  
  external_ahs_trajectory_path <- paste0(external_trend_path,"households_",external_trend_datestamp,"/ons_ahs.rds")
  
  hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
  ahs_cap_year <- 2019
  ldd_max_yr <- 2018
  
  output_dir <- paste0("outputs/housing_led/2018/",projection_name,"_",format(Sys.time(), "%y-%m-%d_%H%M"),"/")
  
  #------------------
  #Setup config list
  config_list <- list(
    projection_name = projection_name,
    communal_est_file = communal_est_file,
    hma_list = hma_list,
    dev_trajectory_path = dev_trajectory_path,
    external_ahs_trajectory_path = external_ahs_trajectory_path,
    trend_households_file = trend_households_file,
    ldd_backseries_path = ldd_backseries_path,
    ahs_cap_year = ahs_cap_year,
    external_trend_path = external_trend_path,
    external_trend_datestamp = external_trend_datestamp,
    first_proj_yr = first_proj_yr,
    final_proj_yr = final_proj_yr,
    ldd_max_yr = ldd_max_yr,
    output_dir = output_dir)
  
  #---------------------
  #run projection
  source('model_code/model_scripts/housing_led/housing_led_control.R')
  borough_projection <- run_housing_led_model(config_list)
  log_warnings(paste0(config_list$output_dir,"warnings.txt"))
  
  #----
  
  ####WARD MODEL
  small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates.rds"
  small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
  small_area_births_backseries_path <- "input_data/small_area_model/ward_births.rds"
  small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths.rds"
  small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"
  
  adults_per_dwelling_path <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
  small_area_to_district_path <- "input_data/lookup/2011_ward_to_district.rds"
  out_migration_rates_path <- "input_data/small_area_model/ward_out_migration_rates.rds"
  in_migration_characteristics_path <- "input_data/small_area_model/ward_in_migration_characteristics.rds"
  
  housing_led_model_path <- config_list$output_dir
 
  borough_fertility_rates_path <- paste0(config_list$external_trend_path,"fertility_rates_",
                                         config_list$external_trend_datestamp,".rds")
  borough_mortality_rates_path <- paste0(config_list$external_trend_path,"mortality_rates_",
                                         config_list$external_trend_datestamp,".rds")
  
  last_data_year <- 2018
  first_proj_yr <- config_list$first_proj_yr
  final_proj_yr <- config_list$final_proj_yr
  birth_rate_n_years_to_avg <- 5
  death_rate_n_years_to_avg <- 5
  
  projection_type <- "ward"

  ward_config_list <- list(small_area_popn_estimates_path = small_area_popn_estimates_path,
                           small_area_communal_est_popn_path = small_area_communal_est_popn_path,
                           small_area_births_backseries_path = small_area_births_backseries_path,
                           small_area_deaths_backseries_path = small_area_deaths_backseries_path,
                           small_area_ldd_data_path = small_area_ldd_data_path,
                           small_area_dev_trajectory_path = small_area_dev_trajectory_path,
                           
                           adults_per_dwelling_path = adults_per_dwelling_path,
                           small_area_to_district_path = small_area_to_district_path,
                           out_migration_rates_path = out_migration_rates_path,
                           in_migration_characteristics_path = in_migration_characteristics_path,
                           
                           housing_led_model_path = housing_led_model_path,
                           
                           borough_fertility_rates_path = borough_fertility_rates_path,
                           borough_mortality_rates_path = borough_mortality_rates_path,
                           
                           last_data_year = last_data_year,
                           first_proj_yr = first_proj_yr,
                           final_proj_yr = final_proj_yr,
                           
                           birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                           death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                           
                           ldd_max_yr = ldd_max_yr,
                           
                           projection_type = projection_type,
                           
                           bpo = bpo)
  
  rm(list = setdiff(ls(), c("ward_config_list","config_list","borough_projection","tm")))
  
  source('model_code/model_scripts/small_area/small_area_control.R')
  ward_projection <- run_small_area_model(ward_config_list)
  log_warnings(paste0(ward_config_list$housing_led_model_path, ward_config_list$projection_type,"/warnings.txt"))
  
  #Dev Sheets
  ward_dev_dataframe <- readRDS(ward_config_list$small_area_dev_trajectory_path) %>%
    left_join(readRDS("input_data/lookup/2011_ward_to_district.rds"), by="gss_code_ward") %>%
    filter(gss_code == borough_gss) %>%
    left_join(data.table::fread("input_data/lookup/lad18_code_to_name.csv"), by="gss_code") %>%
    select(gss_code, borough=gss_name, gss_code_ward, ward_name, year, units) %>%
    tidyr::pivot_wider(names_from = year, values_from = units)
  
  assumed_dev_dataframe <- readRDS(config_list$dev_trajectory_path) %>%
    filter(gss_code == borough_gss) %>%
    mutate(borough = unique(ward_dev_dataframe$borough),
           gss_code_ward = gss_code,
           ward_name = paste0(borough, " (total)")) %>%
    select(gss_code, borough, gss_code_ward, ward_name, year, units) %>%
    tidyr::pivot_wider(names_from = year, values_from = units) %>%
    rbind(ward_dev_dataframe) %>%
    as.data.frame()
  
  wb <- xlsx::loadWorkbook(paste0(config_list$output_dir,"ward/bpo_workbook.xlsx"))
  aa <- getSheets(wb)
  xlsx::addDataFrame(assumed_dev_dataframe, aa$Assumed, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #dev data source 
  dev_source_text <- as.data.frame(paste0("4. These projections incorporate assumptions about future development provided by the London Borough of ",
                            unique(assumed_dev_dataframe$borough)))
                   
  xlsx::addDataFrame(dev_source_text, aa$Metadata, col.names = FALSE, row.names = FALSE, startRow = 11, startColumn = 1)

  #Write xlsx file
  wbwrite <- paste0(config_list$output_dir, config_list$projection_name,"_BPO.xlsx")
  saveWorkbook(wb, wbwrite)
  file.remove(paste0(config_list$output_dir,"ward/bpo_workbook.xlsx"))
  
  #Finish
  data.table::fwrite(data.frame(time = Sys.time() - tm), paste0(ward_config_list$housing_led_model_path, "run_time.txt"))
  message("Complete")
  
  return(list(borough_projection = borough_projection,
              ward_projection = ward_projection))
  
}