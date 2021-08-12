#2020 housing-led projections
#Standard variables

params <- ls()
standard <- list()

#-------------------------------------------------------------------------------
#Basics
standard$first_proj_yr <- 2021
standard$n_proj_yr <- 30

#-------------------------------------------------------------------------------
#Input data paths

standard$communal_est_file <- "dclg_communal_est_population.rds"
standard$trend_households_file <- "dclg_stage_1_households.rds"
standard$ldd_backseries_path <- "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds"
standard$popn_adjustment_path <- NULL
standard$external_ahs_trajectory_path <- paste0(external_trend_path, "households/dclg_ahs.rds")
standard$external_births_path <- NULL
standard$external_deaths_path <- "input_data/mortality/external_deaths_2021.rds"
standard$fertility_rates_path <- "input_data/fertility/fert_rates_5yr_trend_2020.rds"

#-------------------------------------------------------------------------------
#Variables

standard$constrain_projection <- FALSE
standard$hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))

standard$ahs_cap_year <- 2021
standard$ahs_method <- 0.8

standard$ldd_final_yr <- 2019
standard$last_data_yr <- 2020

#-------------------------------------------------------------------------------

#Other stuff

timestamp <- format(Sys.time(), "%y-%m-%d_%H%M")
projection_name <- paste0(projection_name,"_",timestamp)
standard$output_dir <- paste0("outputs/housing_led/2020/",projection_name,"/")

#-------------------------------------------------------------------------------

#These 2 lines ensure that any parameters set in advance of this script running
#are used instead of the standard parameters defined above
standard <- standard[!names(standard) %in% params]
list2env(standard, environment())

#-------------------------------------------------------------------------------

#This runs a separate script to add the standard covid migration assumptions
#to the 3 migration parameters
if(standard_covid_migration){
  source('config_scripts/trend/2020/standard_2020_covid_migration.R')
  mig <- covid_migration(NULL, NULL, domestic_rates)
  domestic_rates <- mig[[3]]
}

if(!"external_births_path" %in% ls()){
  external_births_path <- NULL
}
if(!"external_deaths_path" %in% ls()){
  external_deaths_path <- NULL
}
if(!"popn_adjustment_path" %in% ls()){
  popn_adjustment_path <- NULL
}

#-------------------------------------------------------------------------------
#Borough model config list

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
  first_proj_yr = first_proj_yr,
  n_proj_yr = n_proj_yr,
  ldd_final_yr = ldd_final_yr,
  last_data_yr = last_data_yr,
  output_dir = output_dir,
  domestic_rates = domestic_rates,
  constrain_projection = constrain_projection,
  ahs_method = ahs_method,
  external_births_path = external_births_path,
  external_deaths_path = external_deaths_path,
  fertility_rates_path = fertility_rates_path,
  popn_adjustment_path = popn_adjustment_path)

rm(list = setdiff(ls(), c("config_list", params)))

#-------------------------------------------------------------------------------
#Ward model
standard <- list()
params <- setdiff(ls(), "config_list")

#Variables
standard$first_proj_yr <- 2020
standard$last_data_yr <- 2019
standard$ldd_final_yr <- config_list$ldd_final_yr
standard$housing_led_model_path <- paste0(config_list$output_dir)
standard$birth_rate_n_years_to_avg <- 5
standard$death_rate_n_years_to_avg <- 5
standard$projection_type <- "ward"

#-------------------------------------------------------------------------------
#Input data paths
standard$small_area_popn_estimates_path <- "input_data/small_area_model/ward_data/ward_population_estimates.rds"
standard$small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_data/ward_communal_establishment_population.rds"
standard$small_area_births_backseries_path <- "input_data/small_area_model/ward_data/ward_births.rds"
standard$small_area_deaths_backseries_path <- "input_data/small_area_model/ward_data/ward_deaths.rds"
standard$small_area_ldd_data_path <- "input_data/small_area_model/development_data/ldd_backseries_dwellings_ward.rds"
standard$small_area_births_sya_path <- "input_data/small_area_model/ward_data/ward_sya_births.rds"
standard$small_area_deaths_sya_path <- "input_data/small_area_model/ward_data/ward_sya_deaths.rds"
standard$adults_per_dwelling_path <- "input_data/small_area_model/ward_data/ward_adults_per_dwelling.rds"
standard$small_area_to_district_path <- "input_data/lookup/2011_ward_to_district.rds"
standard$out_migration_rates_path <- "input_data/small_area_model/ward_data/ward_out_migration_rates.rds"
standard$in_migration_characteristics_path <- "input_data/small_area_model/ward_data/ward_in_migration_characteristics.rds"
standard$borough_fertility_rates_path <- paste0(config_list$external_trend_path,"fertility_rates.rds")
standard$borough_mortality_rates_path <- paste0(config_list$external_trend_path,"mortality_rates.rds")

#-------------------------------------------------------------------------------

#These 2 lines ensure that any parameters set in advance of this script running
#are used instead of the standard parameters defined above
standard <- standard[!names(standard) %in% params]
list2env(standard, environment())

#-------------------------------------------------------------------------------
#Always want the small area to run to the same year as the housing-led
#but the small area may start a year sooner, so...

n_proj_yr <- config_list$first_proj_yr + config_list$n_proj_yr - first_proj_yr

#-------------------------------------------------------------------------------

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
                         
                         last_data_yr = last_data_yr,
                         first_proj_yr = first_proj_yr,
                         n_proj_yr = n_proj_yr,
                         
                         birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                         death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                         
                         ldd_final_yr = ldd_final_yr,
                         
                         projection_type = projection_type,
                         
                         small_area_births_sya_path = small_area_births_sya_path,
                         small_area_deaths_sya_path = small_area_deaths_sya_path)

#-------------------------------------------------------------------------------
# MSOA model

msoa_config_list <- convert_ward_config_to_msoa_config(ward_config_list)

#-------------------------------------------------------------------------------

rm(list = setdiff(ls(), c("config_list","ward_config_list","msoa_config_list")))