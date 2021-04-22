#Housing-led scenario 1
#Migration = BPO scenario 1
#Development = Savills scenario

library(popmodules)
library(housingledmodel)

external_trend_path <- paste0(list.files(path="outputs/trend/2019/", pattern= "2019_BPO_scenario_1", full.names = TRUE),"/")
projection_name <- "housing_led_scenario_1"

communal_est_file <- "dclg_communal_est_population.rds"
trend_households_file <- "dclg_stage_1_households.rds"
ldd_backseries_path <- "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds"

dev_trajectory_path <- "input_data/housing_led_model/borough_2019_based_savills.rds"
external_ahs_trajectory_path <- paste0(external_trend_path, "households/dclg_ahs.rds")

popn_adjustment_path <- "input_data/scenario_data/covid19_deaths.rds"

constrain_projection <- F

hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
first_proj_yr <- 2020
last_proj_yr <- 2050
ahs_cap_year <- 2020
ahs_method <- 0.8

ldd_final_yr <- 2019
last_data_yr <- 2019

output_dir <- paste0("outputs/housing_led/2019/",projection_name,"_",format(Sys.time(), "%y-%m-%d_%H%M"),"/")

domestic_rates <- list('2020' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2020.rds",
                                     transition = F),
                       '2021' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2021.rds",
                                     transition = F),
                       '2022' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2022.rds",
                                     transition = T),
                       '2028' = list(path = "input_data/domestic_migration/processed_rates/dom_rates_5yr_avg_2019_gla_mye.rds",
                                     transition = F))

fertility_rates_path <- "input_data/fertility/fertility_rates_provisional_2020_5yr_trend.rds"
additional_births_path <- "input_data/fertility/provisional_births_2020_EW.rds"

#------------------

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
  last_proj_yr = last_proj_yr,
  ldd_final_yr = ldd_final_yr,
  last_data_yr = last_data_yr,
  output_dir = output_dir,
  domestic_rates = domestic_rates,
  constrain_projection = constrain_projection,
  ahs_method = ahs_method,
  additional_births_path = additional_births_path,
  fertility_rates_path = fertility_rates_path,
  popn_adjustment_path = popn_adjustment_path)

#---------------------

rm(list=setdiff(ls(),"config_list"))

#---------------------

#WARD SETUP

projection_name <- config_list$projection_name
first_proj_yr <- config_list$first_proj_yr
last_proj_yr <- config_list$last_proj_yr
dev_trajectory_path <- config_list$dev_trajectory_path
external_trend_path <- config_list$external_trend_path
ldd_final_yr <- config_list$ldd_final_yr
last_data_yr <- config_list$last_data_yr
housing_led_model_path <- paste0(config_list$output_dir)

small_area_dev_trajectory_path <- "input_data/small_area_model/ward_2019_based_savills.rds"
small_area_popn_estimates_path <- "input_data/small_area_model/ward_population_estimates.rds"
small_area_communal_est_popn_path  <- "input_data/small_area_model/ward_communal_establishment_population.rds"
small_area_births_backseries_path <- "input_data/small_area_model/ward_births.rds"
small_area_deaths_backseries_path <- "input_data/small_area_model/ward_deaths.rds"
small_area_ldd_data_path <- "input_data/small_area_model/ldd_backseries_dwellings_ward.rds"
small_area_births_sya_path <- "input_data/small_area_model/ward_sya_births.rds"
small_area_deaths_sya_path <- "input_data/small_area_model/ward_sya_deaths.rds"
adults_per_dwelling_path <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
small_area_to_district_path <- "input_data/lookup/2011_ward_to_district.rds"
out_migration_rates_path <- "input_data/small_area_model/ward_out_migration_rates.rds"
in_migration_characteristics_path <- "input_data/small_area_model/ward_in_migration_characteristics.rds"

borough_fertility_rates_path <- config_list$fertility_rates_path
borough_mortality_rates_path <- paste0(external_trend_path,"mortality_rates.rds")

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
                         
                         last_data_yr = last_data_yr,
                         first_proj_yr = first_proj_yr,
                         last_proj_yr = last_proj_yr,
                         
                         birth_rate_n_years_to_avg = birth_rate_n_years_to_avg,
                         death_rate_n_years_to_avg = death_rate_n_years_to_avg,
                         
                         ldd_final_yr = ldd_final_yr,
                         
                         projection_type = projection_type,
                         
                         small_area_births_sya_path = small_area_births_sya_path,
                         small_area_deaths_sya_path = small_area_deaths_sya_path)

#---------------------
rm(list = setdiff(ls(), c("config_list","ward_config_list")))
projection <- run_housing_led_model(config_list)
ward_projection <- run_small_area_model(ward_config_list)

