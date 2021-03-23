#BPO Scenario 1
#Standard covid assumptions
#Central int, high dom longer term

library(popmodules)
library(trendmodel)

first_proj_yr <- 2020
n_proj_yr <- 31
projection_name <- "2019_BPO_scenario_1"

popn_mye_path <- paste0("input_data/mye/2019/population_gla.rds")
deaths_mye_path <-  paste0("input_data/mye/2019/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2019/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2019/int_out_gla.rds")
int_in_mye_path <-  paste0("input_data/mye/2019/int_in_gla.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2019/domestic_migration_out_(2020_geog).rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2019/domestic_migration_in_(2020_geog).rds")
upc_mye_path <- "input_data/mye/2019/upc_ons.rds"
popn_adjustment_path <- "input_data/scenario_data/covid19_deaths.rds"

int_out_flow_or_rate <- "flow"

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates_(2020_geog).rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016_(2020_geog).rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population_2018_(2020_model).rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014_(2020_geog).rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014_(2020_geog).rds"

write_excel <- FALSE

#-------------------------------------------------

timestamp <- format(Sys.time(), "%y-%m-%d_%H%M")
projection_name <- paste0(projection_name,"_",timestamp)
output_dir <- paste0("outputs/trend/2019/",projection_name,"/")

#------------------------------------------

mortality_rates <- "input_data/mortality/mort_rates_5yr_trend_2019_gla_mye.rds"

#------------------------------------------

fertility_rates <- "input_data/fertility/fert_rates_5yr_trend_2019_gla_mye.rds"

#-----------------------------------------------------

int_out_flows_rates <- list('2020' = list(path = "input_data/scenario_data/bpo_int_out_scenario_1_yr_2020.rds",
                                          transition = F),
                            '2021' = list(path = "input_data/scenario_data/bpo_int_out_scenario_1_yr_2021.rds",
                                          transition = F),
                            '2022' = list(path = "input_data/scenario_data/bpo_int_out_scenario_1_yr_2022.rds",
                                          transition = T),
                            '2028' = list(path = "input_data/scenario_data/international_10yr_out_flows.rds",
                                          transition = F))

#-----------------------------------------------------

int_in  <- list('2020' = list(path = "input_data/scenario_data/bpo_int_in_scenario_1_yr_2020.rds",
                              transition = F),
                '2021' = list(path = "input_data/scenario_data/bpo_int_in_scenario_1_yr_2021.rds",
                              transition = F),
                '2022' = list(path = "input_data/scenario_data/bpo_int_in_scenario_1_yr_2022.rds",
                              transition = T),
                '2028' = list(path = "input_data/scenario_data/international_10yr_in_flows.rds",
                              transition = F))

#-----------------------------------------------------

domestic_rates <- list('2020' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2020.rds",
                                     transition = F),
                       '2021' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2021.rds",
                                     transition = F),
                       '2022' = list(path = "input_data/scenario_data/bpo_dom_scenario_1_yr_2022.rds",
                                     transition = T),
                       '2028' = list(path = "input_data/domestic_migration/processed_rates/dom_rates_5yr_avg_2019_gla_mye.rds",
                                     transition = F))

#-----------------------------------------------------

constraint_fns <- list(list(fn = function() NULL, args = list()))

# prepare the named list to pass into model
config_list <- list(
  projection_name = projection_name,
  first_proj_yr = first_proj_yr,
  n_proj_yr = n_proj_yr,
  output_dir = output_dir,
  
  popn_mye_path = popn_mye_path,
  deaths_mye_path = deaths_mye_path,
  births_mye_path = births_mye_path,
  int_out_mye_path = int_out_mye_path,
  int_in_mye_path = int_in_mye_path,
  dom_out_mye_path = dom_out_mye_path,
  dom_in_mye_path = dom_in_mye_path,
  upc_mye_path = upc_mye_path,
  popn_adjustment_path = popn_adjustment_path,
  
  mortality_rates = mortality_rates,
  fertility_rates = fertility_rates,
  int_out_flows_rates = int_out_flows_rates,
  int_out_method = int_out_flow_or_rate,
  int_in_flows = int_in,
  domestic_rates = domestic_rates,
  constraint_fns = constraint_fns,
  
  communal_est_pop_path = communal_est_pop_path,
  ons_stage1_file_path = ons_stage1_file_path,
  ons_stage2_file_path = ons_stage2_file_path,
  dclg_stage1_file_path = dclg_stage1_file_path,
  dclg_stage2_file_path = dclg_stage2_file_path,
  
  qa_areas_of_interest = FALSE,
  write_QA = FALSE,
  write_excel = write_excel
)

rm(list = setdiff(ls(), "config_list"))

# Run the model
devtools::load_all('model_code/trendmodel')
projection <- run_trend_model(config_list)
