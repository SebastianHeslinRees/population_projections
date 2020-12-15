# TfL scenario projection - agglomeration

library(popmodules)
library(trendmodel)

first_proj_yr <- 2020
n_proj_yr <- 31
projection_name <- "agglomeration"

popn_mye_path <- paste0("input_data/mye/2019/population_ons.rds")
deaths_mye_path <-  paste0("input_data/mye/2019/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2019/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2019/int_out_ons.rds")
int_in_mye_path <-  paste0("input_data/mye/2019/int_in_ons.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2019/domestic_migration_out_(2020_geog).rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2019/domestic_migration_in_(2020_geog).rds")
upc_path <- "input_data/scenario_data/covid19_upc.rds"

int_out_last_data_year <- 2019
int_out_years_to_avg <- 10
int_out_flow_or_rate <- "rate"
int_out_rate_cap <- 0.8

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates_(2020_geog).rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016_(2020_geog).rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population_(2020_geog).rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014_(2020_geog).rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014_(2020_geog).rds"

write_excel <- TRUE

#-------------------------------------------------
timestamp <- format(Sys.time(), "%y-%m-%d_%H%M")
projection_name <- paste0(projection_name,"_",timestamp)
output_dir <- paste0("outputs/trend/2019/",projection_name,"/")

#-------------------------------------------------

mortality_rates <- "input_data/mortality/mort_rates_5yr_trend_2019.rds"

fertility_rates <- "input_data/fertility/fert_rates_5yr_avg_2019.rds"


#-------------------------------------------------

int_out_flows_rates <- list(
  list(fn = popmodules::calculate_mean_international_rates_or_flows, args=list(popn_mye_path = popn_mye_path,
                                                                               births_mye_path = births_mye_path,
                                                                               flow_or_rate = int_out_flow_or_rate,
                                                                               component_path = int_out_mye_path,
                                                                               last_data_year = int_out_last_data_year,
                                                                               n_years_to_avg = int_out_years_to_avg,
                                                                               data_col = "int_out",
                                                                               first_proj_yr = first_proj_yr,
                                                                               n_proj_yr = n_proj_yr,
                                                                               rate_cap = int_out_rate_cap))
)

int_flows_loc <- "input_data/mye/2019/"

int_in  <- list('2020' = list(path = paste0(int_flows_loc,"int_in_10yr_avg_2019_70pc.rds"),
                              transition = F),
                '2021' = list(path = paste0(int_flows_loc,"int_in_10yr_avg_2019_50pc.rds"),
                              transition = T),
                '2024' = list(path = paste0(int_flows_loc,"int_in_increased_ldn_share.rds"),
                              transition = F))

#-----------------------------------------------------

dom_rates_loc <- "input_data/domestic_migration/processed_rates/"

domestic_rates <- list('2020' = list(path = paste0(dom_rates_loc,"dom_rates_10yr_avg_2019_70pc.rds"),
                                     transition = F),
                       '2021' = list(path = paste0(dom_rates_loc,"dom_rates_10yr_avg_2019_50pc.rds"),
                                     transition = T),
                       '2024' = list(path = paste0(dom_rates_loc,"dom_rates_10yr_avg_2019_increased_ldn_in.rds"),
                                     transition = F))

#-----------------------------------------------------

constraint_fns <- list(list(fn = function() NULL, args = list()))
qa_areas_of_interest <- list("London", "E09000001")

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
  upc_path = upc_path,
  
  mortality_rates = mortality_rates,
  fertility_rates = fertility_rates,
  int_out_flows_rates = int_out_flows_rates,
  int_out_method = int_out_flow_or_rate,
  int_in_flows = int_in_flows,
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
projection <- run_trend_model(config_list)