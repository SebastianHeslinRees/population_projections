# config file for model runs

first_proj_yr <- 2019
n_proj_yr <- 20

datestamp <- "2019-10-11"

popn_mye_path <- paste0("input_data/mye/2018/population_ons_",datestamp,".rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons_",datestamp,".rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons_",datestamp,".rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_ons_",datestamp,".rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_ons_",datestamp,".rds")
outputs_dir = "outputs/trend/2018/"

mortality_years_to_avg <- 5
mortality_avg_or_trend <- "average"
mortality_last_data_year <- 2017
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"
mortality_npp_variant <- "2018_principal"

mortality_fns <- list(
  list(fn = popmodules::get_rate_backseries, args = list(component_mye_path = deaths_mye_path, popn_mye_path = popn_mye_path, births_mye_path = births_mye_path)),
  list(fn = popmodules::project_forward_flat, args = list(first_proj_yr = first_proj_yr, n_proj_yr = n_proj_yr, hold_yr = (first_proj_yr - 2)))
)

mortality_fns <- list(
  list(fn = popmodules::get_data_for_snpp_rate_chain, args = list(popn_mye_path = popn_mye_path,
                                                                  births_mye_path = births_mye_path,
                                                                  deaths_mye_path = deaths_mye_path,
                                                                  fertility_or_mortality = "mortality")),
  
  list(fn = popmodules::get_scaled_rate_curve, args = list(target_curves_filepath = mortality_curve_filepath,
                                                           last_data_year = mortality_last_data_year,
                                                           years_to_avg = mortality_years_to_avg,
                                                           avg_or_trend = mortality_avg_or_trend,
                                                           data_col = "deaths",
                                                           output_col = "mortality")),
  
  list(fn = popmodules::project_rates, args = list(rate_col = "mortality",
                                                   rate_trajectory_filepath = mortality_trajectory_filepath,
                                                   first_proj_yr = first_proj_yr,
                                                   n_proj_yr = n_proj_yr,
                                                   npp_var = mortality_npp_variant))
)

fertility_fns <- list(
  list(fn = popmodules::component_proj_from_file, args = list(filepath = "input_data/fertility/modified_fert_rates_2017_base_trend_med.rds",
                                                              proj_yrs = first_proj_yr:(first_proj_yr + n_proj_yr - 1),
                                                              col_data = "rate"))
)

int_out_rate_fns <- list(
  list(fn = popmodules::component_proj_from_file, args = list(filepath = "input_data/migration/modified_int_out_rates_2017_base_trend_med.rds",
                                                              proj_yrs = first_proj_yr:(first_proj_yr + n_proj_yr - 1),
                                                              col_data = "rate"))
)

int_in_fns <- list(
  list(fn = popmodules::component_proj_from_file, args = list(filepath = "input_data/migration/modified_int_in_2017_base_trend_med.rds",
                                                              proj_yrs = first_proj_yr:(first_proj_yr + n_proj_yr - 1),
                                                              col_data = "int_in"))
)

qa_areas_of_interest <- list("London", "E09000001")

# prepare the named list to pass into model
config_list <- list(
  first_proj_yr = first_proj_yr,
  n_proj_yr = n_proj_yr,
  popn_mye_path = popn_mye_path,
  deaths_mye_path = deaths_mye_path,
  births_mye_path = births_mye_path,
  int_out_mye_path = int_out_mye_path,
  int_in_mye_path = int_in_mye_path,
  outputs_dir = outputs_dir,
  mortality_fns = mortality_fns,
  fertility_fns = fertility_fns,
  int_out_rate_fns = int_out_rate_fns,
  int_in_fns = int_in_fns,
  qa_areas_of_interest = qa_areas_of_interest,
  timestamp = format(Sys.time(), "%y-%m-%d_%H%M")
)

rm(first_proj_yr, 
   n_proj_yr, 
   popn_mye_path, 
   deaths_mye_path, 
   births_mye_path, 
   int_out_mye_path, 
   int_in_mye_path,
   outputs_dir,
   mortality_fns, 
   fertility_fns, 
   int_out_rate_fns,
   int_in_fns,
   qa_areas_of_interest)

# Run the model
source("model_code/model_scripts/trend/00_control.R")
run_trend_model(config_list)

