# config file for model runs

first_proj_yr <- 2019
n_proj_yr <- 20
popn_mye_path <- "input_data/mye/2018/population_ons_2019-08-27.rds"
deaths_mye_path <- "input_data/mye/2018/deaths_ons_2019-08-27.rds"
births_mye_path <- "input_data/mye/2018/births_ons_2019-08-27.rds"
int_out_mye_path <- "input_data/mye/2018/international_out_ons_2019-08-27.rds"
int_in_mye_path <- "input_data/mye/2018/international_in_ons_2019-08-27.rds"
outputs_dir = "outputs/trend/2018/"

mortality_fns <- list(
  list(fn = popmodules::get_rate_backseries, args = list(component_mye_path = deaths_mye_path, popn_mye_path = popn_mye_path, births_mye_path = births_mye_path)),
  list(fn = popmodules::project_forward_flat, args = list(first_proj_yr = first_proj_yr, n_proj_yr = n_proj_yr, hold_yr = (first_proj_yr - 2)))
)

fertility_fns <- list(
  list(fn = popmodules::rates_from_file, args = list(filepath = "input_data/fertility/modified_fert_rates_2017_base_trend_med.rds"))
)

int_out_fns <- NA

int_out_rate_fns <- list(
  list(fn = popmodules::rates_from_file, args = list(filepath = "input_data/migration/modified_int_out_rates_2017_base_trend_med.rds"))
)

int_in_fns <- list(
  list(fn = popmodules::component_proj_from_file, args = list(filepath = "input_data/migration/modified_int_in_2017_base_trend_med.rds"))
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
  int_out_fns = int_out_fns,
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
   int_out_fns,
   int_out_rate_fns,
   int_in_fns,
   qa_areas_of_interest)

# Run the model
source("model_code/model_scripts/trend/00_control.R")
run_trend_model(config_list)
