# config file for model runs

first_proj_yr <- 2019
n_proj_yr <- 20
popn_mye_path <- "input_data/mye/2018/population_ons_2019-08-09.rds"
deaths_mye_path <- "input_data/mye/2018/deaths_ons_2019-08-09.rds"
births_mye_path <- "input_data/mye/2018/births_ons_2019-08-09.rds"
outputs_dir = "outputs/trend/2018/"

mortality_fns <- list(
  list(fn = popmodules::get_mortality_backseries, args = list(deaths_mye_path = deaths_mye_path, popn_mye_path = popn_mye_path, births_mye_path = births_mye_path)),
  list(fn = popmodules::project_forward_flat, args = list(first_proj_yr = first_proj_yr, n_proj_yr = n_proj_yr, hold_yr = (first_proj_yr - 2)))
)

# prepare the named list to pass into model
config_list <- list(
  first_proj_yr = first_proj_yr,
  n_proj_yr = n_proj_yr,
  popn_mye_path = popn_mye_path,
  deaths_mye_path = deaths_mye_path,
  outputs_dir = outputs_dir,
  mortality_fns = mortality_fns,
  timestamp = format(Sys.time(), "%y-%m-%d_%H%M")
)
rm(first_proj_yr, n_proj_yr, popn_mye_path, deaths_mye_path, outputs_dir, mortality_fns)

# Run the model
source("model_code/model_scripts/trend/00_control.R")
run_trend_model(config_list)
