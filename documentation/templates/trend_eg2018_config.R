# config file for model runs

first_proj_yr <- 2019
n_proj_yr <- 20
n_dom_averaging_yr <- 5
popn_mye_path <- "input_data/mye/2018/population_ons_2019-08-27.rds"
deaths_mye_path <- "input_data/mye/2018/deaths_ons_2019-08-27.rds"
births_mye_path <- "input_data/mye/2018/births_ons_2019-08-27.rds"
int_out_mye_path <- "input_data/mye/2018/international_out_ons_2019-08-27.rds"
int_in_mye_path <- "input_data/mye/2018/international_in_ons_2019-08-27.rds"
dom_out_mye_path <- "input_data/mye/2018/domestic_out_ons_2019-08-27.rds"
dom_in_mye_path <- "input_data/mye/2018/domestic_in_ons_2019-08-27.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2018/domestic_migration_ons_2019-10-11.rds"
outputs_dir = "outputs/trend/2018/"

mortality_fns <- list(
  list(fn = popmodules::get_rate_backseries, args = list(component_mye_path = deaths_mye_path, popn_mye_path = popn_mye_path, births_mye_path = births_mye_path)),
  list(fn = popmodules::project_forward_flat, args = list(first_proj_yr = first_proj_yr, n_proj_yr = n_proj_yr, hold_yr = (first_proj_yr - 2)))
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

dom_rate_fns <- list(
  list(fn = popmodules::get_rate_backseries, args = list(component_mye_path = dom_origin_destination_path,
                                                          popn_mye_path = popn_mye_path,
                                                          births_mye_path = births_mye_path,
                                                          col_partial_match = "gss_in",
                                                          col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                          col_component = "value")),
  list(fn = popmodules::average_past_rates, args = list(col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                        n_years = n_dom_averaging_yr))
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
  dom_out_mye_path = dom_out_mye_path,
  dom_in_mye_path = dom_in_mye_path,
  dom_origin_destination_path = dom_origin_destination_path,
  outputs_dir = outputs_dir,
  mortality_fns = mortality_fns,
  fertility_fns = fertility_fns,
  int_out_rate_fns = int_out_rate_fns,
  int_in_fns = int_in_fns,
  dom_rate_fns = dom_rate_fns,
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
   dom_out_mye_path,
   dom_in_mye_path,
   dom_origin_destination_path,
   outputs_dir,
   mortality_fns, 
   fertility_fns, 
   int_out_rate_fns,
   int_in_fns,
   dom_rate_fns,
   qa_areas_of_interest)

# Run the model
source("model_code/model_scripts/trend/00_control.R")
run_trend_model(config_list)
