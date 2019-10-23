# config file for model runs

#devtools::load_all("model_code/popmodules")

first_proj_yr <- 2019
n_proj_yr <- 20

datestamp <- "2019-10-22"

popn_mye_path <- paste0("input_data/mye/2018/population_ons_",datestamp,".rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons_",datestamp,".rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons_",datestamp,".rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_ons_",datestamp,".rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_ons_",datestamp,".rds")
dom_out_mye_path <- paste0("input_data/mye/2018/domestic_out_ons_", datestamp, ".rds")
dom_in_mye_path <- paste0("input_data/mye/2018/domestic_in_ons_", datestamp, ".rds")
dom_origin_destination_path <- paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds")
outputs_dir = "outputs/trend/2018/"

mortality_years_to_avg <- 5
mortality_avg_or_trend <- "average"
mortality_last_data_year <- 2018
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"
mortality_npp_variant <- "2018_principal"

fertility_years_to_avg <- 5
fertility_avg_or_trend <- "average"
fertility_last_data_year <- 2018
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"
fertility_npp_variant <- "2018_principal"

#-------------------------------------------------

mortality_fns <- list(

  
  list(fn = popmodules::scaled_mortality_curve, args = list(popn_mye_path = popn_mye_path,
                                                            births_mye_path = births_mye_path,
                                                            deaths_mye_path = deaths_mye_path,
                                                            target_curves_filepath = mortality_curve_filepath,
                                                            last_data_year = mortality_last_data_year,
                                                            years_to_avg = mortality_years_to_avg,
                                                            avg_or_trend = mortality_avg_or_trend,
                                                            data_col = "deaths",
                                                            output_col = "rate")),
  
  list(fn = popmodules::project_rates_npp, args = list(rate_col = "rate",
                                                   rate_trajectory_filepath = mortality_trajectory_filepath,
                                                   first_proj_yr = first_proj_yr,
                                                   n_proj_yr = n_proj_yr,
                                                   npp_var = mortality_npp_variant))
)

#------------------------------------------

fertility_fns <- list(
  list(fn = popmodules::scaled_fertility_curve, args = list(popn_mye_path = popn_mye_path,
                                                           births_mye_path = births_mye_path,
                                                           target_curves_filepath = fertility_curve_filepath,
                                                           last_data_year = fertility_last_data_year,
                                                           years_to_avg = fertility_years_to_avg,
                                                           avg_or_trend = fertility_avg_or_trend,
                                                           data_col = "births",
                                                           output_col = "rate")),
  
  list(fn = popmodules::project_rates_npp, args = list(rate_col = "rate",
                                                   rate_trajectory_filepath = fertility_trajectory_filepath,
                                                   first_proj_yr = first_proj_yr,
                                                   n_proj_yr = n_proj_yr,
                                                   npp_var = fertility_npp_variant))
)


#-----------------------------------------------------


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
                                                          col_partial_match = c("gss_out","gss_in"),
                                                          col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                          col_component = "value")),
  list(fn = popmodules::average_domestic_migration_rates, args = list(last_data_year = first_proj_yr-1,
                                                                      years_to_avg = years_to_avg,
                                                                      col_rate = "rate"))
)

qa_areas_of_interest <- list("London", "E09000001")

# prepare the named list to pass into model
config_list <- list(
  first_proj_yr = first_proj_yr,
  n_proj_yr = n_proj_yr,
  years_to_avg = years_to_avg,
  popn_mye_path = popn_mye_path,
  deaths_mye_path = deaths_mye_path,
  births_mye_path = births_mye_path,
  int_out_mye_path = int_out_mye_path,
  int_in_mye_path = int_in_mye_path,
  dom_out_mye_path = dom_out_mye_path,
  dom_in_mye_path = dom_in_mye_path,
  mortality_curves = mortality_curves,
  fertility_curves = fertility_curves,
  mortality_trends = mortality_trends,
  fertility_trends = fertility_trends,
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
   years_to_avg,
   popn_mye_path, 
   deaths_mye_path, 
   births_mye_path, 
   int_out_mye_path, 
   int_in_mye_path,
   dom_out_mye_path,
   dom_in_mye_path,
   mortality_curves,
   fertility_curves,
   mortality_trends,
   fertility_trends,
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

