# config file for model runs

first_proj_yr <- 2019
n_proj_yr <- 20
years_to_avg <- 5
popn_mye_path <- "input_data/mye/2018/population_ons_2019-10-18.rds"
deaths_mye_path <- "input_data/mye/2018/deaths_ons_2019-10-18.rds"
births_mye_path <- "input_data/mye/2018/births_ons_2019-10-18.rds"
int_out_mye_path <- "input_data/mye/2018/international_out_ons_2019-10-18.rds"
int_in_mye_path <- "input_data/mye/2018/international_in_ons_2019-10-18.rds"
dom_out_mye_path <- "input_data/mye/2018/domestic_out_ons_2019-10-18.rds"
dom_in_mye_path <- "input_data/mye/2018/domestic_in_ons_2019-10-18.rds"
mortality_curves <- "input_data/mortality/ons_asmr_curves.rds"
fertility_curves <- "input_data/fertility/ons_asfr_curves.rds"
mortality_trends <- "input_data/mortality/npp_mortality_trend.rds"
fertility_trends <- "input_data/fertility/npp_fertility_trend.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2018/domestic_migration_ons_2019-10-18.rds"
outputs_dir = "outputs/trend/2018/"

mortality_fns <- list(
  list(fn = popmodules::initial_year_rate, args = list(population = c(popn_mye_path, births_mye_path),
                                                       component_data = deaths_mye_path,
                                                       target_curves = mortality_curves,
                                                       last_data_year = first_proj_yr - 1,
                                                       years_to_avg = years_to_avg,
                                                       avg_or_trend = "average",
                                                       data_col = "deaths",
                                                       output_col = "rate")),
  list(fn = popmodules::project_rates, args = list(rate_col = "rate",
                                                   rate_trajectory = mortality_trends,
                                                   first_proj_yr = first_proj_yr,
                                                   n_proj_yr = n_proj_yr,
                                                   npp_var = "2016_principal"))
  )

fertility_fns <- list(
  list(fn = popmodules::initial_year_rate, args = list(population = popn_mye_path,
                                                       component_data = births_mye_path,
                                                       target_curves = fertility_curves,
                                                       last_data_year = first_proj_yr - 1,
                                                       years_to_avg = years_to_avg,
                                                       avg_or_trend = "average",
                                                       data_col = "births",
                                                       output_col = "rate")),
  list(fn = popmodules::project_rates, args = list(rate_col = "rate",
                                                   rate_trajectory = fertility_trends,
                                                   first_proj_yr = first_proj_yr,
                                                   n_proj_yr = n_proj_yr,
                                                   npp_var = "2016_principal"))
  )

int_out_rate_fns <- list(
  list(fn = popmodules::component_proj_from_file, args = list(filepath = "input_data/migration/modified_int_out_rates_2017_base_trend_med.rds",
                                                              proj_yrs = first_proj_yr:(first_proj_yr + n_proj_yr - 1),
                                                              col_data = "rate")),
  list(fn = popmodules::recode_gss_to_2011, args = list(col_aggregation = c("gss_code", "year" ,"age", "sex"),
                                                        fun = list(mean)))
  )

int_in_fns <- list(
  list(fn = popmodules::component_proj_from_file, args = list(filepath = "input_data/migration/modified_int_in_2017_base_trend_med.rds",
                                                              proj_yrs = first_proj_yr:(first_proj_yr + n_proj_yr - 1),
                                                              col_data = "int_in")),
  list(fn = popmodules::recode_gss_to_2011, args = list(col_aggregation = c("gss_code", "year" ,"age", "sex"),
                                                        fun = list(mean)))
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
 