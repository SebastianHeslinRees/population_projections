# config file for model runs

devtools::load_all("model_code/popmodules")

first_proj_yr <- 2019
n_proj_yr <- 2
projection_name <- "test"

popn_mye_path <- paste0("input_data/mye/2018/population_gla_2019-11-14.rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_gla_2019-11-14.rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla_2019-11-14.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_out.rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_in.rds")
dom_origin_destination_path <- paste0("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds")
upc_path <- NULL
outputs_dir <- "outputs/trend/2018/"

mortality_years_to_avg <- 1
mortality_avg_or_trend <- "average"
mortality_last_data_year <- 2018
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"
mortality_npp_variant <- "2018_principal"

fertility_years_to_avg <- 1
fertility_avg_or_trend <- "average"
fertility_last_data_year <- 2018
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"
fertility_npp_variant <- "2018_principal"

int_out_last_data_year <- 2018
int_out_years_to_avg <- 2
int_out_method <- "rate"
int_out_rate_cap <- 0.8

int_in_last_data_year <- 2018
int_in_years_to_avg <- 2
int_in_flow_or_rate <- "flow"

dom_mig_last_data_year <- 2018
dom_mig_years_to_avg <- 2

popn_constraint_path <- "input_data/constraints/npp_2018_population_constraint.rds"
births_constraint_path <- "input_data/constraints/npp_2018_fertility_constraint.rds"
deaths_constraint_path <- "input_data/constraints/npp_2018_mortality_constraint.rds"
int_in_constraint_path <- "input_data/constraints/npp_2018_international_in_constraint.rds"
int_out_constraint_path <- "input_data/constraints/npp_2018_international_out_constraint.rds"
cross_in_constraint_path <- "input_data/constraints/npp_2018_cross_border_in_constraint.rds"
cross_out_constraint_path <- "input_data/constraints/npp_2018_cross_border_out_constraint.rds"

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates.rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016.rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population.rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014.rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014.rds"

write_excel <- FALSE
write_QA <- FALSE



#-------------------------------------------------


mortality_fns <- list(
  
  
  list(fn = popmodules::scaled_mortality_curve, args = list(popn_mye_path = popn_mye_path,
                                                            births_mye_path = births_mye_path,
                                                            deaths_mye_path = deaths_mye_path,
                                                            target_curves_filepath = mortality_curve_filepath,
                                                            last_data_year = mortality_last_data_year,
                                                            n_years_to_avg = mortality_years_to_avg,
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
                                                            n_years_to_avg = fertility_years_to_avg,
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

int_out_fns <- list(
  list(fn = popmodules::calculate_mean_international_rates_or_flows, args=list(popn_mye_path = popn_mye_path,
                                                                               births_mye_path = births_mye_path,
                                                                               flow_or_rate = int_out_method,
                                                                               component_path = int_out_mye_path,
                                                                               last_data_year = int_out_last_data_year,
                                                                               n_years_to_avg = int_out_years_to_avg,
                                                                               data_col = "int_out",
                                                                               first_proj_yr = first_proj_yr,
                                                                               n_proj_yr = n_proj_yr,
                                                                               rate_cap = int_out_rate_cap))
)


int_in_fns <- list(
  list(fn = popmodules::calculate_mean_international_rates_or_flows, args=list(popn_mye_path = popn_mye_path,
                                                                               births_mye_path = births_mye_path,
                                                                               flow_or_rate = int_in_flow_or_rate,
                                                                               component_path = int_in_mye_path,
                                                                               last_data_year = int_in_last_data_year,
                                                                               n_years_to_avg = int_in_years_to_avg,
                                                                               data_col = "int_in",
                                                                               first_proj_yr = first_proj_yr,
                                                                               n_proj_yr = n_proj_yr))
)

dom_rate_fns <- list(
  list(fn = popmodules::get_rate_backseries, args = list(component_mye_path = dom_origin_destination_path,
                                                         popn_mye_path = popn_mye_path,
                                                         births_mye_path = births_mye_path,
                                                         years_backseries = (first_proj_yr - dom_mig_years_to_avg):(first_proj_yr - 1),
                                                         col_partial_match = c("gss_out","gss_in"),
                                                         col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                         col_component = "value",
                                                         rate_cap = NULL)),
  
  list(fn = popmodules::calculate_mean_domestic_rates, args = list(last_data_year = dom_mig_last_data_year,
                                                                   n_years_to_avg = dom_mig_years_to_avg,
                                                                   col_rate = "rate",
                                                                   rate_cap = 0.8))
)

constraint_fns <- list(
  list(fn = popmodules::get_data_from_file, args = list(args = list(population_constraint = popn_constraint_path,
                                                        births_constraint = births_constraint_path,
                                                        deaths_constraint = deaths_constraint_path,
                                                        international_in_constraint = int_in_constraint_path,
                                                        international_out_constraint = int_out_constraint_path,
                                                        cross_border_in_constraint = cross_in_constraint_path,
                                                        cross_border_out_constraint = cross_out_constraint_path)))
)


#TODO figure out the best way to get a null value when we don't want to constrain
#constraint_fns <- list(list(fn = function() NULL, args = list()))

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
  upc_path = upc_path,
  outputs_dir = outputs_dir,
  mortality_fns = mortality_fns,
  fertility_fns = fertility_fns,
  int_out_fns = int_out_fns,
  int_in_fns = int_in_fns,
  dom_rate_fns = dom_rate_fns,
  constraint_fns = constraint_fns,
  qa_areas_of_interest = qa_areas_of_interest,
  int_out_method = int_out_method,
  write_excel  = write_excel,
  write_QA = write_QA,
  communal_est_pop_path = communal_est_pop_path,
  ons_stage1_file_path = ons_stage1_file_path,
  ons_stage2_file_path = ons_stage2_file_path,
  dclg_stage1_file_path = dclg_stage1_file_path,
  dclg_stage2_file_path = dclg_stage2_file_path,
  projection_name = projection_name,
  timestamp = format(Sys.time(), "%y-%m-%d_%H%M")
)

rm(list = setdiff(ls(), "config_list"))

# Save settings
# TODO this isn't super robust and will only run from RStudio - find a smarter way to do it
if (!grepl("/$", config_list$outputs_dir)) config_list$outputs_dir <- paste0(config_list$outputs_dir, "/")
projdir <- rprojroot::find_root(rprojroot::is_git_root)
copy_dir <- paste0(projdir, "/", config_list$outputs_dir, config_list$projection_name)
dir.create(copy_dir, recursive = TRUE)
this_file <- rstudioapi::getSourceEditorContext()$path
file.copy(this_file, paste0(copy_dir, "/config_list_", config_list$timestamp, ".R"))


# Run the model
source("model_code/model_scripts/trend/00_control.R")
projection <- run_trend_model(config_list)

