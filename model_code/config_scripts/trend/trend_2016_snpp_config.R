# config file for model runs

devtools::load_all("model_code/popmodules")
devtools::load_all("model_code/trendmodel")

first_proj_yr <- 2017
n_proj_yr <- 25
projection_name <- "2016_snpp"

popn_mye_path <- paste0("input_data/mye/2018/population_ons_",datestamp,".rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons_",datestamp,".rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons_",datestamp,".rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_ons_",datestamp,".rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_ons_",datestamp,".rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_out_", datestamp, ".rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_in_", datestamp, ".rds")
upc_path <- NULL

mortality_years_to_avg <- 5
mortality_avg_or_trend <- "trend"
mortality_last_data_year <- 2016
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves_2018.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"
mortality_npp_variant <- "2016_principal"

fertility_years_to_avg <- 5
fertility_avg_or_trend <- "average"
fertility_last_data_year <- 2016
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves_2018.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"
fertility_npp_variant <- "2016_principal"

int_out_last_data_year <- 2016
int_out_years_to_avg <- 5
int_out_flow_or_rate <- "flow"
int_out_rate_cap <- 0.8

popn_constraint_path <- "input_data/constraints/npp_2016_population_constraint.rds"
births_constraint_path <- "input_data/constraints/npp_2016_fertility_constraint.rds"
deaths_constraint_path <- "input_data/constraints/npp_2016_mortality_constraint.rds"
int_in_constraint_path <- "input_data/constraints/npp_2016_international_in_constraint.rds"
int_out_constraint_path <- "input_data/constraints/npp_2016_international_out_constraint.rds"
cross_in_constraint_path <- "input_data/constraints/npp_2016_cross_border_in_constraint.rds"
cross_out_constraint_path <- "input_data/constraints/npp_2016_cross_border_out_constraint.rds"

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates.rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016.rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population.rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014.rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014.rds"

write_excel <- FALSE


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

int_out_rate_fns <- list(
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


int_flows_loc <- "input_data/mye/2016/"

int_in  <- list('2017' = list(path = paste0(int_flows_loc,"int_in_5yr_avg_2016.rds"),
                              transition = FALSE))

#-----------------------------------------------------

dom_rates_loc <- "input_data/domestic_migration/processed_rates/"

domestic_rates <- list('2017' = list(path = paste0(dom_rates_loc,"dom_rates_5yr_avg_2016.rds"),
                                     transition = FALSE))


constraint_fns <- list(
  list(fn = popmodules::get_data_from_file, args = list(files = list(population_constraint = popn_constraint_path,
                                                                     
                                                                     births_constraint = births_constraint_path,
                                                                     deaths_constraint = deaths_constraint_path,
                                                                     international_in_constraint = int_in_constraint_path,
                                                                     international_out_constraint = int_out_constraint_path,
                                                                     cross_border_in_constraint = cross_in_constraint_path,
                                                                     cross_border_out_constraint = cross_out_constraint_path)))
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
  output_dir = output_dir,
  mortality_fns = mortality_fns,
  fertility_fns = fertility_fns,
  int_out_rate_fns = int_out_rate_fns,
  int_in_fns = int_in_fns,
  dom_rate_fns = dom_rate_fns,
  constraint_fns = constraint_fns,
  qa_areas_of_interest = qa_areas_of_interest,
  write_excel  = write_excel,
  communal_est_pop_path = communal_est_pop_path,
  ons_stage1_file_path = ons_stage1_file_path,
  ons_stage2_file_path = ons_stage2_file_path,
  dclg_stage1_file_path = dclg_stage1_file_path,
  dclg_stage2_file_path = dclg_stage2_file_path,
  int_out_flow_or_rate = int_out_flow_or_rate,
  projection_name = projection_name,
  timestamp = format(Sys.time(), "%y-%m-%d_%H%M")
)

rm(list = setdiff(ls(), "config_list"))

# Save settings
# TODO this isn't super robust and will only run from RStudio - find a smarter way to do it
if (!grepl("/$", config_list$output_dir)) config_list$output_dir <- paste0(config_list$output_dir, "/")
projdir <- rprojroot::find_root(rprojroot::is_git_root)
copy_dir <- paste0(projdir, "/", config_list$output_dir, config_list$projection_name)
dir.create(copy_dir, recursive = TRUE, showWarnings = FALSE)
this_file <- rstudioapi::getSourceEditorContext()$path
file.copy(this_file, paste0(copy_dir, "/config_list_", config_list$timestamp, ".R"))

# Run the model

projection <- run_trend_model(config_list)

