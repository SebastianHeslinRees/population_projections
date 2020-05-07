# config file for model runs

devtools::load_all("model_code/popmodules")

first_proj_yr <- 2019
n_proj_yr <- 5
projection_name <- "2018_scenario_test"

#rates files
rates_dir <- "input_data/rates_files/"
dom_rate_path <- paste0(rates_dir, "dom_rates.rds")
int_in_path <- paste0(rates_dir, "int_in.rds")
int_out_rate_path <- paste0(rates_dir, "int_out_rate.rds")
fertility_rate_path <- paste0(rates_dir, "fertility.rds")
mortality_rate_path <- paste0(rates_dir, "mortality.rds")
upc_path <- paste0(rates_dir, "upc.rds")

popn_mye_path <- paste0("input_data/mye/2018/population_gla_2019-11-13.rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_gla_2019-11-13.rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla_2019-11-13.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_out.rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_in.rds")
dom_origin_destination_path <- paste0("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds")

domestic_transition_yr <- 2021
int_out_flow_or_rate <- "rate"

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates.rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016.rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population.rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014.rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014.rds"

write_excel <- FALSE

#-------------------------------------------------
timestamp <- format(Sys.time(), "%y-%m-%d_%H%M")
projection_name <- paste0(projection_name,"_",timestamp)
output_dir <- paste0("outputs/trend/2018/",projection_name,"/")

#-------------------------------------------------

#rates functions
mortality_fns <- list(list(fn = readRDS, args = list(mortality_rate_path)))
fertility_fns <- list(list(fn = readRDS, args = list(fertility_rate_path)))
int_in_fns <- list(list(fn = readRDS, args = list(int_in_path)))
int_out_rate_fns <- list(list(fn = readRDS, args = list(int_out_rate_path)))
dom_rate_fns <- list(list(fn = readRDS, args = list(dom_rate_path)))
constraint_fns <- list(list(fn = function() NULL, args = list()))

#----------------------------
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
  output_dir = output_dir,
  mortality_fns = mortality_fns,
  fertility_fns = fertility_fns,
  int_out_fns = int_out_rate_fns,
  int_in_fns = int_in_fns,
  dom_rate_fns = dom_rate_fns,
  domestic_transition_yr = domestic_transition_yr,
  constraint_fns = constraint_fns,
  qa_areas_of_interest = qa_areas_of_interest,
  int_out_method = int_out_flow_or_rate,
  write_excel  = write_excel,
  write_QA = FALSE,
  communal_est_pop_path = communal_est_pop_path,
  ons_stage1_file_path = ons_stage1_file_path,
  ons_stage2_file_path = ons_stage2_file_path,
  dclg_stage1_file_path = dclg_stage1_file_path,
  dclg_stage2_file_path = dclg_stage2_file_path
)

# Save settings
# TODO this isn't super robust and will only run from RStudio - find a smarter way to do it
projdir <- rprojroot::find_root(rprojroot::is_git_root)
copy_dir <- paste0(projdir, "/", output_dir)
dir.create(copy_dir, recursive = TRUE)
this_file <- rstudioapi::getSourceEditorContext()$path
file.copy(this_file, paste0(copy_dir, "config_list_", projection_name, ".R"))

rm(list = setdiff(ls(), "config_list"))

# Run the model
source("model_code/model_scripts/trend/00_control.R")
projection <- run_trend_model(config_list)
log_warnings(paste0(config_list$output_dir, "warnings.txt"))
