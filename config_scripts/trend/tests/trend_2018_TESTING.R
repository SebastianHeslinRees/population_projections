# config file for model runs

library(popmodules)
library(trendmodel)
library(dplyr)

first_proj_yr <- 2019
n_proj_yr <- 2
projection_name <- "2018_test"

popn_mye_path <- paste0("input_data/mye/2018/population_gla.rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_gla.rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_out.rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_in.rds")
upc_path <- NULL

mortality_years_to_avg <- 5
mortality_avg_or_trend <- "trend"
mortality_last_data_year <- 2018
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves_2018.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"
mortality_npp_variant <- "2018_principal"

fertility_years_to_avg <- 5
fertility_avg_or_trend <- "average"
fertility_last_data_year <- 2018
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves_2018.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"
fertility_npp_variant <- "2018_principal"

int_out_last_data_year <- 2018
int_out_years_to_avg <- 10
int_out_flow_or_rate <- "rate"
int_out_rate_cap <- 0.8

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

write_excel <- TRUE

#-------------------------------------------------

timestamp <- format(Sys.time(), "%y-%m-%d_%H%M")
projection_name <- paste0(projection_name,"_",timestamp)
output_dir <- paste0("outputs/trend/2018/",projection_name,"/")

#-------------------------------------------------

mortality_rates <- list(
  
  
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

fertility_rates <- list(
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


int_flows_loc <- "input_data/mye/2018/"

int_in  <- list('2019' = list(path = paste0(int_flows_loc,"int_in_10yr_avg_2018.rds"),
                              transition = FALSE))

#-----------------------------------------------------

dom_rates_loc <- "input_data/domestic_migration/processed_rates/"

domestic_rates <- list('2019' = list(path = paste0(dom_rates_loc,"dom_rates_10yr_avg_2018.rds"),
                                     transition = FALSE))

#-----------------------------------------------------

# constraint_fns <- list(
#   list(fn = popmodules::get_constraints_from_file, args = list(popn_path = popn_constraint_path,
#                                                                births_path = births_constraint_path,
#                                                                deaths_path = deaths_constraint_path,
#                                                                int_in_path = int_in_constraint_path,
#                                                                int_out_path = int_out_constraint_path,
#                                                                cross_in_path = cross_in_constraint_path,
#                                                                cross_out_path = cross_out_constraint_path))
# )

constraint_fns <- list(list(fn = function() NULL, args = list()))

qa_areas_of_interest <- list("London", "E09000001")

dir.create('input_data/test_data', showWarnings = FALSE)
upc_path <- "input_data/test_data/test_upc.rds"
upc <- readRDS(popn_mye_path) %>% 
  filter(year %in% 2011:2012) %>%
  mutate(year = year + 8) %>%
  rename(upc = popn) %>%
  filter_to_LAs() %>%
  filter(substr(gss_code,1,3)=="E09",
         age %in% 10:15,
         year == 2020) %>% 
  select(year, gss_code, sex, age, upc)
saveRDS(upc, upc_path)

#upc_path <- NULL

# prepare the named list to pass into model
config_list <- list(
  projection_name = projection_name,
  first_proj_yr = first_proj_yr,
  n_proj_yr = n_proj_yr,
  popn_mye_path = popn_mye_path,
  deaths_mye_path = deaths_mye_path,
  births_mye_path = births_mye_path,
  int_out_mye_path = int_out_mye_path,
  int_in_mye_path = int_in_mye_path,
  dom_out_mye_path = dom_out_mye_path,
  dom_in_mye_path = dom_in_mye_path,
  upc_path = upc_path,
  output_dir = output_dir,
  mortality_rates = mortality_rates,
  fertility_rates = fertility_rates,
  int_out_flows_rates = int_out_flows_rates,
  int_in_flows = int_in,
  domestic_rates = domestic_rates,
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

# Run the model
devtools::load_all('model_code/popmodules')
devtools::load_all('model_code/trendmodel')

rm(list = setdiff(ls(), "config_list"))
projection <- run_trend_model(config_list)
