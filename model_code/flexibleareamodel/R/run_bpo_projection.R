#' Run the borough and ward housing-led models using a BPO dwelling trajectory
#'
#' A wrapper for run_borough_and_ward_projection. Takes as input a csv in the
#' of the GLA housing trajectory template for any borough and converts it into
#' an rds in tidy format. Run the borough housing-led model and the ward small
#' area model using a set of standard model parameters.
#'
#' @param bpo_name String. The name of the dwelling trajectory csv saved in the
#'   folder \code{bpo_dir} folder. The output projection will also have this name.
#' @param trajectory_range Numeric or NULL. The years covered by the input trajectory.
#'  Default 2020:2041.
#' @param wards String. Either WD13 or WD22
#' @param variant String. Either lower or upper
#' @param projection_range Numeric. The years to project. \code{Default 2020:2050}.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv
#' @param csv_name String. The name of the dwelling trajectory csv saved in the
#' @param fert_scenario String. NPP fertility scenario (principal, high, low)
#'
#' @import dplyr
#' @import stringr
#' @export

run_bpo_projection <- function(bpo_name,
                               wards,
                               variant,
                               trajectory_range = 2012:2041,
                               projection_range = 2022:2041,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2021_based/",
                               csv_name = bpo_name){
  
  first_proj_yr <- min(projection_range)
  last_proj_yr <- max(projection_range)
  
  assert_that(variant %in% c("lower","upper","TEST"))
  
  borough <- bpo_name
  scenario <- case_when(variant == "lower" ~ "scenario1",
                        variant == "upper" ~ "scenario2",
                        TRUE ~ "TEST")
  proj_name <- paste(borough, scenario, sep = "_")
  
  #-----------------------------------------------------------------------------
  
  ahs_mix <- 0.5
  n_proj_yr <- last_proj_yr - first_proj_yr + 1 #20
  output_dir <- paste0("outputs/flexible_area_model/2021_based/bpo/", proj_name)
  data_dir <- "input_data/flexible_area_model/"
  
  #-----------------------------------------------------------------------------
  
  #Set the domestic migration data and paths for the variant projection that's been selected
  
  in_migration <- list(
    '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                  transition = F))
  
  out_migration <- list(
    '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                  transition = F))
  
  
  
  #-----------------------------------------------------------------------------
  
  #Constraint Path (trend model)
  if(variant == "TEST"){
    constraint_path <- "outputs/trend/2021/2021_TEST_22-11-28_1522/"
  }
  
  if(variant == "lower"){
    constraint_path <- ""
  }
  if(variant == "upp"){
    constraint_path <- ""
  }
  
  #-----------------------------------------------------------------------------
  
  constraint_list <- list(constraint_path = constraint_path,
                          apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/WD22CD_to_NUTS2.rds",
                          make_constraint_lookup_path = "input_data/flexible_area_model/lookups/LAD_to_NUTS2.rds",
                          mapping = c("constraint_area","year","sex","age"),
                          components = list(births = T,
                                            deaths = T,
                                            in_migration = F,
                                            out_migration = F,
                                            population = T))
  
  external_births <- list(births_path = "input_data/scenario_data/2022_births_dummy_file.rds",
                          apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                          mapping = c("gss_code","year","sex","age"))
  
  
  #-----------------------------------------------------------------------------
  
  # Process the csv trajectory into an rds file
  # And get the gss code for the borough in question
  
  borough_gss <- bpo_template_to_rds(csv_name = csv_name,
                                     bpo_dir = bpo_dir,
                                     trajectory_range = trajectory_range)
  
  small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",csv_name,".rds")
  
  #-----------------------------------------------------------------------------  
  
  config_list <- list(projection_name = proj_name,
                      first_proj_yr = first_proj_yr,
                      n_proj_yr = n_proj_yr,
                      output_dir = output_dir,
                      
                      #backseries
                      population_path = paste0(data_dir, "backseries/ward_population_WD22CD.rds"),
                      deaths_path = paste0(data_dir, "backseries/ward_deaths_WD22CD.rds"),
                      births_path = paste0(data_dir, "backseries/ward_births_WD22CD.rds"),
                      out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD22CD.rds"),
                      in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD22CD.rds"),
                      
                      #rates
                      mortality_rates = paste0(data_dir, "processed/mortality_rates_WD22CD.rds"),
                      fertility_rates = paste0(data_dir, "processed/fertility_rates_WD22CD.rds"),
                      in_migration = in_migration,
                      out_migration = out_migration,
                      
                      #constraints
                      constraint_list = constraint_list,
                      external_births = external_births,
                      
                      #dev trajectory
                      dev_trajectory_path = small_area_dev_trajectory_path, 
                      
                      #housing-led stuff
                      ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD22CD.rds"),
                      communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD22CD.rds"),
                      dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"),
                      
                      #settings
                      hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD22CD.rds"),
                      ahs_mix = ahs_mix,
                      hhr_static_or_projected = "static",
                      lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                      excess_deaths_path = NULL,
                      geog_code_col = "gss_code_ward",
                      geog_name_col = "ward_name",
                      parallel = FALSE,
                      borough_outputs = TRUE)
  
  
  
  
  x <- flexmodel_hl_projection(config_list, n_cores = 20)
  rm(x)
  gc()
  
  borough <- .camel(str_replace_all(borough, "_", " "))
  scenario <- case_when(variant == "lower" ~ " Sceanrio 1",
                        variant == "upper" ~ "Scenario 2",
                        TRUE ~ "TEST")
  proj_desc <- paste0(borough, " BPO - Migration ", scenario, " - BPO development data - 2022 ward boundaries")
  
  create_excel(config_list$output_dir, proj_name, proj_desc, borough_gss)
  
  message("complete")
  
}
