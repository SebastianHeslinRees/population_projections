#' Create a BPO model config list using standard parameters for the 2021-based
#' projections.
#'
#' @param params A named list of trend model parameters which will overwrite the
#'   standard parameters
#' @param variant Which BPO scenario is being run
#'
#' @return A named config list ready for input into `flexmodel_hl_projection`
#'

standard_bpo_parameters <- function(params, variant){
  
  list2env(params, environment())
  standard <- list()
  
  standard$ahs_mix <- 0.5
  standard$n_proj_yr <- last_proj_yr - first_proj_yr + 1 #20
  standard$output_dir <- paste0("outputs/flexible_area_model/2021_based/bpo/", proj_name)
  data_dir <- "input_data/flexible_area_model/"
  
  #-----------------------------------------------------------------------------
  
  #Set the domestic migration data and paths for the variant projection that's been selected
  
  if(stringr::str_detect(variant, "5")){
    
    standard$in_migration <- list(
      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                    transition = F))
    
    standard$out_migration <- list(
      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                    transition = F))
    
  }
  
  if(stringr::str_detect(variant, "10")){
    
    standard$in_migration <- list(
      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_10yr_avg.rds"),
                    transition = F))
    
    standard$out_migration <- list(
      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_10yr_avg.rds"),
                    transition = F))
    
  }
  
  #-----------------------------------------------------------------------------
  
  #Constraint Path (trend model)
  if(stringr::str_detect(variant, "5")){
    constraint_path <- "outputs/trend/2021/2021_5yr_23-01-03_1558/"
  }
  if(stringr::str_detect(variant, "10")){
    constraint_path <- "outputs/trend/2021/2021_10yr_23-01-03_1558/"
  }
  
  if(stringr::str_detect(variant, "unconstrained")){
    components_to_constrain <-  list(births = F,
                                     deaths = F,
                                     in_migration = F,
                                     out_migration = F,
                                     population = F)
  } else {
    
    components_to_constrain <- list(births = T,
                                    deaths = T,
                                    in_migration = F,
                                    out_migration = F,
                                    population = T)
    
  }
  
  #-----------------------------------------------------------------------------
  
  standard$constraint_list <- list(constraint_path = constraint_path,
                          apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/WD22CD_to_NUTS2.rds",
                          make_constraint_lookup_path = "input_data/flexible_area_model/lookups/LAD_to_NUTS2.rds",
                          mapping = c("constraint_area","year","sex","age"),
                          components = components_to_constrain)
  
  
  standard$external_births <- list(births_path = "input_data/scenario_data/births_mid_22.rds",
                          apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                          mapping = c("gss_code","year","sex","age"))
  
  
  #-------------------------------------------------------------------------------
  
  #These 2 line ensure that any parameters set in advance of this script running
  #replace the standard parameters
  standard <- standard[!names(standard) %in% names(params)]
  list2env(standard, environment())
  
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
                      #dev_trajectory_path = small_area_dev_trajectory_path, 
                      
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
  
  return(config_list)
  
}