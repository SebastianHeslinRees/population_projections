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
#' @param projection_range Numeric. The years to project. \code{Default 2020:2050}.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv
#' @param csv_name String. The name of the dwelling trajectory csv saved in the
#'
#' @import dplyr
#' @export

run_bpo_projection <- function(bpo_name,
                               trajectory_range,
                               wards,
                               projection_range = 2020:2041,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                               csv_name = bpo_name){
  
  first_proj_yr <- min(projection_range)
  last_proj_yr <- max(projection_range)
  
  assert_that(wards %in% c("WD13","WD22"))
  
  #-----------------------------------------------------------------------------
  
  data_dir <- "input_data/smallarea_model/"
  
  #Set the domestic migration data and paths for the variant projection that's been selected
  if(wards == "WD22"){
    
    in_migration <- list(
      '2020' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2020.rds"),
                    transition = F),
      '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                    transition = F))
    
    out_migration <- list(
      '2020' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2020.rds"),
                    transition = F),
      '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                    transition = F))
  }
  
  if(wards == "WD13"){
    
    in_migration <- list(
      '2020' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2020.rds"),
                    transition = F),
      '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_5yr_avg.rds"),
                    transition = F))
    
    out_migration <- list(
      '2020' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2020.rds"),
                    transition = F),
      '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_5yr_avg.rds"),
                    transition = F))
    
  }
  
  #-----------------------------------------------------------------------------
  
  constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                          mapping = c("gss_code","year","sex","age"),
                          components = list(births = F,
                                            deaths = F,
                                            in_migration = F,
                                            out_migration = F,
                                            population = F),
                          lookup_path = "input_data/smallarea_model/lookups/London_hma.rds")
  
  #-----------------------------------------------------------------------------
  
  #process the csv trajectory into an rds file
  #Also, figure out the gss code for the borough in question
  #browser()
  borough_gss <- bpo_template_to_rds(csv_name = csv_name,
                                     bpo_dir = bpo_dir,
                                     trajectory_range = trajectory_range,
                                     wards)
  
  small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",csv_name,".rds")
  
  #-----------------------------------------------------------------------------
  
  if(wards == "WD22"){
    
    config_list <- list(projection_name = bpo_name,
                        first_proj_yr = first_proj_yr,
                        n_proj_yr = last_proj_yr - first_proj_yr + 1, #22
                        output_dir = paste0("outputs/smallarea_model/", bpo_name),
                        
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
                        
                        #dev trajectory
                        dev_trajectory_path = small_area_dev_trajectory_path, 
                        
                        #housing-led stuff
                        ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD22CD.rds"),
                        communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD22CD.rds"),
                        dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"),
                        
                        #settings
                        hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD22CD.rds"),
                        ahs_mix = 0.8,
                        hhr_static_or_projected = "static",
                        lookup_path = "input_data/smallarea_model/lookups/ward_2022_name_lookup.rds",
                        excess_deaths_path = NULL)
    
  }
  
  if(wards == "WD13"){
    
    config_list <- list(projection_name = bpo_name,
                        first_proj_yr = first_proj_yr,
                        n_proj_yr = 22,#last_proj_yr - first_proj_yr + 1 , #22
                        output_dir = paste0("outputs/smallarea_model/", bpo_name),
                        
                        #backseries
                        population_path = paste0(data_dir, "backseries/ward_population_WD13CD.rds"),
                        deaths_path = paste0(data_dir, "backseries/ward_deaths_WD13CD.rds"),
                        births_path = paste0(data_dir, "backseries/ward_births_WD13CD.rds"),
                        out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD13CD.rds"),
                        in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD13CD.rds"),
                        
                        #rates
                        mortality_rates = paste0(data_dir, "processed/mortality_rates_5yr_trend_WD13CD.rds"),
                        fertility_rates = paste0(data_dir, "processed/fertility_rates_5yr_trend_WD13CD.rds"),
                        in_migration = in_migration,
                        out_migration = out_migration,
                        
                        #constraints
                        constraint_list = constraint_list,
                        
                        #dev trajectory
                        dev_trajectory_path = small_area_dev_trajectory_path, 
                        
                        #housing-led stuff
                        ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD13CD.rds"),
                        communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD13CD.rds"),
                        dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD13CD.rds"),
                        
                        #settings
                        hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD13CD.rds"),
                        ahs_mix = 0.8,
                        hhr_static_or_projected = "static",
                        lookup_path = "input_data/smallarea_model/lookups/ward_2013_name_lookup.rds",
                        excess_deaths_path = "input_data/smallarea_model/processed/excess_covid_deaths_WD13CD.rds")
    
  }
  
  run_small_area_hl_model(config_list)
  proj_name <- str_replace_all(paste(bpo_name, "BPO"), "_", " ")
  wb_name <- bpo_name
  create_excel(config_list$output_dir, wb_name, proj_name, borough_gss)
  
  message("complete")
  
}
