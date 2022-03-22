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
#'
#' @import dplyr
#' @import stringr
#' @export

run_bpo_projection <- function(bpo_name,
                               wards,
                               variant,
                               trajectory_range = 2012:2041,
                               projection_range = 2021:2041,
                               bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                               csv_name = bpo_name){
  
  first_proj_yr <- min(projection_range)
  last_proj_yr <- max(projection_range)
  
  assert_that(wards %in% c("WD13","WD22"))
  assert_that(variant %in% c("lower","upper"))
  
  borough <- str_split(bpo_name, "_WD")[[1]][1]
  scenario <- ifelse(variant == "lower", "scenario1", "scenario2")
  proj_name <- paste(borough, scenario, wards, sep = "_")
  
  #-----------------------------------------------------------------------------
  
  ahs_mix <- 0.5
  n_proj_yr <- last_proj_yr - first_proj_yr + 1 #21
  output_dir <- paste0("outputs/flexible_area_model/bpo/", proj_name)
  
  #-----------------------------------------------------------------------------
  
  data_dir <- "input_data/flexible_area_model/"
  
  #Set the domestic migration data and paths for the variant projection that's been selected
  if(wards == "WD22"){
    
    in_migration <- list(
      '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                    transition = F))
    
    out_migration <- list(
      '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                    transition = F))
  }
  
  if(wards == "WD13"){
    
    in_migration <- list(
      '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_5yr_avg.rds"),
                    transition = F))
    
    out_migration <- list(
      '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2021.rds"),
                    transition = F),
      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2022.rds"),
                    transition = T),
      '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_5yr_avg.rds"),
                    transition = F))
    
  }
  
  #-----------------------------------------------------------------------------
  
  if(wards == "WD22"){
    excess_deaths <- "input_data/flexible_area_model/processed/excess_covid_deaths_WD22CD.rds"
  }
  if(wards == "WD13"){
    excess_deaths <- "input_data/flexible_area_model/processed/excess_covid_deaths_WD13CD.rds"
  }
  
  #-----------------------------------------------------------------------------
  
  if(variant == "lower"){
    constraint_path <- "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/"
  }
  
  if(variant == "upper"){
    constraint_path <- "outputs/trend/2020/2020_CC_central_upper_21-09-21_1259/"
  }
  
  #-----------------------------------------------------------------------------
  
  constraint_list <- list(constraint_path = constraint_path,
                          lookup_path = "input_data/flexible_area_model/lookups/NUTS2_hma.rds",
                          mapping = c("constraint_area","year","sex","age"),
                          components = list(births = T,
                                            deaths = T,
                                            in_migration = F,
                                            out_migration = F,
                                            population = T))
  
  #-----------------------------------------------------------------------------
  
  #process the csv trajectory into an rds file
  #Also, figure out the gss code for the borough in question
  
  borough_gss <- bpo_template_to_rds(csv_name = csv_name,
                                     bpo_dir = bpo_dir,
                                     trajectory_range = trajectory_range,
                                     wards)
  
  small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",csv_name,".rds")
  
  #-----------------------------------------------------------------------------  
  
  if(wards == "WD22"){
    
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
                        excess_deaths_path = excess_deaths)
    
  }
  
  if(wards == "WD13"){
    
    config_list <- list(projection_name = proj_name,
                        first_proj_yr = first_proj_yr,
                        n_proj_yr = n_proj_yr,
                        output_dir = output_dir,
                        
                        #backseries
                        population_path = paste0(data_dir, "backseries/ward_population_WD13CD.rds"),
                        deaths_path = paste0(data_dir, "backseries/ward_deaths_WD13CD.rds"),
                        births_path = paste0(data_dir, "backseries/ward_births_WD13CD.rds"),
                        out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD13CD.rds"),
                        in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD13CD.rds"),
                        
                        #rates
                        mortality_rates = paste0(data_dir, "processed/mortality_rates_WD13CD.rds"),
                        fertility_rates = paste0(data_dir, "processed/fertility_rates_WD13CD.rds"),
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
                        ahs_mix = ahs_mix,
                        hhr_static_or_projected = "static",
                        lookup_path = "input_data/flexible_area_model/lookups/ward_2013_name_lookup.rds",
                        excess_deaths_path = excess_deaths)
    
  }
  
  x <- run_small_area_hl_model(config_list, n_cores = 20)
  rm(x)
  gc()
  
  borough <- .camel(str_replace_all(borough, "_", " "))
  ward_year <- str_replace(wards, "WD", "20")
  scenario <- substr(scenario, 9, 9)
  proj_desc <- paste0(borough, " BPO - Migration scenario ", scenario, " - BPO development data - ", ward_year," ward boundaries")
  
  create_excel(config_list$output_dir, proj_name, proj_desc, borough_gss)
  
  message("complete")
  
}

.camel <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}
