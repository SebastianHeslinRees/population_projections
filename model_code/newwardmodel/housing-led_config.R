data_dir <- "input_data/new_ward_model/"
projection_name <- "housing-led_test"

#TREND STUFF
constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = FALSE,
                                          deaths = FALSE,
                                          in_migration = FALSE,
                                          out_migration = FALSE,
                                          population = FALSE))


in_migration <- list(
  '2020' = list(path =  paste0(data_dir, "in_migration_flows_WD20CD_5yr_avg.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "in_migration_flows_WD20CD_5yr_avg.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "in_migration_flows_WD20CD_10yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2020' = list(path =  paste0(data_dir, "out_migration_rates_WD20CD_5yr_avg.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "out_migration_rates_WD20CD_5yr_avg.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "out_migration_rates_WD20CD_10yr_avg.rds"),
                transition = F))

config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 22,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "ward_population_WD20CD.rds"),
                    deaths_path = paste0(data_dir, "ward_deaths_WD20CD.rds"),
                    births_path = paste0(data_dir, "ward_births_WD20CD.rds"),
                    out_migration_path = paste0(data_dir, "ward_outflow_WD20CD.rds"),
                    in_migration_path = paste0(data_dir, "ward_inflow_WD20CD.rds"),
                    mortality_rates = paste0(data_dir, "mortality_rates_WD20CD.rds"),
                    fertility_rates = paste0(data_dir, "fertility_rates_WD20CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list,
                    
                    #HOUSING-LED STUFF
                    
                    external_trend_path = "outputs/newwardmodel/test_2050/",
                    communal_est_path = "input_data/small_area_model/ward_data/ward_communal_establishment_population.rds",
                    dev_trajectory_path = "input_data/small_area_model/development_data/ward_2020-based_savills.rds", 
                    
                    
                    
                    ldd_backseries_path = "input_data/small_area_model/development_data/ldd_backseries_dwellings_ward.rds",
                    
                    hhr_path = paste0(data_dir, "ward_HHR.rds"),
                    ahs_method = 0.8,
                    #-------------------------------------------------------------------------------
                    #Variables
                    
                    constrain_projection = FALSE,
                    #TODO hma_list = list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33))),
                    
                    ahs_cap_year = 2020,
                    #ahs_method = 0.8,
                    
                    ldd_final_yr = 2019,
                    last_data_yr = 2019
                  
)
