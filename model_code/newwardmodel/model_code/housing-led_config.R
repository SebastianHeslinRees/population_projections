data_dir <- "input_data/new_ward_model/"
projection_name <- "housing-led_test"

#TREND STUFF
constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = FALSE,
                                          out_migration = FALSE,
                                          population = FALSE))

in_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_Covid_2020.rds"),
                transition = F),
  '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_10yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_Covid_2020.rds"),
                transition = F),
  '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_10yr_avg.rds"),
                transition = F))

config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 22,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "backseries/ward_population_WD20CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD20CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD20CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD20CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD20CD.rds"),
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_WD20CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_WD20CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list,
                    
                    #HOUSING-LED STUFF
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD20CD.rds"),
                    dev_trajectory_path = paste0(data_dir, "development_data/ward_shlaa_trajectory_WD20CD.rds"), 
                    
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD20CD.rds"),
                    
                    hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD20CD.rds"),
                    ahs_mix = 0.8,
                    hhr_static_or_projected = "static"
                    #-------------------------------------------------------------------------------
                    
                    #Not currently used
                    
                    #constrain_projection = FALSE
                    #TODO hma_list = list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33))),
                    
)
