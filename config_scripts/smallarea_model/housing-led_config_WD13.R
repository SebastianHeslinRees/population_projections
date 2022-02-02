devtools::load_all('model_code/smallareamodel2')
data_dir <- "input_data/smallarea_model/"
projection_name <- "housing-led_test_2013"

#Constraints
constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = FALSE,
                                          out_migration = FALSE,
                                          population = FALSE))

#Migration
in_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2020.rds"),
                transition = F),
  '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_10yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2020.rds"),
                transition = F),
  '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_10yr_avg.rds"),
                transition = F))

#Config
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 1,#22,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    
                    #backseries
                    population_path = paste0(data_dir, "backseries/ward_population_WD13CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD13CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD13CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD13CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD13CD.rds"),
                    
                    #rates
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_5yr_avg_WD13CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_5yr_avg_WD13CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    
                    #constraints
                    constraint_list = constraint_list,
                    
                    #HOUSING-LED STUFF
                    dev_trajectory_path = "input_data/small_area_model/development_data/ward_2020-based_savills.rds",
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD13CD.rds"),
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD13CD.rds"),
                    dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD13CD.rds"),
                    hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD13CD.rds"),
                    
                    #settings
                    ahs_mix = 0.8,
                    hhr_static_or_projected = "static",
                    lookup_path = "input_data/smallarea_model/lookups/ward_2013_name_lookup.rds"
)
 
hl_model <- run_small_area_hl_model(config_list)
