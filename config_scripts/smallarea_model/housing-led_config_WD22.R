devtools::load_all('model_code/popmodules/')
devtools::load_all('model_code/smallareamodel2')
data_dir <- "input_data/smallarea_model/"
projection_name <- "housing-led_test_2022_fully_constrained"

#Constraints
constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        lookup_path = "input_data/smallarea_model/lookups/dummy_hma.rds",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = T,
                                          out_migration = T,
                                          population = TRUE))
#Migration
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

#Config
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 22, #22
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    
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
                    
                    #HOUSING-LED STUFF
                    dev_trajectory_path = paste0(data_dir, "development_data/ward_shlaa_trajectory_WD22CD.rds"), 
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD22CD.rds"),
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD22CD.rds"),
                    dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"),
                    
                    #settings
                    hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD22CD.rds"),
                    ahs_mix = 0.8,
                    hhr_static_or_projected = "static",
                    lookup_path = "input_data/smallarea_model/lookups/ward_2022_name_lookup.rds"
                    
)

model_output <- run_small_area_hl_model(config_list)

