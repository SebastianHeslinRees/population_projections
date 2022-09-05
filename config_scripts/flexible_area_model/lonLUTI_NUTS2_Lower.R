devtools::load_all('model_code/popmodules/')
data_dir <- "input_data/flexible_area_model/"
projection_name <- "lonLUTI_NUTS2_Lower_TEST"

#Constraints
constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        lookup_path = "input_data/flexible_area_model/lookups/NUTS2_hma.rds",
                        mapping = c("constraint_area","year","sex","age"),
                        components = list(births = T,
                                          deaths = T,
                                          in_migration = F,
                                          out_migration = F,
                                          population = T))
#Migration
in_migration <- list(
  '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_lonLUTI_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_lonLUTI_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_lonLUTI_5yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_lonLUTI_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_lonLUTI_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_lonLUTI_5yr_avg.rds"),
                transition = F))

#Config
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2021,
                    n_proj_yr = 21, #21
                    output_dir = paste0("outputs/flexible_area_model/", projection_name),
                    
                    #backseries
                    population_path = paste0(data_dir, "backseries/lonLUTI_population.rds"),
                    deaths_path = paste0(data_dir, "backseries/lonLUTI_deaths.rds"),
                    births_path = paste0(data_dir, "backseries/lonLUTI_births.rds"),
                    out_migration_path = paste0(data_dir, "backseries/lonLUTI_outflow.rds"),
                    in_migration_path = paste0(data_dir, "backseries/lonLUTI_inflow.rds"),
                    
                    #rates
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_lonLUTI.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_lonLUTI.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    
                    #constraints
                    constraint_list = constraint_list,
                    
                    #HOUSING-LED STUFF
                    dev_trajectory_path = paste0(data_dir, "development_data/savills_trajectory_lonLUTI.rds"), 
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_lonLUTI.rds"),
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_lonLUTI.rds"),
                    dwellings_to_households_path = paste0(data_dir, "processed/dwelling_2_hh_ratio_lonLUTI.rds"),
                    
                    #settings
                    hhr_path = paste0(data_dir, "processed/hh_rep_rate_lonLUTI.rds"),
                    ahs_mix = 0.5,
                    hhr_static_or_projected = "static",
                    lookup_path = "input_data/flexible_area_model/lookups/lonLUTI_name_lookup.rds",
                    excess_deaths_path = NULL,
                    geog_code_col = "LonLUTI3",
                    geog_name_col = "name"
)

devtools::load_all("model_code/flexibleareamodel/")
model_output <- flexmodel_hl_projection(config_list)

#create_excel(config_list$output_dir, "Croydon.xlsx", "Croydon BPO test", bpo = "E09000008")
#create_excel(config_list$output_dir, "NUTS2 Lower lonLUTI.xlsx", "Test", smallarea = "lonLUTI")



