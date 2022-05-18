devtools::load_all('model_code/flexibleareamodel')
#devtools::load_all('model_code/popmodules')
data_dir <- "input_data/flexible_area_model/"
projection_name <- "housing-led_2013_FFFFT"

#Constraints
constraint_list <- list(constraint_path = "outputs/trend/2020/2019_comparisson_22-01-31_1033/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = F,
                                          deaths = F,
                                          in_migration = F,
                                          out_migration = F,
                                          population = T),
                        lookup_path = "input_data/flexible_area_model/lookups/london_hma.rds")

#Migration
in_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_4yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_4yr_avg.rds"),
                transition = F))

#Config
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 22,
                    output_dir = paste0("outputs/flexible_area_model/", projection_name),
                    
                    #backseries
                    population_path = paste0(data_dir, "backseries/ward_population_WD13CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD13CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD13CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD13CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD13CD.rds"),
                    
                    #rates
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_4yr_trend_WD13CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_4yr_trend_WD13CD.rds"),
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
                    lookup_path = "input_data/flexible_area_model/lookups/ward_2013_name_lookup.rds"
  
)
 
model_output <- flexmodel_hl_projection(config_list)
