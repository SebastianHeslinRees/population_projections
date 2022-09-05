devtools::load_all('model_code/popmodules/')
devtools::load_all('model_code/flexibleareamodel')
data_dir <- "input_data/flexible_area_model/"
projection_name <- "WD13_NUTS2_Lower_TEST_NEW"

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

#Config
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2021,
                    n_proj_yr = 21, #21
                    output_dir = paste0("outputs/flexible_area_model/", projection_name),
                    
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
                    
                    #HOUSING-LED STUFF
                    dev_trajectory_path = paste0(data_dir, "development_data/ward_savills_trajectory_WD13CD.rds"), 
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD13CD.rds"),
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD13CD.rds"),
                    dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD13CD.rds"),
                    
                    #settings
                    hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD13CD.rds"),
                    ahs_mix = 0.5,
                    hhr_static_or_projected = "static",
                    lookup_path = "input_data/flexible_area_model/lookups/ward_2013_name_lookup.rds",
                    excess_deaths_path = "input_data/flexible_area_model/processed/excess_covid_deaths_WD13CD.rds",
                    geog_code_col = "gss_code_ward",
                    geog_name_col = "ward_name"
)

devtools::load_all("model_code/flexibleareamodel/")
model_output <- flexmodel_hl_projection(config_list)

#create_excel(config_list$output_dir, "Croydon.xlsx", "Croydon BPO test", bpo = "E09000008")
create_excel(config_list$output_dir, "NUTS2 Lower WD13.xlsx", "Test")



