devtools::load_all('model_code/popmodules/')
devtools::load_all('model_code/flexibleareamodel')
data_dir <- "input_data/flexible_area_model/"
ward_2022_name_lookup <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds")
th_wards <- filter(ward_2022_name_lookup, gss_code == "E09000030")$gss_code_ward
base_trajectory <- readRDS("Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/rds/bpo_ward_trajectory_tower_hamlets_WD22.rds")

#-----------

for(x in c(0,100,200,300,400,500,1000)){
  
  projection_name <- paste0("TH_sensitivity_", x, "_unit_uplift")
  dev_trajectory <- paste0("TH_dev_scenarios/",projection_name, "_DEV.rds")
  
  back_dev <- filter(base_trajectory, year < 2020)
  foward_dev <- filter(base_trajectory, year >= 2020)
  
  th_trajectory <- filter(foward_dev, gss_code_ward %in% th_wards) %>% 
    mutate(units = units + x)
  
  new_dev <- filter(foward_dev, !gss_code_ward %in% th_wards) %>% 
    bind_rows(th_trajectory, back_dev)
  
  saveRDS(new_dev, paste0(data_dir, dev_trajectory))
  
  
  #Constraints
  constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                          make_constraint_lookup_path = "input_data/flexible_area_model/lookups/NUTS2_hma.rds",
                          apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/NUTS2_WD22.rds",
                          mapping = c("constraint_area","year","sex","age"),
                          components = list(births = T,
                                            deaths = T,
                                            in_migration = F,
                                            out_migration = F,
                                            population = T))
  #Migration
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
  
  #Config
  config_list <- list(projection_name = projection_name,
                      first_proj_yr = 2021,
                      n_proj_yr = 21,
                      output_dir = paste0("outputs/flexible_area_model/", projection_name),
                      
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
                      dev_trajectory_path = paste0(data_dir, dev_trajectory), 
                      ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD22CD.rds"),
                      communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD22CD.rds"),
                      dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"),
                      
                      #settings
                      hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD22CD.rds"),
                      ahs_mix = 0.8,
                      hhr_static_or_projected = "static",
                      lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                      excess_deaths_path = NULL,
                      geog_code_col = "gss_code_ward",
                      geog_name_col = "ward_name",
                      parallel = FALSE,
                      borough_outputs = TRUE
                      
  )
  
  model_output <- flexmodel_hl_projection(config_list)
  
  create_excel(config_list$output_dir, paste0("th_",x,".xlsx"), projection_name, bpo = "E09000030")
  
}
