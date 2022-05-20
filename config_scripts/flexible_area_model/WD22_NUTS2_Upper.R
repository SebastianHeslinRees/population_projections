library(popmodules)
library(flexibleareamodel)
data_dir <- "input_data/flexible_area_model/"
projection_name <- "SHLAA_Scenario2_WD22"

#Constraints
constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CC_central_upper_21-09-21_1259/",
                        lookup_path = "input_data/flexible_area_model/lookups/NUTS2_hma.rds",
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
                    n_proj_yr = 21, #21
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
                    dev_trajectory_path = paste0(data_dir, "development_data/ward_savills_trajectory_WD22CD.rds"), 
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD22CD.rds"),
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD22CD.rds"),
                    dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"),
                    
                    #settings
                    hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD22CD.rds"),
                    ahs_mix = 0.5,
                    hhr_static_or_projected = "static",
                    lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                    excess_deaths_path = NULL
                    
)

model_output <- run_small_area_hl_model(config_list)

create_excel(config_list$output_dir, "SHLAA Scenario 2 WD22.xlsx", "SHLAA Scenario 2 WD22")
