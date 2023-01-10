library(flexibleareamodel)

data_dir <- "input_data/flexible_area_model/"
projection_name <- "2021_SHLAA_dev_5yr"

#Constraints
constraint_list <- list(constraint_path = "outputs/trend/2021/2021_5yr_23-01-10_1223/",
                        apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/WD22CD_to_NUTS2.rds",
                        make_constraint_lookup_path = "input_data/flexible_area_model/lookups/LAD_to_NUTS2.rds",
                        mapping = c("constraint_area","year","sex","age"),
                        components = list(births = T,
                                          deaths = T,
                                          in_migration = F,
                                          out_migration = F,
                                          population = T))

external_births <- list(births_path = "input_data/scenario_data/births_mid_22.rds",
                        apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                        mapping = c("gss_code","year","sex","age"))



#Migration
in_migration <- list(
  '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                transition = F))

#Config
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2022,
                    n_proj_yr = 2, #20
                    output_dir = paste0("outputs/flexible_area_model/2021_based/", projection_name),
                    
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
                    
                    excess_deaths_path = NULL,
                    external_births = external_births,
                    geog_code_col = "gss_code_ward",
                    geog_name_col = "ward_name",
                    
                    parallel = FALSE,
                    borough_outputs = TRUE
                    
)

# devtools::load_all('model_code/popmodules/')
# devtools::load_all('model_code/flexibleareamodel/')
model_output <- flexmodel_hl_projection(config_list)

create_excel(config_list$output_dir, "SHLAA 5-year Migration.xlsx", "Test Projection 2021-base")
