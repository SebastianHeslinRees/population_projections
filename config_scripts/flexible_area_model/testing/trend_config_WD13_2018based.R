devtools::load_all('model_code/smallareamodel2')
data_dir <- "input_data/flexible_area_model/"
projection_name <- "trend_2013_2018based"

constraint_list <- list(constraint_path = "outputs/trend/2018/2018_central",
                         mapping = c("gss_code","year","sex","age"),
                         components = list(births = TRUE,
                                           deaths = TRUE,
                                           in_migration = FALSE,
                                           out_migration = FALSE,
                                           population = FALSE))

in_migration <- list(
  '2019' = list(path = paste0(data_dir, "processed/in_migration_flows_WD13CD_2018_10yr.rds"),
  transition = F))

out_migration <- list(
  '2019' = list(path = paste0(data_dir, "processed/out_migration_rates_WD13CD_2018_10yr.rds"),
                transition = F))

config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2019,
                    n_proj_yr = 23,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "backseries/ward_population_WD13CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD13CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD13CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD13CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD13CD.rds"),
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_WD13CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_WD13CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list,
                    lookup_path = "input_data/flexible_area_model/lookups/ward_2013_name_lookup.rds")

#-------------------------------------------------------------------------------

#~3 mins with constraining (2 proj years)
#~8 mins with constraining (31 proj years)
projection <- run_small_area_trend_model(config_list)

#-------------------------------------------------------------------------------
