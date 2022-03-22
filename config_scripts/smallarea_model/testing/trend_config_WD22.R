devtools::load_all('model_code/popmodules')
devtools::load_all('model_code/smallareamodel2')

data_dir <- "input_data/smallarea_model/"
projection_name <- "test_2022_fully_constrained"

# data.frame(gss_code = c(paste0("E0900000",1:9), paste0("E090000",10:33)),
#            constraint_area = c(rep("con 1", 15), rep("con 2", 18))) %>%
#   saveRDS('input_data/smallarea_model/lookups/dummy_hma.rds')

constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("constraint_area", "year","sex","age"),
                        lookup_path = "input_data/smallarea_model/lookups/dummy_hma.rds",
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = T,
                                          out_migration = T,
                                          population = TRUE))

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

config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 31, #31
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "backseries/ward_population_WD22CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD22CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD22CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD22CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD22CD.rds"),
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_WD22CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_WD22CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list,
                    lookup_path = "input_data/smallarea_model/lookups/ward_2022_name_lookup.rds")

#-------------------------------------------------------------------------------

#~3 mins with constraining (2 proj years)
#~8 mins with constraining (31 proj years)
projection <- run_small_area_trend_model(config_list)

