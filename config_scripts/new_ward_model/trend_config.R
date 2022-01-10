devtools::load_all('model_code/newwardmodel')
data_dir <- "input_data/new_ward_model/"
projection_name <- "test_2"

constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = FALSE,
                                          out_migration = FALSE,
                                          population = FALSE))



in_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_Covid_2020.rds"),
                transition = F),
  '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD20CD_10yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2020' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_Covid_2020.rds"),
                transition = F),
  '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD20CD_10yr_avg.rds"),
                transition = F))

config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "backseries/ward_population_WD20CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD20CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD20CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD20CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD20CD.rds"),
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_WD20CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_WD20CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list)

#-------------------------------------------------------------------------------

#~3 mins with constraining (2 proj years)
#~8 mins with constraining (31 proj years)
projection <- run_small_area_trend_model(config_list)

#-------------------------------------------------------------------------------

components <- projection$detailed_components
summary(components$diff)

summary <- projection$summary

x_00F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 0)
x_06F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 6)
x_23F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 23)
x_90F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 90)
x_23M <- filter(components, gss_code_ward == "E05000026", sex == 'male', age == 23)

neg_pop <- filter(components, popn < 0)
neg_pop_check <- filter(components, sqrt(change^2) > start_popn & change < 0)
neg_pop_other <- setdiff(neg_pop, neg_pop_check)