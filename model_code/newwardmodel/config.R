source("model_code/newwardmodel/run_proj.R")
data_dir <- "input_data/new_ward_model/"
projection_name <- "test"

constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = TRUE,
                                          out_migration = TRUE,
                                          population = TRUE))

config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 11,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "ward_population_WD20CD.rds"),
                    deaths_path = paste0(data_dir, "ward_deaths_WD20CD.rds"),
                    births_path = paste0(data_dir, "ward_births_WD20CD.rds"),
                    out_migration_path = paste0(data_dir, "ward_outflow_WD20CD.rds"),
                    in_migration_path = paste0(data_dir, "ward_inflow_WD20CD.rds"),
                    mortality_rates = paste0(data_dir, "mortality_rates_WD20CD.rds"),
                    fertility_rates = paste0(data_dir, "fertility_rates_WD20CD.rds"),
                    in_mig_flows = paste0(data_dir, "in_migration_flows_WD20CD.rds"),
                    out_mig_rates = paste0(data_dir, "out_migration_rates_WD20CD.rds"),
                    constraint_list = constraint_list)

#-------------------------------------------------------------------------------

#~3 mins with constraining
system.time({
projection <- run_new_ward_model(config_list)
})
#-------------------------------------------------------------------------------

components <- projection$components_df

x_00F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 0)
x_06F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 6)
x_23F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 23)
x_90F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 90)

neg_pop <- filter(components, popn < 0)
neg_pop_check <- filter(components, sqrt(change^2) > start_popn & change < 0)
neg_pop_other <- setdiff(neg_pop, neg_pop_check)