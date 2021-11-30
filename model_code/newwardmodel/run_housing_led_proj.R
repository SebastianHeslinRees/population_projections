library(dplyr)
devtools::load_all('model_code/popmodules')

source("model_code/newwardmodel/projection_loop.R")
#source("model_code/newwardmodel/arrange_core_outputs.R")
#source("model_code/newwardmodel/output.R")
source("model_code/newwardmodel/get_constraints.R")


data_dir <- "input_data/new_ward_model/"

#-------------------------------------------------------------------------------
config_list <- list(projection_name = projection_name,
                    first_proj_yr = 2020,
                    n_proj_yr = 31,
                    output_dir = paste0("outputs/newwardmodel/", projection_name),
                    population_path = paste0(data_dir, "ward_population_WD20CD.rds"),
                    deaths_path = paste0(data_dir, "ward_deaths_WD20CD.rds"),
                    births_path = paste0(data_dir, "ward_births_WD20CD.rds"),
                    out_migration_path = paste0(data_dir, "ward_outflow_WD20CD.rds"),
                    in_migration_path = paste0(data_dir, "ward_inflow_WD20CD.rds"),
                    mortality_rates = paste0(data_dir, "mortality_rates_WD20CD.rds"),
                    fertility_rates = paste0(data_dir, "fertility_rates_WD20CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list)
#-------------------------------------------------------------------------------

in_migration <- list(
  '2020' = list(path =  paste0(data_dir, "in_migration_flows_WD20CD_5yr_avg.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "in_migration_flows_WD20CD_5yr_avg.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "in_migration_flows_WD20CD_10yr_avg.rds"),
                transition = F))

out_migration <- list(
  '2020' = list(path =  paste0(data_dir, "out_migration_rates_WD20CD_5yr_avg.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "out_migration_rates_WD20CD_5yr_avg.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "out_migration_rates_WD20CD_10yr_avg.rds"),
                transition = F))

constraint_list <- list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                        mapping = c("gss_code","year","sex","age"),
                        components = list(births = TRUE,
                                          deaths = TRUE,
                                          in_migration = FALSE,
                                          out_migration = FALSE,
                                          population = FALSE))



config_list <- list(first_proj_yr = 2020,
                    n_proj_yr = 31,
                    population_path = paste0(data_dir, "ward_population_WD20CD.rds"),
                    external_trend_path = "outputs/newwardmodel/test_2050/",
                    mortality_rates = paste0(data_dir, "mortality_rates_WD20CD.rds"),
                    fertility_rates = paste0(data_dir, "fertility_rates_WD20CD.rds"),
                    in_migration = in_migration,
                    out_migration = out_migration,
                    constraint_list = constraint_list)

#projection years
first_proj_yr <- config_list$first_proj_yr
last_proj_yr <-  first_proj_yr + config_list$n_proj_yr -1

#Get backseries - 5 secs
message("get backseries")
population <- get_component_from_file(filepath = config_list$population_path, 
                                      max_yr = config_list$first_proj_yr - 1) %>%
  select(year, gss_code, gss_code_ward, age, sex, popn)


component_constraints <- get_data_from_file(
  list(birth_constraint = paste0(config_list$external_trend_path,"births.rds"),
       death_constraint = paste0(config_list$external_trend_path,"deaths.rds"),
       out_migration_constraint = paste0(config_list$external_trend_path,"out_migration.rds"))) #%>%
  #create_constraints()

mortality_rates <- get_component_from_file(filepath = config_list$mortality_rates, 
                                           max_yr = last_proj_yr)

fertility_rates <- get_component_from_file(filepath = config_list$fertility_rates, 
                                           max_yr = last_proj_yr)

in_flow_info <- get_rates_flows_info(config_list$in_migration, first_proj_yr, last_proj_yr)
projected_in_migration <- NULL

out_rate_info <- get_rates_flows_info(config_list$out_migration, first_proj_yr, last_proj_yr)
projected_out_migration <- NULL


#Constraints - 30 secs
if(!is.null(constraint_list)){
  message("get constraints")
  constraint_list <- get_constraints(constraint_list, last_proj_yr)
}
#-------------------------------------------------------------------------------

projection_year <- 2020

projection <- list()
curr_yr_popn <- filter(population, year == first_proj_yr-1)
#for(projection_year in first_proj_yr:last_proj_yr){
  
  curr_yr_fertility <- filter(fertility_rates, year == projection_year)
  curr_yr_mortality <- filter(mortality_rates, year == projection_year)
  
  projected_in_migration <- get_rates_or_flows(projected_in_migration, in_flow_info,
                                               projection_year, first_proj_yr,
                                               col_aggregation = c("year", "gss_code", "gss_code_ward", "sex", "age"),
                                               data_col = "in_flow")
  
  
  curr_yr_in_flows <- filter(projected_in_migration, year == projection_year) %>% 
    mutate(year = projection_year) %>% 
    select(year, gss_code, gss_code_ward, sex, age, in_flow)
  
  projected_out_migration <- get_rates_or_flows(projected_out_migration, out_rate_info,
                                                projection_year, first_proj_yr,
                                                col_aggregation = c("year", "gss_code", "gss_code_ward", "sex", "age"),
                                                data_col = "out_rate")
  
  curr_yr_out_rates <- filter(projected_out_migration, year == projection_year) %>% 
    mutate(year = projection_year) %>% 
    select(year, gss_code, gss_code_ward, sex, age, out_rate)
  
  # curr_yr_ahs <- filter(external_ahs, year == projection_year)
  # curr_yr_households_static <- filter(household_trajectory_static, year == projection_year)
  # curr_yr_households_adjusted <- filter(household_trajectory_adjusted, year == projection_year)
  
  # communal_establishment_population
  # hma_constraint
  # component_constraints
  # external ahs
  # households 1
  # households 2
  # hma_list
  # ahs_cap_year
  # ahs_cap
  # ahs_method
  # ldd_final_yr
  # constraint_projection
  
  if(config_list$constrain_projection){
    curr_yr_hma_constraint <- filter(hma_constraint, year == projection_year)
  }
  curr_yr_constrain <- config_list$constrain_projection | projection_year <= config_list$last_data_yr
  

  projection[[projection_year]] <- projection_loop(start_population = curr_yr_popn,
                                                   fertility_rates = curr_yr_fertility,
                                                   mortality_rates = curr_yr_mortality,
                                                   out_rates = curr_yr_out_rates,
                                                   in_flows = curr_yr_in_flows,
                                                   projection_year = projection_year,
                                                   constraint_list = constraint_list)
  
  projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                    trend_projection = trend_projection[[projection_year]],
                                                    component_constraints = component_constraints,
                                                    hma_constraint = curr_yr_hma_constraint,
                                                    communal_establishment_population = communal_establishment_population,
                                                    external_ahs = curr_yr_ahs,
                                                    households_1 = curr_yr_households_adjusted,
                                                    households_2 = curr_yr_households_adjusted ,
                                                    hma_list = hma_list,
                                                    projection_year = projection_year,
                                                    ahs_cap_year = config_list$ahs_cap_year,
                                                    ahs_cap = ahs_cap,
                                                    ahs_method = config_list$ahs_method,
                                                    ldd_final_yr = config_list$ldd_final_yr,
                                                    constrain_projection = curr_yr_constrain)
  
  ahs_cap <- projection[[projection_year]]$ahs_cap
  curr_yr_popn <- projection[[projection_year]]$population
}






