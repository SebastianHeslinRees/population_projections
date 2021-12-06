library(dplyr)
devtools::load_all('model_code/popmodules')

source("model_code/newwardmodel/projection_loop.R")
#source("model_code/newwardmodel/arrange_core_outputs.R")
#source("model_code/newwardmodel/output.R")
source("model_code/newwardmodel/get_constraints.R")


source("model_code/newwardmodel/housing-led_config.R")
source("model_code/newwardmodel/housing_led_core.R")

#-------------------------------------------------------------------------------
#TREND MODEL PREP

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
ahs_cap <- NULL
communal_establishment_population <- readRDS(config_list$communal_est_path)
dwellings <- readRDS(config_list$dev_trajectory_path)
#TODO Convert to dwellings to households
households <- dwellings %>% rename(households = units)

projection <- list()
trend_projection <- list()
curr_yr_popn <- filter(population, year == first_proj_yr-1)
for(projection_year in first_proj_yr:last_proj_yr){
  
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
  
  
  # hma_constraint
  # component_constraints
  # external ahs
  curr_yr_households <- filter(households, year == projection_year)
  # hma_list
  # ahs_cap_year
  # ahs_method
  # ldd_final_yr
  # constraint_projection
  
  if(config_list$constrain_projection){
    curr_yr_hma_constraint <- filter(hma_constraint, year == projection_year)
  }
  curr_yr_constrain <- config_list$constrain_projection | projection_year <= config_list$last_data_yr
  
  
  trend_projection[[projection_year]] <- projection_loop(start_population = curr_yr_popn,
                                                         fertility_rates = curr_yr_fertility,
                                                         mortality_rates = curr_yr_mortality,
                                                         out_rates = curr_yr_out_rates,
                                                         in_flows = curr_yr_in_flows,
                                                         projection_year = projection_year,
                                                         constraint_list = constraint_list)
  
  projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                    trend_projection = trend_projection[[projection_year]],
                                                    #component_constraints = component_constraints,
                                                    #hma_constraint = curr_yr_hma_constraint,
                                                    communal_establishment_population = communal_establishment_population,
                                                    #external_ahs = curr_yr_ahs,
                                                    households_1 = curr_yr_households,
                                                    #households_2 = curr_yr_households_adjusted ,
                                                    #hma_list = hma_list,
                                                    projection_year = projection_year,
                                                    ahs_cap_year = config_list$ahs_cap_year,
                                                    ahs_cap = ahs_cap,
                                                    ahs_method = config_list$ahs_method,
                                                    ldd_final_yr = config_list$ldd_final_yr,
                                                    constrain_projection = curr_yr_constrain)
  
  ahs_cap <- projection[[projection_year]]$ahs_cap
  curr_yr_popn <- projection[[projection_year]]$population
  
}






