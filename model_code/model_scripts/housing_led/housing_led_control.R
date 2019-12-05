#config
constraint_data_fns <- get_data_from_file(list(
  birth_constraint = birth_constraint_path,
  death_constraint = death_constraint_path,
  international_out_constraint = int_out_constraint_path,
  population_constraint = popn_constraint_path))

component_rates_fns <- get_data_from_file(list(
  fertility_rates = fert_rates_path,
  mortality_probabilities = mort_probs_path,
  int_out_flows_rates = int_out_path,
  int_in_flows = int_in_path,
  domestic_rates = domestc_rates_path))

#control
housing_led_control <- function(config_list){}

expected_config <- c(constraint_data_fns,
                     component_rates_fns,
                     communal_est_path,
                     dev_trajectory_path,
                     ahs_trajectory_path,
                     dwelling_ratio_path,
                     first_proj_yr)

#The control will mostly be for reading-in data
#And managing the return from the core loop

#For constraining
constraints <- evaluate_fns_list(config_list$constraint_data_fns)

#component rates
component_rates <- evaluate_fns_list(config_list$component_rates_fns)

final_proj_yr <- max(constraints$population_constraint$year)

#other data
#use get_component
communal_establishment_population <- get_component(config_list$communal_est_path, max_yr = final_proj_yr)
average_household_size <- get_component(config_list$ahs_trajectory_path, max_yr = final_proj_yr)
development_trajectory <- get_component(config_list$dev_trajectory_path, max_yr = final_proj_yr)
dwelling2household_ratio <- readRDS(config_list$dwelling_ratio_path) %>% as.data.frame()

#For trend model
int_out_method #could/should this be detected from the input?
npp_constraints = NULL
upc = NULL

#general
offset_or_ratio_method

#this added by BC on 10/05/2019.  Previously file only contained single set of ratios - these could be valid for only the DCLG or ONS model.
#The ratio file needs updating whenever the population estimates, past housing data, or household formation assumptions change
#Have also added the ability to use the alternative methodlogy (which previously was the standard approach) for reconciling population change and dwelling change since 2011
#Now can choose between the *ratio* method and the *offset* method
#the choice of dclg or ons consistent data should be done automatically now.  The choice between ratio and offset approach is for now hardcoded
#TODO remove hardcoding of ratio/offset method and have this defined in the model run scripts

#a script is now in the input folder to create the input file

#choose method
####HARDCODED###
use_offset_method <- FALSE
################

if(use_offset_method){
  
  #offset method swaps out the normal census starting dwellings, but retains the census ratios of households/dwelling
  if(household_model == "ONS"){
    
    ratio <- ratio%>%
      select(gss_code, census_dwellings = ons_dwellings, ratio = census_ratio)
    
  } else {
    
    ratio <- ratio%>%
      select(gss_code, census_dwellings = dclg_dwellings, ratio = census_ratio)
  }
  
  
} else {
  
  #ratio method swaps out the HH/dwelling ratio, but keeps the standard census starting dwellings
  if(household_model == "ONS"){
    ratio <- ratio%>%
      select(gss_code, census_dwellings, ratio = ons_ratio)
  } else {
    ratio <- ratio%>%
      select(gss_code, census_dwellings, ratio = dclg_ratio)
  }
  
}

for(projection_year in first_proj_yr:final_proj_yr){
  
  curr_yr_fertility <- filter(component_rates$fert_rates, year == projection_year)
  curr_yr_mortality <- filter(component_rates$mort_probs, year == projection_year)
  curr_yr_int_out <- filter(component_rates$int_out, year == projection_year)
  curr_yr_int_in_flows <- component_rates$int_in %>% filter(year == projection_year)
  
  projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                    fertility_rates = curr_yr_fertility, 
                                                    mortality_rates = curr_yr_mortality,
                                                    int_out_flows_rates = curr_yr_int_out,
                                                    int_in_flows = curr_yr_int_in_flows,
                                                    domestic_rates = component_rates$domestic_rates,
                                                    int_out_method = config_list$int_out_method,
                                                    npp_constraints = NULL, upc = NULL,
                                                    constraints = constraints,
                                                    communal_establishment_population,
                                                    average_household_size,
                                                    development_trajectory,
                                                    dwelling2household_ratio,
                                                    projection_year = projection_year)
  
}



