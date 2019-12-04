#The control will mostly be for reading-in data
#And managing the return from the core loop

#For constraining
#Need to adapt get_constraints_from_file to do this
#Pass to core as a named list
birth_constraint
death_constraint
international_out_constraint
population_constraint

#other data
#use get_component
communal_establishment_population
average_household_size
development_trajectory
dwelling2household_ratio

fertility_rates
mortality_probabilities
fertility_rates
mortality_rates
int_out_flows_rates
int_in_flows
domestic_rates

#For trend model
int_out_method #could this be detected from the input?
constraints = NULL
upc = NULL

#general
projection_year
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

for(projection_year in 2019:2023){
  
  curr_yr_fertility <- filter(fertility_rates, year == projection_year)
  curr_yr_mortality <- filter(mortality_rates, year == projection_year)
  curr_yr_int_out <- filter(int_out_flows_rates, year == projection_year)
  curr_yr_int_in_flows <- int_in_flows %>% filter(year == projection_year)
  
  projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                    fertility_rates = curr_yr_fertility, 
                                                    mortality_rates = curr_yr_mortality,
                                                    int_out_flows_rates = curr_yr_int_out,
                                                    int_in_flows = curr_yr_int_in_flows,
                                                    domestic_rates = domestic_rates,
                                                    int_out_method = config_list$int_out_method,
                                                    npp_constraints = NULL, upc = NULL,
                                                    borough_constraints,
                                                    communal_establishment_population,
                                                    average_household_size,
                                                    development_trajectory,
                                                    dwelling2household_ratio,
                                                    projection_year = projection_year)
  
}



