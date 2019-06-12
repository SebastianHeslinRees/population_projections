library(tidyverse)

#core CCM functions
# handles the process of ageing on the population and applying components of change
# core_functions <- "scripts/core_functions.R"
source(core_functions)

# config_paths contains paths to component functions, model inputs, and output locations 
# config_paths <- "component_functions/path_info.R"
# source(config_paths)

##### source component functions ####
#each must contain mandatory functions that return dataframes containing the change to be applied to the population
#optional functions create reporting outputs or to be called to generate inputs to the other component functons

#input-outputfunctions
# these functions handle the reading of the input population data, conversion into the standard form used by the model, and the creation of outputs
# mandatory functions: get_pop_estimates, ft_output_pop, ft_output_coc, get_population_parameters, create_ccm_dataframe
source(io_functions)

#birth functions
# returns a projected number of births 
# mandatory functions: calculate_births
source(birth_functions)

#death functions
# returns projected deaths and projected number of infant deaths (i.e. deaths between birth and age 0)
# mandatory functions: calculate_deaths, caculate_infant_deaths
source(death_functions)

#migration functions:
# returns projected net migration, and projected net migration of infants (i.e. moves made between birth and age 0)
# mandatory functions: calculate_migration, calculate_infant_migration
source(migration_functions)

#visualisation functions
source(visualisation_functions)

#TODO figure out how to best to specify geography of the data and handle inputs on mixed/nested geographies

##### set projection parameters #####
#start and end points of projection
base_year <- 2011
end_year <- 2100

#TODO how should this be scoped?
pop_parameters <- get_population_parameters()

#prepare data ahead of running projection 
input_pop_estimates <- get_pop_estimates(pop_path, "csv")

formatted_pop_estimates <- ft_pop_estimates(input_pop_estimates)

proj_years <- seq(base_year, end_year, by = pop_parameters$interval_size)

#TODO check validity of data and configuration

#initialise lists for population and components of change
# ccm_pop is the dataframe used to hold population data within the model in standardised format
# ccm_coc holds the standard components of change 
# as the model has to be able to handle data with banded age categories, ages are converted into 'age_intervals'
ccm_pop <- setNames(vector("list", length(proj_years)), proj_years)
ccm_coc <- setNames(vector("list", length(proj_years)), proj_years)
ccm_pop[[1]] <- create_pop_ccm(formatted_pop_estimates, base_year)

###### run projection loop ######
# s_pop and e_pop are the populations at the start (s_year) and end (e_year) of the cyle

for(i in 1:(length(proj_years) -1)){
  
  s_year <- proj_years[i]
  e_year <- proj_years[i + 1]
  
  s_pop <- ccm_pop[[i]]
  
  ## calculate components ##
  components <- calculate_components(s_pop)

  #TODO add option for component interaction
  
  ## age-on and apply components ##
  e_pop <- age_on(s_pop, e_year, components)
  
  ccm_pop[[i + 1]] <- e_pop
  ccm_coc[[i + 1]] <- components
}

# create formatted outputs 
population_out <- ft_output_pop(ccm_pop)
components_out <- ft_output_coc(ccm_coc)

#create plots
pop_plots <- create_population_plots(population_out)
component_plots <- create_component_plots(components_out)


#TEMP - testing plot outputs
pop_plots$age_structure
pop_plots$total_pop
component_plots$births
