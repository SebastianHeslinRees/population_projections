create_constraints <- function(dfs, col_aggregation=c("year","gss_code")){
  
  for(i in seq(dfs)){
    
    nm <- names(dfs[[i]])[ncol(dfs[[i]])]
    dfs[[i]] <- dfs[[i]] %>%
      dtplyr::lazy_dt() %>%
      group_by_at(col_aggregation) %>%
      summarise(value = sum(!!sym(nm))) %>%
      rename(!!nm := value) %>%
      as.data.frame()
  }
  
  return(dfs)
}


#config
population_backseries_path <- "outputs/trend/2018/2018_central/population_19-11-13_2056.rds"

birth_constraint_path <- "outputs/trend/2018/2018_central/births_19-11-13_2056.rds"
death_constraint_path <- "outputs/trend/2018/2018_central/deaths_19-11-13_2056.rds"
int_out_constraint_path <- "outputs/trend/2018/2018_central/int_out_19-11-13_2056.rds"
popn_constraint_path <- "outputs/trend/2018/2018_central/population_19-11-13_2056.rds"

fert_rates_path <- "outputs/trend/2018/2018_central/fertility_rates_19-11-13_2056.rds"
mort_rates_path <- "outputs/trend/2018/2018_central/mortality_rates_19-11-13_2056.rds"
int_out_path <- "outputs/trend/2018/2018_central/int_out_rates_19-11-13_2056.rds"
int_in_path <- "outputs/trend/2018/2018_central/int_in_19-11-13_2056.rds"
domestic_rates_path <- "outputs/trend/2018/2018_central/domestic_rates_19-11-13_2056.rds"

communal_est_path <- "outputs/trend/2018/2018_central/ons_communal_est_popn_19-11-13_2056.rds"
dev_trajectory_path <- NA
ahs_trajectory_path <- NA
dwelling_ratio_path <- NA

hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
first_proj_yr <- 2019

constraint_data_fns <- list(
  list(fn = popmodules::get_data_from_file,
       args = list(
         list(
           birth_constraint = birth_constraint_path,
           death_constraint = death_constraint_path,
           international_out_constraint = int_out_constraint_path))),
  list(fn = create_constraints,
       args = list(col_aggregation = c("year","gss_code"))))

component_rates_fns <- list(
  list(fn = popmodules::get_data_from_file,
       args= list(
         list(
           fertility_rates = fert_rates_path,
           mortality_rates = mort_rates_path,
           int_out_flows_rates = int_out_path,
           int_in_flows = int_in_path,
           domestic_rates = domestic_rates_path))))

#x <- evaluate_fns_list(constraint_data_fns)
config_list <- list(population_backseries_path = population_backseries_path,
                    constraint_data_fns = constraint_data_fns,
                    component_rates_fns = component_rates_fns,
                    communal_est_path = communal_est_path,
                    dev_trajectory_path = dev_trajectory_path,
                    ahs_trajectory_path = ahs_trajectory_path,
                    dwelling_ratio_path = dwelling_ratio_path,
                    hma_list = hma_list,
                    first_proj_yr = first_proj_yr)



#control
housing_led_control <- function(config_list){}

expected_config <- c(population_backseries_path,
                     constraint_data_fns,
                     component_rates_fns,
                     communal_est_path,
                     dev_trajectory_path,
                     ahs_trajectory_path,
                     dwelling_ratio_path,
                     hma_list,
                     first_proj_yr)

#The control will mostly be for reading-in data
#And managing the return from the core loop

#For constraining
constraints <- evaluate_fns_list(config_list$constraint_data_fns)

#component rates
component_rates <- evaluate_fns_list(config_list$component_rates_fns)

population <- readRDS(population_backseries_path)
constraints$population_constraint <- population
final_proj_yr <- max(population$year)

#other data
#use get_component
communal_establishment_population <- readRDS(config_list$communal_est_path)#, max_yr = final_proj_yr)
#average_household_size <- get_component(config_list$ahs_trajectory_path, max_yr = final_proj_yr)
#development_trajectory <- get_component(config_list$dev_trajectory_path, max_yr = final_proj_yr)
#dwelling2household_ratio <- readRDS(config_list$dwelling_ratio_path) %>% as.data.frame()

#For trend model
#TODO Is this  good idea? Means passing 1 less variable and potentially
#avoids a mismatch between methods and data but could it create problems?
config_list$int_out_method <- ifelse(max(component_rates[['int_out_flows_rates']]$int_out)>1 , "flow", "rate") 
npp_constraints = NULL
upc = NULL

#general
#offset_or_ratio_method
#this added by BC on 10/05/2019.  Previously file only contained single set of ratios - these could be valid for only the DCLG or ONS model.
#The ratio file needs updating whenever the population estimates, past housing data, or household formation assumptions change
#Have also added the ability to use the alternative methodlogy (which previously was the standard approach) for reconciling population change and dwelling change since 2011
#Now can choose between the *ratio* method and the *offset* method
#the choice of dclg or ons consistent data should be done automatically now.  The choice between ratio and offset approach is for now hardcoded
#TODO remove hardcoding of ratio/offset method and have this defined in the model run scripts
#a script is now in the input folder to create the input file
#choose method
####HARDCODED###
#use_offset_method <- FALSE
################

curr_yr_popn <- filter(population, year == first_proj_yr-1)

#TODO import trend model package
source('~/Documents/git/population_projections/model_code/model_scripts/trend/02_core.R')
source('~/Documents/git/population_projections/model_code/model_scripts/housing_led/housing_led_core.R')

projection <- list()
for(projection_year in first_proj_yr:final_proj_yr){
  
  curr_yr_fertility <- filter(component_rates$fertility_rates, year == projection_year)
  curr_yr_mortality <- filter(component_rates$mortality_rates, year == projection_year)
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
                                                    hma_list = config_list$hma_list,
                                                    projection_year = projection_year)
  
  curr_yr_popn <- projection[[projection_year]][['population']]
}



