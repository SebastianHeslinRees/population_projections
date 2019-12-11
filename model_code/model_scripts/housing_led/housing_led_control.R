devtools::load_all('model_code/popmodules')

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
trend_path <- "outputs/trend/2018/2018_central/"
trend_datestamp <- "19-11-13_2056"
communal_est_file <- "ons_communal_est_population.rds"

dev_trajectory_path <- "input_data/housing_led_model/borough_shlaa_trajectory.rds"
ahs_trajectory_path <- "input_data/housing_led_model/dclg_ahs.rds"
dwelling_ratio_path <- "input_data/housing_led_model/dwellings_to_households_census.rds"

hma_list <- list(london = c(paste0("E0900000",1:9), paste0("E090000",10:33)))
first_proj_yr <- 2019


constraint_data_fns <- list(
  list(fn = popmodules::get_data_from_file,
       args = list(
         list(
           birth_constraint = paste0(trend_path,"births_",trend_datestamp,".rds"),
           death_constraint = paste0(trend_path,"deaths_",trend_datestamp,".rds"),
           international_out_constraint = paste0(trend_path,"int_out_",trend_datestamp,".rds")))),
  list(fn = create_constraints,
       args = list(col_aggregation = c("year","gss_code"))))

component_rates_fns <- list(
  list(fn = popmodules::get_data_from_file,
       args= list(list(
           fertility_rates = paste0(trend_path,"fertility_rates_",trend_datestamp,".rds"),
           mortality_rates = paste0(trend_path,"mortality_rates_",trend_datestamp,".rds"),
           int_out_flows_rates = paste0(trend_path,"int_out_rates_",trend_datestamp,".rds"),
           int_in_flows = paste0(trend_path,"int_in_",trend_datestamp,".rds"),
           domestic_rates = paste0(trend_path,"domestic_rates_",trend_datestamp,".rds")))))

config_list <- list()
config_list$constraint_data_fns <- constraint_data_fns
config_list$component_rates_fns <- component_rates_fns
config_list$communal_est_path <- paste0(trend_path,"households_",trend_datestamp,"/",communal_est_file)

#control
housing_led_control <- function(config_list){}

expected_config <- c()

#The control will mostly be for reading-in data
#And managing the return from the core loop

#For constraining
constraints <- evaluate_fns_list(config_list$constraint_data_fns)

#component rates
component_rates <- evaluate_fns_list(config_list$component_rates_fns)

population <- readRDS(paste0(trend_path, "population_", trend_datestamp,".rds"))
constraints$population_constraint <- population
final_proj_yr <- max(population$year)

#other data
#use get_component
communal_establishment_population <- readRDS(config_list$communal_est_path) %>%
  dtplyr::lazy_dt() %>%
  group_by(gss_code, year) %>%
  summarise(communal_est_popn = sum(communal_establishment_population)) %>%
  as.data.frame()

average_household_size <- readRDS(config_list$ahs_trajectory_path) %>% as.data.frame()
development_trajectory <- readRDS(config_list$dev_trajectory_path) %>% project_forward_flat(2050)
dwelling2household_ratio <- readRDS(config_list$dwelling_ratio_path) %>% as.data.frame()

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
source('model_code/model_scripts/trend/02_core.R')
source('model_code/model_scripts/housing_led/housing_led_core.R')

projection <- list()

#for testing only
projection_year <- first_proj_yr

#development_trajectory = development_trajectory,
household_trajectory <- left_join(development_trajectory, dwelling2household_ratio, by="gss_code") %>%
  group_by(gss_code) %>%
  mutate(cumulative_dwellings = cumsum(new_homes)) %>%
  ungroup() %>%
  mutate(projected_stock = cumulative_dwellings+census_dwellings) %>%
  mutate(households = projected_stock / ratio) %>%
  select(gss_code, year, households)

ahs_cap <- NULL
config_list$ahs_cap_year <- 2020

first_proj_yr <- 2019
final_proj_yr <- 2021

for(projection_year in first_proj_yr:final_proj_yr){
  
  curr_yr_fertility <- filter(component_rates$fertility_rates, year == projection_year)
  curr_yr_mortality <- filter(component_rates$mortality_rates, year == projection_year)
  curr_yr_int_out <- mutate(component_rates$int_out, year = projection_year)
  curr_yr_int_in_flows <- component_rates$int_in %>% filter(year == projection_year)
  curr_yr_ahs <- filter(average_household_size, year == projection_year)
  curr_yr_households <- filter(household_trajectory, year == projection_year)
  
  projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                    fertility_rates = curr_yr_fertility, 
                                                    mortality_rates = curr_yr_mortality,
                                                    int_out_flows_rates = curr_yr_int_out,
                                                    int_in_flows = curr_yr_int_in_flows,
                                                    domestic_rates = component_rates$domestic_rates,
                                                    int_out_method = config_list$int_out_method,
                                                    npp_constraints = NULL, upc = NULL,
                                                    constraints = constraints,
                                                    communal_establishment_population = communal_establishment_population,
                                                    average_household_size = curr_yr_ahs,
                                                    households = curr_yr_households,
                                                    hma_list = config_list$hma_list,
                                                    projection_year = projection_year,
                                                    ahs_cap_year = config_list$ahs_cap_year,
                                                    ahs_cap = ahs_cap)
  
  ahs_cap <- projection[[projection_year]][['ahs_cap']]
  curr_yr_popn <- projection[[projection_year]][['population']]
}

projection <- arrange_housing_led_core_outputs(projection, first_proj_yr, final_proj_yr)
output_dir <- "outputs/housing_led/2018/test/"

dir.create(output_dir, recursive = T)

lapply(seq_along(projection),
       function(i) saveRDS(projection[[i]],
                           paste0(output_dir, names(projection)[i], ".rds"))) %>%
  invisible()
