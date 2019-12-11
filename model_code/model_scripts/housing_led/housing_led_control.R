run_housing_led_model <- function(config_list){}

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


#validate
expected_config <- c()

#component rates
component_rates <- evaluate_fns_list(config_list$component_rates_fns)

#For constraining
constraints <- evaluate_fns_list(config_list$constraint_data_fns) %>%
  create_constraints()

#housing market area constraint
hma_list <- hma_list %>% 
  tibble::enframe("hma","gss_code") %>% 
  tidyr::unnest(cols=c("hma","gss_code")) %>%
  as.data.frame()

hma_constraint <- readRDS(paste0(config_list$trend_path, "population_", config_list$trend_datestamp,".rds")) %>%
  dtplyr::lazy_dt() %>%
  filter(gss_code %in% hma_list$gss_code) %>%
  group_by_at(c("year","hma","sex","age")) %>%
  summarise(popn := sum(popn)) %>%
  as.data.frame()

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
int_out_method <- ifelse(max(component_rates[['int_out_flows_rates']]$int_out)>1 , "flow", "rate") 
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

curr_yr_popn <- readRDS(paste0(trend_path, "population_", trend_datestamp,".rds")) %>%
  filter(population, year == first_proj_yr-1)

final_proj_yr <- max(hma_constraint$year)

#TODO import trend model package
source('model_code/model_scripts/trend/02_core.R')
source('model_code/model_scripts/housing_led/housing_led_core.R')

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

projection <- list()

for(projection_year in first_proj_yr:final_proj_yr){
  
  curr_yr_fertility <- filter(component_rates$fertility_rates, year == projection_year)
  curr_yr_mortality <- filter(component_rates$mortality_rates, year == projection_year)
  curr_yr_int_out <- mutate(component_rates$int_out, year = projection_year)
  curr_yr_int_in_flows <- component_rates$int_in %>% filter(year == projection_year)
  curr_yr_ahs <- filter(average_household_size, year == projection_year)
  curr_yr_households <- filter(household_trajectory, year == projection_year)
  curr_yr_hma_constraint <- filter(hma_constraint, year == projection_year)
  
  projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                    fertility_rates = curr_yr_fertility, 
                                                    mortality_rates = curr_yr_mortality,
                                                    int_out_flows_rates = curr_yr_int_out,
                                                    int_in_flows = curr_yr_int_in_flows,
                                                    domestic_rates = component_rates$domestic_rates,
                                                    int_out_method = int_out_method,
                                                    npp_constraints = NULL, upc = NULL,
                                                    component_constraints = component_constraints,
                                                    hma_constraint = curr_yr_hma_constraint,
                                                    communal_establishment_population = communal_establishment_population,
                                                    average_household_size = curr_yr_ahs,
                                                    households = curr_yr_households,
                                                    hma_list = hma_list,
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
