run_housing_led_model <- function(config_list){
  
  create_constraints <- function(dfs, col_aggregation=c("year","gss_code")){
    
    for(i in seq(dfs)){
      
      nm <- last(names(dfs[[i]])) #names(dfs[[i]])[ncol(dfs[[i]])]
      dfs[[i]] <- dfs[[i]] %>%
        dtplyr::lazy_dt() %>%
        group_by_at(col_aggregation) %>%
        summarise(value = sum(!!sym(nm))) %>%
        rename(!!nm := value) %>%
        as.data.frame()
    }
    
    return(dfs)
  }
  
  
  external_trend_households_path <- paste0(config_list$external_trend_path,"households_",config_list$external_trend_datestamp,"/",config_list$trend_households_file)
  external_communal_est_path <- paste0(config_list$external_trend_path,"households_",config_list$external_trend_datestamp,"/",config_list$communal_est_file)
  
  #validate
  expected_config <- c("projection_name",
                       "communal_est_file",
                       "hma_list",
                       "dev_trajectory_path",
                       "external_ahs_trajectory_path",
                       "trend_households_file",
                       "ldd_backseries_path",
                       "ahs_cap_year",
                       "external_trend_path",
                       "external_trend_datestamp",
                       "first_proj_yr",
                       "final_proj_yr",
                       "ldd_max_yr",
                       "timestamp")
  
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  #component rates
  component_rates <- get_data_from_file(
         list(fertility_rates = paste0(config_list$external_trend_path,"fertility_rates_",config_list$external_trend_datestamp,".rds"),
              mortality_rates = paste0(config_list$external_trend_path,"mortality_rates_",config_list$external_trend_datestamp,".rds"),
              int_out_flows_rates = paste0(config_list$external_trend_path,"int_out_rates_",config_list$external_trend_datestamp,".rds"),
              int_in_flows = paste0(config_list$external_trend_path,"int_in_",config_list$external_trend_datestamp,".rds"),
              domestic_rates = paste0(config_list$external_trend_path,"domestic_rates_",config_list$external_trend_datestamp,".rds")))
  
  #For constraining
  component_constraints <- get_data_from_file(
      list(birth_constraint = paste0(config_list$external_trend_path,"births_",config_list$external_trend_datestamp,".rds"),
           death_constraint = paste0(config_list$external_trend_path,"deaths_",config_list$external_trend_datestamp,".rds"),
           international_out_constraint = paste0(config_list$external_trend_path,"int_out_",config_list$external_trend_datestamp,".rds"))) %>%
    create_constraints()
  
  #housing market area constraint
  hma_list <- config_list$hma_list %>% 
    tibble::enframe("hma","gss_code") %>% 
    tidyr::unnest(cols=c("hma","gss_code")) %>%
    as.data.frame()
  
  hma_constraint <- readRDS(paste0(config_list$external_trend_path, "population_", config_list$external_trend_datestamp,".rds")) %>%
    filter(gss_code %in% hma_list$gss_code) %>%
    dtplyr::lazy_dt() %>%
    left_join(hma_list, by="gss_code") %>%
    group_by_at(c("year","hma","sex","age")) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  #check
  assertthat::assert_that(config_list$final_proj_yr <= max(hma_constraint$year))
  
  #other data
  communal_establishment_population <- readRDS(external_communal_est_path) %>%
    dtplyr::lazy_dt() %>%
    group_by(gss_code, year) %>%
    summarise(communal_est_popn = sum(communal_establishment_population)) %>%
    as.data.frame()
  
  external_ahs <- readRDS(config_list$external_ahs_trajectory_path) %>% as.data.frame()
  development_trajectory <- readRDS(config_list$dev_trajectory_path) %>% project_forward_flat(2050) 
  
  #housing trajectory
  external_trend_households <- readRDS(external_trend_households_path) %>%
    filter(year <= config_list$ldd_max_yr)%>%
    dtplyr::lazy_dt() %>%
    group_by(gss_code, year) %>%
    summarise(households = sum(households)) %>%
    as.data.frame()
  
  #TODO I need to decide what we're pre-processing and what we're doing in the control
  #TODO This is missing census data for the base stock so the numbers make no sense
  
  #census stock in 2011 + LDD development upto 2019
  ldd_backseries <- readRDS(config_list$ldd_backseries_path)%>%
    select(names(development_trajectory))
  
  dwelling_trajectory <- filter(ldd_backseries, year == config_list$ldd_max_yr) %>%
    rbind(filter(development_trajectory, year > config_list$ldd_max_yr)) %>% 
    group_by(gss_code) %>%
    mutate(dwellings = cumsum(units)) %>%
    ungroup() %>%
    select(year, gss_code, dwellings)
  
  dwelling_trajectory <- filter(ldd_backseries, year != config_list$ldd_max_yr) %>%
    rename(dwellings = units) %>%
    rbind(dwelling_trajectory) %>%
    arrange(gss_code, year)
  
  dwelling2household_ratio <- filter(external_trend_households,
                                     year <= config_list$ldd_max_yr,
                                     year %in% dwelling_trajectory$year,
                                     gss_code %in% dwelling_trajectory$gss_code) %>%
    left_join(dwelling_trajectory, by=c("gss_code","year")) %>%
    mutate(dw2hh_ratio = households/dwellings) %>%
    select(year, gss_code, dw2hh_ratio) %>%
    project_forward_flat(config_list$final_proj_yr)
  
  #development_trajectory
  household_trajectory <- dwelling_trajectory %>% 
    left_join(dwelling2household_ratio, by=c("year", "gss_code")) %>%
    mutate(households = dwellings * dw2hh_ratio) %>%
    select(gss_code, year, households)
  
  #For trend model
  #TODO Is this  good idea? Means passing 1 less variable and potentially
  #avoids a mismatch between methods and data but could it create problems?
  int_out_method <- ifelse(max(component_rates[['int_out_flows_rates']]$int_out)>1 , "flow", "rate") 
  
  #TODO Sort this out so it can take dataframes here
  npp_constraints = NULL
  upc = NULL
  
  #TODO import trend model package
  source('model_code/model_scripts/trend/02_core.R')
  source('model_code/model_scripts/housing_led/housing_led_core.R')
  source('model_code/model_scripts/housing_led/arrange_housing_led_core_outputs.R')
  source('model_code/model_scripts/housing_led/output_housing_led_projection.R')
  
  #Starting population
  curr_yr_popn <- readRDS(paste0(external_trend_path, "population_", external_trend_datestamp,".rds")) %>%
    filter(year == first_proj_yr-1)
  
  #Other varibales
  ahs_cap <- NULL
  first_proj_yr <- config_list$first_proj_yr
  final_proj_yr <- config_list$final_proj_yr
  
  projection <- list()
  
  for(projection_year in first_proj_yr:final_proj_yr){
    
    curr_yr_fertility <- filter(component_rates$fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(component_rates$mortality_rates, year == projection_year)
    curr_yr_int_out <- mutate(component_rates$int_out, year = projection_year)
    curr_yr_int_in_flows <- component_rates$int_in %>% filter(year == projection_year)
    curr_yr_ahs <- filter(external_ahs, year == projection_year)
    curr_yr_households <- filter(household_trajectory, year == projection_year)
    curr_yr_hma_constraint <- filter(hma_constraint, year == projection_year)
   
    trend_projection <- trend_core(start_population = curr_yr_popn,
                                   fertility_rates = curr_yr_fertility, 
                                   mortality_rates = curr_yr_mortality,
                                   int_out_flows_rates = curr_yr_int_out,
                                   int_in_flows = curr_yr_int_in_flows,
                                   domestic_rates = component_rates$domestic_rates,
                                   int_out_method = int_out_method,
                                   constraints = npp_constraints,
                                   upc = upc,
                                   projection_year)
    
    projection[[projection_year]] <- housing_led_core(start_population = curr_yr_popn, 
                                                      trend_projection = trend_projection,
                                                      component_constraints = component_constraints,
                                                      hma_constraint = curr_yr_hma_constraint,
                                                      communal_establishment_population = communal_establishment_population,
                                                      external_ahs = curr_yr_ahs,
                                                      household_data = curr_yr_households,
                                                      hma_list = hma_list,
                                                      projection_year = projection_year,
                                                      ahs_cap_year = config_list$ahs_cap_year,
                                                      ahs_cap = ahs_cap,
                                                      ldd_max_yr = config_list$ldd_max_yr)
    
    ahs_cap <- projection[[projection_year]][['ahs_cap']]
    curr_yr_popn <- projection[[projection_year]][['population']]
  }
  
  message(" ")
  
  projection <- arrange_housing_led_core_outputs(projection, first_proj_yr, final_proj_yr)
  
  output_dir <- paste0("outputs/housing_led/2018/",config_list$projection_name,"/")
  output_housing_led_projection(projection, output_dir, config_list$timestamp)
  
}
