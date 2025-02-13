#' Arrange flexible area model outputs
#' 
#' Take the projection output list which is organised by year and rearrange it
#' so that its organised by component. Add on the backseries.
#' 
#' @param projection A list output from housing_led_core
#' @param population A dataframe of the population backseries
#' @param births A dataframe of the births backseries
#' @param deaths A dataframe of the deaths backseries
#' @param in_migration A dataframe of the in migration backseries
#' @param out_migration A dataframe of the out migration backseries
#' @param fertility_rates A dataframe of fertility rates
#' @param mortality_rates A dataframe of mortality rates
#' @param in_mig_flows A dataframe of input in migration flows
#' @param out_mig_rates A dataframe of input out migration rates
#' @param dwelling_trajectory A dataframe
#' @param dwelling_stock A dataframe
#' @param first_proj_yr Numeric. The first projection year
#' @param last_proj_yr Numeric. The last projection
#' @param config_list List. The model config list
#' @param model String. Either 'trend' or 'housing-led'
#' @param n_cores Numeric. Number of cores
#' @param parallel Logical. Should the borough data creation be run in parallel
#' 
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom dtplyr lazy_dt
#' @importFrom tidyr pivot_wider
#' 
#' @return A list of projection outputs by component

arrange_flexmodel_outputs <- function(projection,
                                      population, births, deaths,
                                      in_migration, out_migration,
                                      fertility_rates, mortality_rates,
                                      in_mig_flows, out_mig_rates,
                                      dwelling_trajectory, dwelling_stock,
                                      households, communal_establishment_population,
                                      first_proj_yr, last_proj_yr,
                                      config_list, model,
                                      n_cores = 1, parallel = FALSE){
  
  lookup <- readRDS(config_list$lookup_path) %>% 
    rename(area_code = all_of(config_list$geog_code_col),
           area_name = all_of(config_list$geog_name_col))
  
  if("gss_code" %in% names(projection[[first_proj_yr]][['population']])){
    population <- .add_gss_code(population, lookup)
    births <- .add_gss_code(births, lookup)
    deaths <- .add_gss_code(deaths, lookup)
    out_migration <- .add_gss_code(out_migration, lookup)
    in_migration <- .add_gss_code(in_migration, lookup)
    nested_geog <- c("gss_code", "area_code")
    output_geog_cols <- c("gss_code","la_name","area_code","area_name")
  } else {
    nested_geog <- "area_code"
    output_geog_cols <- c("area_code","area_name")
  }
  
  #Components backseries
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_popn_unc <- list(population %>% filter(year < first_proj_yr))
  proj_popn_con <- list(population %>% filter(year < first_proj_yr))
  proj_out_migration <- list(out_migration %>% filter(year < first_proj_yr))
  proj_in_migration <- list(in_migration %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()
  proj_ahs <- list()
  proj_households <- list()
  proj_hh_pop_sya <- list()
  
  #-----------------------------------------------------------------------------
  
  #Net migration backseries
  
  join_by <- intersect(names(out_migration), names(in_migration))
  
  proj_net_migration <- list(left_join(in_migration, out_migration, by = join_by) %>% 
                               mutate(netflow = inflow - outflow) %>% 
                               filter(year < first_proj_yr) %>% 
                               select(!!join_by, netflow))
  #-----------------------------------------------------------------------------
  #Components dataframe backseries
  
  proj_components <- list(rbind(
    mutate(proj_popn[[1]], component = "popn") %>% rename(value = popn),
    mutate(proj_births[[1]], component = "births") %>% rename(value = births),
    mutate(proj_deaths[[1]], component = "deaths") %>% rename(value = deaths),
    mutate(proj_in_migration[[1]], component = "inflow") %>% rename(value = inflow),
    mutate(proj_out_migration[[1]], component = "outflow") %>% rename(value = outflow),
    mutate(proj_net_migration[[1]], component = "netflow") %>% rename(value = netflow)) %>% 
      pivot_wider(values_from = value, names_from = component) %>% 
      mutate(births = ifelse(is.na(births), 0, births)) %>% 
      select(!!nested_geog, year, sex, age, popn, births, deaths, inflow, outflow, netflow))
  
  #-----------------------------------------------------------------------------
  
  #Elements common to trend and housing-led projections
  
  for(projection_year in first_proj_yr:last_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_out_migration[[projection_year]] <- projection[[projection_year]][['out_migration']]
    proj_net_migration[[projection_year]] <- projection[[projection_year]][['net_migration']]
    proj_in_migration[[projection_year]] <- projection[[projection_year]][['in_migration']]
    proj_components[[projection_year]] <- projection[[projection_year]][['detailed_components']] %>% 
      select(names(proj_components[[1]]))
    
  }
  
  output_list <- list(population = proj_popn,
                      births = proj_births,
                      deaths = proj_deaths,
                      out_migration = proj_out_migration,
                      in_migration = proj_in_migration,
                      net_migration = proj_net_migration,
                      detailed_components = proj_components) %>% 
    lapply(.bind_and_arrange, lookup)
  
  #-----------------------------------------------------------------------------
  #Components summary dataframe
  
  output_list$summary <- output_list$detailed_components %>%
    select(-age, -sex) %>% 
    group_by(across(!!output_geog_cols), year) %>% 
    summarise(across(.fns = sum), .groups = 'drop_last') %>%                  
    mutate(across(where(is.numeric) & !year, ~round(.x, digits=3))) %>% 
    mutate(change = births - deaths + netflow) %>% 
    select(!!output_geog_cols, year,
           population = popn, births, deaths, inflow, outflow, netflow, change) %>% 
    arrange(across(!!nested_geog), year) %>% 
    data.frame()
  
  #-----------------------------------------------------------------------------
  
  #Model-specific outputs
  
  if(model == "trend"){
    
    #trend
    for(projection_year in first_proj_yr:last_proj_yr){
      proj_births_by_mother[[projection_year]] <- projection[[projection_year]][['births_by_mothers_age']]
      proj_natural_change[[projection_year]] <- projection[[projection_year]][['natural_change']]
    }
    
    trend_list <- list(natural_change = proj_natural_change,
                       births_by_mothers_age = proj_births_by_mother) %>% 
      lapply(.bind_and_arrange, lookup)
    
    output_list <- c(output_list, trend_list)
    
  } else {
    
    #housing-led
    for(projection_year in first_proj_yr:last_proj_yr){
      proj_popn_unc[[projection_year]] <- projection[[projection_year]][['unconstrained_population']]
      proj_popn_con[[projection_year]] <- projection[[projection_year]][['constrained_population']]
      proj_ahs[[projection_year]] <- projection[[projection_year]][['ahs']]
      proj_households[[projection_year]] <- projection[[projection_year]][['households_detail']]
      proj_hh_pop_sya[[projection_year]] <- projection[[projection_year]][['household_population_sya']]
    }
   
    #housing-led backseries
    for(backseries_year in min(population$year):(first_proj_yr-1)){
      
      proj_hh_pop_sya[[backseries_year]] <- filter(population, year == backseries_year) %>% 
        .remove_ce_popn(communal_establishment_population) %>% 
        select(names(proj_hh_pop_sya[[first_proj_yr]]))
      
      #browser()
      
      proj_households[[backseries_year]] <- proj_hh_pop_sya[[backseries_year]] %>% 
        group_by(year, across(!!nested_geog)) %>% 
        summarise(population = sum(popn),
                  ce_population = sum(ce_popn),
                  household_popn = sum(household_popn),
                  .groups = 'drop_last') %>% 
        data.frame() %>% 
        left_join(households, by = c("year", "area_code")) %>% 
        rename(input_households = households)
        
      proj_ahs[[backseries_year]] <- proj_households[[backseries_year]] %>% 
        mutate(hhr_ahs = NA, trend_ahs = NA, blended_ahs = NA,
               actual_ahs = household_popn / input_households) %>% 
        select(names(proj_ahs[[first_proj_yr]])) %>% 
        data.frame()
    }
    
    #housing-led
    housing_list <- list(unconstrained_population = proj_popn_unc,
                         constrained_population = proj_popn_con,
                         household_population_sya = proj_hh_pop_sya,
                         ahs_detail = proj_ahs,
                         households_detail = proj_households) %>% 
      lapply(.bind_and_arrange, lookup)
    
    housing_list$ahs <- housing_list$ahs_detail %>% 
      select(year, area_code, actual_ahs) %>% 
      pivot_wider(names_from = year, values_from = actual_ahs)
    
    housing_list$dwelling_trajectory <- dwelling_trajectory %>% 
      filter(year %in% 2012:last_proj_yr) %>% 
      left_join(lookup, by = c("area_code")) %>% 
      data.frame() %>% 
      arrange(across(!!nested_geog), year) %>% 
      select(!!output_geog_cols, year, dwellings = units)
    
    housing_list$dwelling_stock <- dwelling_stock %>% 
      filter(year %in% 2011:last_proj_yr) %>% 
      left_join(lookup, by = c("area_code")) %>% 
      data.frame() %>% 
      arrange(across(!!nested_geog), year) %>% 
      select(!!output_geog_cols, year, dwellings = units)
    
    output_list <- c(output_list, housing_list)
    
  }
  
  output_list <- output_list %>% lapply(.rename_geog_cols, config_list$geog_code_col, config_list$geog_name_col)
  
  if(config_list$borough_outputs){
    borough_data <- aggregate_borough_data2(output_list,
                                            config_list$constraint_list$constraint_path,
                                            first_proj_yr,
                                            n_cores,
                                            parallel)
    
    return(c(output_list, borough_data = borough_data))
    
  } else {
    
    return(output_list)
    
  }
  
}

