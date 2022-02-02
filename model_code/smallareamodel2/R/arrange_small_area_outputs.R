#' Arrange ward-level outputs
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
#' @param first_proj_yr Numeric. The first projection year
#' @param last_proj_yr Numeric. The last projection
#' @param lookup_path String. The file path of a lookup containing small area codes
#'  and names and LA codes and names
#' @param model String. Either 'trend' or 'housing-led'
#' 
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom dtplyr lazy_dt
#' 
#' @return A list of projection outputs by component
#' 

arrange_small_area_outputs <- function(projection,
                                       population, births, deaths,
                                       in_migration, out_migration,
                                       fertility_rates, mortality_rates,
                                       in_mig_flows, out_mig_rates,
                                       first_proj_yr, last_proj_yr,
                                       lookup_path, model){
  
  lookup <- readRDS(lookup_path)
  
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_popn_unc <- list(population %>% filter(year < first_proj_yr))
  proj_popn_con <- list(population %>% filter(year < first_proj_yr))
  proj_out_migration <- list(out_migration %>% filter(year < first_proj_yr))
  proj_in_migration <- list(in_migration %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()
  proj_components <- list()
  proj_ahs <- list()
  proj_households <- list()
  proj_hh_pop_sya <- list()
  proj_components <- list()
  
  join_by <- intersect(names(out_migration), names(in_migration))
  
  proj_net_migration <- list(left_join(in_migration, out_migration, by = join_by) %>% 
                               mutate(net_migration = inflow - outflow) %>% 
                               filter(year < first_proj_yr) %>% 
                               select(year, gss_code, gss_code_ward, sex, age, net_migration))
  
  
  for(projection_year in first_proj_yr:last_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_out_migration[[projection_year]] <- projection[[projection_year]][['out_migration']]
    proj_net_migration[[projection_year]] <- projection[[projection_year]][['net_migration']]
    proj_in_migration[[projection_year]] <- projection[[projection_year]][['in_migration']]
    
    #possible problem
    proj_components[[projection_year]] <- projection[[projection_year]][['detailed_components']]
    
  }
  
  if(model == "trend"){
    
    #trend
    for(projection_year in first_proj_yr:last_proj_yr){
      proj_births_by_mother[[projection_year]] <- projection[[projection_year]][['births_by_mothers_age']]
      proj_natural_change[[projection_year]] <- projection[[projection_year]][['natural_change']]
    }
    
  } else {
    
    #housing-led
    for(projection_year in first_proj_yr:last_proj_yr){
      proj_popn_unc[[projection_year]] <- projection[[projection_year]][['unconstrained_population']]
      proj_popn_con[[projection_year]] <- projection[[projection_year]][['constarined_population']]
      proj_ahs[[projection_year]] <- projection[[projection_year]][['ahs']]
      proj_households[[projection_year]] <- projection[[projection_year]][['households_detail']]
      proj_hh_pop_sya[[projection_year]] <- projection[[projection_year]][['household_population_sya']]
    }
  }
  
  bind_and_arrange <- function(x){
    
    z <- rbindlist(x, use.names = TRUE) %>% 
      left_join(lookup, by = c("gss_code", "gss_code_ward")) %>% 
      data.frame()
    
    arrange_by <- intersect(c("gss_code", "la_name", "gss_code_ward", "ward_name", "year", "sex", "age"),
                            names(z))
    
    select_by <- c(arrange_by, setdiff(names(z), arrange_by))
    
    z %>% 
      lazy_dt() %>% 
      arrange_at(arrange_by) %>% 
      select_at(select_by) %>% 
      data.frame()
  }
  
  output_list <- list(population = proj_popn,
                      births = proj_births,
                      deaths = proj_deaths,
                      out_migration = proj_out_migration,
                      in_migration = proj_in_migration,
                      net_migration = proj_net_migration,
                      detailed_components = proj_components) %>% 
    lapply(bind_and_arrange)
  
  #Summary df
  output_list$summary <- output_list$detailed_components %>%
    select(-age, -sex) %>% 
    group_by(gss_code, la_name, gss_code_ward, ward_name, year) %>% 
    summarise(across(.fns = sum), .groups = 'drop_last') %>% 
    data.frame() %>%                  
    mutate_if(is.numeric, round, digits = 3)
  #TODO reorder so popn is first and add a total change
  
  #round components tables
  output_list$detailed_components <- output_list$detailed_components %>%                  
    mutate_if(is.numeric, round, digits = 3)

  
  
  if(model == "trend"){
    
    #trend
    trend_list <- list(natural_change = proj_natural_change,
                       births_by_mothers_age = proj_births_by_mother)%>% 
      lapply(bind_and_arrange)
    
    output_list <- c(output_list, trend_list)
    
  } else {
    
    #housing-led
    housing_list <- list(unconstrained_population = proj_popn_unc,
                         constrained_population = proj_popn_con,
                         household_population_sya = proj_hh_pop_sya,
                         ahs = proj_ahs,
                         households_detail = proj_households) %>% 
      lapply(bind_and_arrange)
    
    output_list <- c(output_list, housing_list)
    
  }
  
  return(output_list)
  
}