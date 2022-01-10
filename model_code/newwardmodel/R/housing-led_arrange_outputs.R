#' Arrange ward-level housing-led outputs
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
#' @param in_migration_flows A dataframe of input in migration flows
#' @param out_migration_rates A dataframe of input out migration rates
#' @param first_proj_yr Numeric. The first projection year
#' @param last_proj_yr Numeric. The last projection
#' 
#' @import dplyr
#' @importFrom data.table rbindlist
#' 
#' @return A list of projection outputs by component

arrange_housing_led_outputs <- function(projection,
                                        population, births, deaths,
                                        in_migration, out_migration,
                                        fertility_rates, mortality_rates,
                                        in_migration_flows, out_migration_rates,
                                        first_proj_yr, last_proj_yr){

  #Backseries
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  
  proj_in_migration <- list(in_migration %>% filter(year < first_proj_yr))
  proj_out_migration <- list(out_migration %>% filter(year < first_proj_yr))
  
  join_by <- intersect(names(out_migration), names(in_migration))
  proj_net_migration <- list(left_join(in_migration, out_migration, by = join_by) %>% 
                               mutate(net_migration = inflow - outflow) %>% 
                               filter(year < first_proj_yr) %>% 
                               select(-inflow, -outflow))
  
  #No backseries
  proj_ahs <- list()
  proj_households <- list()
  proj_hh_pop_sya <- list()
  proj_components <- list()
  
  #Projected
  for(projection_year in first_proj_yr:last_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_out_migration[[projection_year]] <- projection[[projection_year]][['out_migration']]
    proj_net_migration[[projection_year]] <- projection[[projection_year]][['net_migration']]
    proj_in_migration[[projection_year]] <- projection[[projection_year]][['in_migration']]
    
    proj_ahs[[projection_year]] <- projection[[projection_year]][['ahs']]
    proj_households[[projection_year]] <- projection[[projection_year]][['households_detail']]
    proj_hh_pop_sya[[projection_year]] <- projection[[projection_year]][['household_population_sya']]
    
    proj_components[[projection_year]] <- projection[[projection_year]][['components']]
    
  }
  
  #Organise
  bind_and_arrange <- function(x){
    
    arrange_by <- intersect(c("gss_code", "gss_code_ward", "year", "sex", "age"),
                            names(x))
    
    rbindlist(x, use.names = TRUE) %>%
      arrange_at(arrange_by) %>% 
      data.frame()
  }
  
  output_list <- list(population = proj_popn,
                      births = proj_births,
                      deaths = proj_deaths,
                      out_migration = proj_out_migration,
                      in_migration = proj_in_migration,
                      net_migration = proj_net_migration,
                      household_population_sya = proj_hh_pop_sya,
                      ahs = proj_ahs,
                      households_detail = proj_households,
                      components = proj_components) %>% 
    lapply(bind_and_arrange)
  
  
  return(output_list)
}