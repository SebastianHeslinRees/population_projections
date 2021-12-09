arrange_housing_led_outputs <- function(projection,
                                        population, births, deaths,
                                        in_migration, out_migration,
                                        fertility_rates, mortality_rates,
                                        projected_in_migration,
                                        projected_out_migration,
                                        first_proj_yr, last_proj_yr){
  
  
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_out_migration <- list(out_migration %>% filter(year < first_proj_yr))
  proj_in_migration <- list(in_migration %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  
  join_by <- intersect(names(out_migration), names(in_migration))
  proj_net_migration <- list(left_join(in_migration, out_migration, by = join_by) %>% 
                               mutate(net_migration = inflow - outflow) %>% 
                               filter(year < first_proj_yr) %>% 
                               select(-inflow, -outflow))
  
  proj_popn <- list()
  proj_ahs <- list()
  proj_selection <- list()
  
  for(projection_year in first_proj_yr:last_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_out_migration[[projection_year]] <- projection[[projection_year]][['out_migration']]
    proj_net_migration[[projection_year]] <- projection[[projection_year]][['net_migration']]
    proj_in_migration[[projection_year]] <- projection[[projection_year]][['in_migration']]
    
    proj_ahs[[projection_year]] <- projection[[projection_year]][['ahs']]
    proj_selection[[projection_year]] <- projection[[projection_year]][['selected_ahs']]
    
  }
  
  bind_and_arrange <- function(x){
    
    arrange_by <- intersect(c("gss_code", "gss_code_ward", "year", "sex", "age"),
                            names(x))
    
    data.table::rbindlist(x, use.names = TRUE) %>%
      arrange_at(arrange_by) %>% 
      data.frame()
  }
  
  output_list <- list(population = proj_popn,
                      births = proj_births,
                      deaths = proj_deaths,
                      out_migration = proj_out_migration,
                      in_migration = proj_in_migration,
                      net_migration = proj_net_migration,
                      ahs = proj_ahs,
                      selection = proj_selection) %>% 
    lapply(bind_and_arrange)
  
  return(output_list)
}