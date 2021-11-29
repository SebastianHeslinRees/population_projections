arrange_core_outputs <- function(projection,
                                 population, births, deaths,
                                 in_migration, out_migration,
                                 fertility_rates, mortality_rates,
                                 in_mig_flows, out_mig_rates,
                                 first_proj_yr, last_proj_yr){
  
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_out_migration <- list(out_migration %>% filter(year < first_proj_yr))
  proj_in_migration <- list(in_migration %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()
  proj_components <- list()
  
  join_by <- intersect(names(out_migration), names(in_migration))
  proj_net_migration <- list(left_join(in_migration, out_migration, by = join_by) %>% 
                               mutate(net_migration = inflow - outflow) %>% 
                               filter(year < first_proj_yr))
  
  for(projection_year in first_proj_yr:last_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_out_migration[[projection_year]] <- projection[[projection_year]][['out_migration']]
    proj_net_migration[[projection_year]] <- projection[[projection_year]][['net_migration']]
    proj_in_migration[[projection_year]] <- projection[[projection_year]][['in_migration']]
    proj_natural_change[[projection_year]] <- projection[[projection_year]][['natural_change']]
    proj_births_by_mother[[projection_year]] <- projection[[projection_year]][['births_by_mothers_age']]
    proj_components[[projection_year]] <- projection[[projection_year]][['detailed_components']]
    
  }
  
  bind_and_arrange <- function(x){
    data.table::rbindlist(x, use.names = TRUE) %>%
      arrange(gss_code, gss_code_ward, year, sex, age) %>% 
      data.frame()
  }
  
  output_list <- list(population = proj_popn,
                      births = proj_births,
                      deaths = proj_deaths,
                      out_migration = proj_out_migration,
                      in_migration = proj_in_migration,
                      net_migration = proj_net_migration,
                      natural_change = proj_natural_change,
                      births_by_mothers_age = proj_births_by_mother,
                      detailed_components = proj_components) %>% 
    lapply(bind_and_arrange)
  
  output_list$summary <- output_list$detailed_components %>% 
    select(-age, -sex) %>% 
    group_by(year, gss_code, gss_code_ward) %>% 
    summarise(across(.fns = sum), .groups = 'drop_last') %>% 
    data.frame()
  
  output_list$detailed_components[6:14] <- round(output_list$detailed_components[6:14], 3)
  output_list$summary[4:12] <- round(output_list$summary[4:12], 3)
  
  return(output_list)
  
}