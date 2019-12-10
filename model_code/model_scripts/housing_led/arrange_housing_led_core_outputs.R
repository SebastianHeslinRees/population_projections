arrange_housing_led_core_outputs <- function(projection, first_proj_yr, final_proj_yr){
  
  proj_popn <- list()
  proj_int_out <- list()
  proj_int_in <- list()
  proj_deaths <- list()
  proj_births <- list()
  proj_dom_out <- list()
  proj_dom_in <- list()
  proj_ahs <- list()
  proj_ahs_choice <- list()
  
  for(projection_year in first_proj_yr:final_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_int_out[[projection_year]] <- projection[[projection_year]][['int_out']]
    proj_int_in[[projection_year]] <- projection[[projection_year]][['int_in']]
    proj_dom_out [[projection_year]] <- projection[[projection_year]][['dom_out']]
    proj_dom_in[[projection_year]] <- projection[[projection_year]][['dom_in']]
    proj_ahs[[projection_year]] <- projection[[projection_year]][['ahs']]
    proj_ahs_choice[[projection_year]] <- projection[[projection_year]][['ahs_choice']]
    
  }
  
  proj_popn   <- data.frame(data.table::rbindlist(proj_popn, use.names=TRUE))
  proj_deaths <- data.frame(data.table::rbindlist(proj_deaths, use.names=TRUE))
  proj_births <- data.frame(data.table::rbindlist(proj_births, use.names=TRUE))
  proj_int_out <- data.frame(data.table::rbindlist(proj_int_out, use.names=TRUE))
  proj_int_in <- data.frame(data.table::rbindlist(proj_int_in, use.names=TRUE))
  proj_dom_out <- data.frame(data.table::rbindlist(proj_dom_out, use.names=TRUE))
  proj_dom_in <- data.frame(data.table::rbindlist(proj_dom_in, use.names=TRUE))
  proj_ahs <- data.frame(data.table::rbindlist(proj_ahs, use.names=TRUE))
  proj_ahs_choice <- data.frame(data.table::rbindlist(proj_ahs_choice, use.names=TRUE))

  
  return(list(population = proj_popn,
              deaths = proj_deaths,
              births = proj_births,
              int_out = proj_int_out,
              int_in = proj_int_in,
              dom_out = proj_dom_out,
              dom_in = proj_dom_in,
              ahs = proj_ahs,
              ahs_choice = proj_ahs_choice))
  
}
