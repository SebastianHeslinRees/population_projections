arrange_housing_led_core_outputs <- function(projection, trend_projection, first_proj_yr, last_proj_yr, upc){

  proj_popn <- list()
  proj_int_out <- list()
  proj_int_in <- list()
  proj_deaths <- list()
  proj_births <- list()
  proj_dom_out <- list()
  proj_dom_in <- list()
  proj_ahs <- list()
  proj_ahs_choice <- list()
  proj_household_popn <- list()
  proj_trend_popn <- list()
  proj_adj_mig <- list()
  proj_unconstrained <- list()
  
  for(projection_year in first_proj_yr:last_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_int_out[[projection_year]] <- projection[[projection_year]][['int_out']]
    proj_int_in[[projection_year]] <- projection[[projection_year]][['int_in']]
    proj_dom_out [[projection_year]] <- projection[[projection_year]][['dom_out']]
    proj_dom_in[[projection_year]] <- projection[[projection_year]][['dom_in']]
    proj_ahs[[projection_year]] <- projection[[projection_year]][['ahs']]
    proj_ahs_choice[[projection_year]] <- projection[[projection_year]][['ahs_choice']]
    proj_household_popn[[projection_year]] <- projection[[projection_year]][['household_population']]
    
    proj_trend_popn[[projection_year]] <- trend_projection[[projection_year]][['population']]
    proj_adj_mig[[projection_year]] <- projection[[projection_year]][['adjusted_domestic_migration']]
    proj_unconstrained[[projection_year]] <- projection[[projection_year]][['unconstrained_population']]
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
  proj_household_popn <- data.frame(data.table::rbindlist(proj_household_popn, use.names=TRUE))
  proj_trend_popn <- data.frame(data.table::rbindlist(proj_trend_popn, use.names=TRUE))
  proj_adj_mig <- data.frame(data.table::rbindlist(proj_adj_mig, use.names = TRUE))
  proj_unconstrained <- data.frame(data.table::rbindlist(proj_unconstrained, use.names = TRUE))
  
  #complete upc
  if(!is.null(upc)){
    upc <- upc %>% 
      tidyr::complete(year = first_proj_yr:last_proj_yr,
                      gss_code = unique(proj_popn$gss_code),
                      sex = c("male", "female"),
                      age = 0:90,
                      fill = list(upc = 0)) %>% 
      data.frame()
  }
  
  projection <- list(population = proj_popn,
                     births = proj_births,
                     deaths = proj_deaths,
                     int_out = proj_int_out,
                     int_in = proj_int_in,
                     dom_out = proj_dom_out,
                     dom_in = proj_dom_in,
                     ahs = proj_ahs,
                     ahs_choice = proj_ahs_choice,
                     household_population = proj_household_popn,
                     trend_population = proj_trend_popn,
                     adjusted_domestic_migration = proj_adj_mig,
                     unconstrained_population = proj_unconstrained,
                     upc = upc)

  return(projection)
}
