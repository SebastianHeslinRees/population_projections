arrange_small_area_core_outputs <- function(projection, popn_backseries, dwelling_trajectory,
                                            first_proj_yr, final_proj_yr){
 
  popn_backseries <- filter(popn_backseries, year %in% 2010:(first_proj_yr-1))
  
  proj_popn <- list()
  proj_popn[[1]] <- popn_backseries
  proj_deaths <- list()
  proj_births <- list()
  proj_migration <- list()
  assumed_dev <- list()
  
  for(projection_year in first_proj_yr:final_proj_yr){
    
    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_migration[[projection_year]] <- projection[[projection_year]][['migration']]
    #assumed_dev[[projection_year]] <- projection[[projection_year]][['assumed_development']]
  }
  
  proj_popn   <- data.frame(data.table::rbindlist(proj_popn, use.names=TRUE))
  proj_deaths <- data.frame(data.table::rbindlist(proj_deaths, use.names=TRUE))
  proj_births <- data.frame(data.table::rbindlist(proj_births, use.names=TRUE))
  proj_migration <- data.frame(data.table::rbindlist(proj_migration, use.names=TRUE))
  #assumed_dev <- data.frame(data.table::rbindlist(assumed_dev, use.names = TRUE))
  
  return(list(population = proj_popn,
              births = proj_births,
              deaths = proj_deaths,
              migration = proj_migration,
              assumed_development = dwelling_trajectory))
  
}
