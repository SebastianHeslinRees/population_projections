arrange_housing_led_core_outputs <- function(projection, first_proj_yr, final_proj_yr,
                                             external_trend_path, external_trend_datestamp){
  
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
  
  
  projection <- list(population = proj_popn,
                     births = proj_births,
                     deaths = proj_deaths,
                     int_out = proj_int_out,
                     int_in = proj_int_in,
                     dom_out = proj_dom_out,
                     dom_in = proj_dom_in,
                     ahs = proj_ahs,
                     ahs_choice = proj_ahs_choice)
  
  backseries <- get_data_from_file(
    list(popn = paste0(external_trend_path,"population_",external_trend_datestamp,".rds"),
         births = paste0(external_trend_path,"births_",external_trend_datestamp,".rds"),
         deaths = paste0(external_trend_path,"deaths_",external_trend_datestamp,".rds"),
         int_out = paste0(external_trend_path,"int_out_",external_trend_datestamp,".rds"),
         int_in = paste0(external_trend_path,"int_in_",external_trend_datestamp,".rds"),
         dom_out = paste0(external_trend_path,"dom_out_",external_trend_datestamp,".rds"),
         dom_in = paste0(external_trend_path,"dom_in_",external_trend_datestamp,".rds")
    ))
  
  for(i in 1:7){
    
    x <- filter(backseries[[i]], year %in% 2011:(first_proj_yr-1))
  
    projection[[i]] <- data.table::rbindlist(list(x,projection[[i]]),
      use.names = TRUE) %>%
      as.data.frame()
  }
  
  return(projection)
  
}
