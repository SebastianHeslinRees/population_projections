#' Arrange outputs from the \code{small_area_led_core} function into inputs for the
#' \code{output_small_area_projection} function
#'
#' The \code{small_area_led_core} outputs a list for each year of the projection.
#' Each list is a list of components. This function rearrages those outputs into
#' a list of dataframes of components.
#'
#' @param projection A list. The output list from the \code{small_area_led_core} function
#' @param popn_backseries A dataframe. The small area population backseries
#' @param dwelling_trajectory A dataframe. The small area dwelling trajectory by year
#'   and geographic aggregation (ward or msoa)
#' @param first_proj_yr Numeric. First projection year
#' @param final_proj_yr Numeric. Last projection year
#'
#' @return A list where each element is a data frame containing data for each year
#'   of the projection and the backseries.
#'
#' @import dplyr
#'
#' @export

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
    
  }
  
  proj_popn   <- data.frame(data.table::rbindlist(proj_popn, use.names=TRUE))
  proj_deaths <- data.frame(data.table::rbindlist(proj_deaths, use.names=TRUE))
  proj_births <- data.frame(data.table::rbindlist(proj_births, use.names=TRUE))
  proj_migration <- data.frame(data.table::rbindlist(proj_migration, use.names=TRUE))
  
  return(list(population = proj_popn,
              births = proj_births,
              deaths = proj_deaths,
              migration = proj_migration,
              assumed_development = dwelling_trajectory))
  
}
