#' Arrange outputs from the \code{small_area_led_core} function into inputs for the
#' \code{output_small_area_projection} function
#'
#' The \code{small_area_led_core} outputs a list for each year of the projection.
#' Each list is a list of components. This function rearranges those outputs into
#' a list of dataframes of components.
#'
#' @param projection A list. The output list from the \code{small_area_led_core} function
#' @param popn_backseries A dataframe. The small area population backseries
#' @param dwelling_trajectory A dataframe. The small area dwelling trajectory by year
#'   and geographic aggregation (ward or msoa)
#' @param first_proj_yr Numeric. First projection year
#' @param last_proj_yr Numeric. Last projection year
#' @param small_area_births_sya A dataframe. Births by single year of age and sex 
#'  for each geographic aggregation and year
#' @param small_area_deaths_sya  A dataframe. Deaths by single year of age and sex
#'  for each #'  geographic aggregation and year
#'
#' @return A list where each element is a data frame containing data for each year
#'   of the projection and the backseries.
#'
#' @import dplyr

arrange_small_area_core_outputs <- function(projection, popn_backseries,
                                            dwelling_trajectory,
                                            first_proj_yr, last_proj_yr,
                                            small_area_births_sya,
                                            small_area_deaths_sya){
 
  popn_backseries <- filter(popn_backseries, year %in% 2010:(first_proj_yr-1))
  
  births_backseries <- small_area_births_sya %>%
    filter(year %in% 2002:(first_proj_yr-1))
  
  deaths_backseries <- small_area_deaths_sya %>%
    filter(year %in% 2002:(first_proj_yr-1))
  
  past_net <- popn_backseries %>%
    popn_age_on(col_aggregation = c("year", "gss_code_small_area", "age", "sex"),
                births = births_backseries) %>%
    left_join(unique(select(popn_backseries, gss_code, gss_code_small_area)), by="gss_code_small_area") %>% 
    filter(year %in% 2011:2018) %>%
    left_join(deaths_backseries, by = c("gss_code", "gss_code_small_area", "year", "sex", "age")) %>%
    mutate(nat_chg = popn - deaths) %>%
    select(-popn) %>%
    left_join(popn_backseries, by = c("gss_code", "gss_code_small_area", "year", "sex", "age")) %>%
    mutate(migration = popn - nat_chg) %>%
    select(year, gss_code, gss_code_small_area, sex, age, migration)
  
  proj_popn <- list()
  proj_popn[[1]] <- popn_backseries
  proj_deaths <- list()
  proj_deaths[[1]] <- deaths_backseries
  proj_births <- list()
  proj_births[[1]] <- births_backseries
  proj_migration <- list()
  proj_migration[[1]] <- past_net

  for(projection_year in first_proj_yr:last_proj_yr){
    
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
