#' Arrange outputs from the \code{housing_led_core} function into inputs for the
#' \code{output_housing_led_projections} function
#'
#' The \code{housing_led_core} outputs a list for each year of the projection.
#' Each list is a list of components. This function rearranges those outputs into
#' a list of dataframes of components. The \code{trend_projection} parameter is the
#' output of the first stage of the housing-led process, not a full trend model
#' projection.
#'
#' @param projection A list. The output list from the \code{housing_led_core} function
#' @param trend_projection A list. The output list from the \code{trend_core} function
#' @param first_proj_yr Numeric. First projection year
#' @param last_proj_yr Numeric. Last projection year
#' @param upc Dataframe or NULL. UPC component
#'
#' @return A list where each element is a data frame containing data for each year
#'   of the projection and the backseries.
#'
#' @importFrom data.table rbindlist
#'

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
  
  proj_popn   <- data.frame(rbindlist(proj_popn, use.names=TRUE))
  proj_deaths <- data.frame(rbindlist(proj_deaths, use.names=TRUE))
  proj_births <- data.frame(rbindlist(proj_births, use.names=TRUE)) %>% filter(age == 0)
  proj_int_out <- data.frame(rbindlist(proj_int_out, use.names=TRUE))
  proj_int_in <- data.frame(rbindlist(proj_int_in, use.names=TRUE))
  proj_dom_out <- data.frame(rbindlist(proj_dom_out, use.names=TRUE))
  proj_dom_in <- data.frame(rbindlist(proj_dom_in, use.names=TRUE))
  proj_ahs <- data.frame(rbindlist(proj_ahs, use.names=TRUE))
  proj_ahs_choice <- data.frame(rbindlist(proj_ahs_choice, use.names=TRUE))
  proj_household_popn <- data.frame(rbindlist(proj_household_popn, use.names=TRUE))
  proj_trend_popn <- data.frame(rbindlist(proj_trend_popn, use.names=TRUE))
  proj_adj_mig <- data.frame(rbindlist(proj_adj_mig, use.names = TRUE))
  proj_unconstrained <- data.frame(rbindlist(proj_unconstrained, use.names = TRUE))
  
  #complete upc
  if(!is.null(upc)){
    upc <- upc %>% 
      tidyr::complete(year = first_proj_yr:last_proj_yr,
                      gss_code = unique(proj_popn$gss_code),
                      sex = c("male", "female"),
                      age = 0:90,
                      fill = list(upc = 0)) %>% 
      data.frame()
  } else {
    upc <- proj_popn %>% 
      mutate(upc = 0) %>% 
      select(-popn)
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