#' Arrange outputs from the \code{trend_core} function into inputs for the
#' \code{trend_model_outputs} function
#'
#' The \code{trend_core} outputs a list for each year of the projection.
#' Each list is a list of components. This function rearrages those outputs into
#' a list of dataframes of components.
#'
#' @param projection A list. The output list from hte \code{trend_core} function
#' @param population Dataframe. Backseries population data
#' @param births Dataframe. Backseries births data
#' @param deaths Dataframe. Backseries deaths data
#' @param int_out Dataframe. Backseries international out migration data
#' @param int_in Dataframe. Backseries international in migration data
#' @param dom_in Dataframe. Backseries domestic in migration data
#' @param dom_out Dataframe. Backseries domestic out migration data
#' @param fertility_rates Dataframe. Model fertility rates
#' @param mortality_rates Dataframe. Model mortality rates
#' @param int_out_rates Dataframe. Model international out migration rates
#' @param int_in_flows Dataframe. Model internation in migration flows
#' @param domestic_rates Dataframe. Model mortality rates
#' @param first_proj_yr Numeric. First projection year
#' @param last_proj_yr Numeric. Last projection year
#'
#' @return A list where each element is a data frame containing data for each year
#'   of the projection and the backseries.
#'
#' @importFrom data.table rbindlist
#'
#' @export

arrange_trend_core_outputs <- function(projection,
                             population, births, deaths, int_out, int_in, dom_in, dom_out,
                             fertility_rates, mortality_rates,
                             int_out_rates, int_in_flows, domestic_rates,
                             first_proj_yr, last_proj_yr){

  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_int_out <- list(int_out %>% filter(year < first_proj_yr))
  proj_int_in <- list(int_in %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_dom_out <- list(dom_out %>% filter(year < first_proj_yr))
  proj_dom_in <- list(dom_in %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()

  for(projection_year in first_proj_yr:last_proj_yr){

    proj_popn[[projection_year]] <- projection[[projection_year]][['population']]
    proj_births[[projection_year]] <- projection[[projection_year]][['births']]
    proj_deaths[[projection_year]] <- projection[[projection_year]][['deaths']]
    proj_int_out[[projection_year]] <- projection[[projection_year]][['int_out']]
    proj_int_in[[projection_year]] <- projection[[projection_year]][['int_in']]
    proj_dom_out [[projection_year]] <- projection[[projection_year]][['dom_out']]
    proj_dom_in[[projection_year]] <- projection[[projection_year]][['dom_in']]
    proj_natural_change[[projection_year]] <- projection[[projection_year]][['natural_change']]
    proj_births_by_mother[[projection_year]] <- projection[[projection_year]][['births_by_mothers_age']]

  }

  proj_popn   <- data.frame(rbindlist(proj_popn, use.names=TRUE))
  proj_deaths <- data.frame(rbindlist(proj_deaths, use.names=TRUE))
  proj_births <- data.frame(rbindlist(proj_births, use.names=TRUE))
  proj_int_out <- data.frame(rbindlist(proj_int_out, use.names=TRUE))
  proj_int_in <- data.frame(rbindlist(proj_int_in, use.names=TRUE))
  proj_dom_out <- data.frame(rbindlist(proj_dom_out, use.names=TRUE))
  proj_dom_in <- data.frame(rbindlist(proj_dom_in, use.names=TRUE))
  proj_natural_change <- data.frame(rbindlist(proj_natural_change, use.names=TRUE))
  proj_births_by_mother <- data.frame(rbindlist(proj_births_by_mother, use.names=TRUE))


  #For int_out and domestic the rate is constant so there is no need to output all years
  int_out_rates <- filter(int_out_rates, year <= first_proj_yr)

  return(list(population = proj_popn,
              deaths = proj_deaths,
              births = proj_births,
              int_out = proj_int_out,
              int_in = proj_int_in,
              dom_out = proj_dom_out,
              dom_in = proj_dom_in,
              births_by_mothers_age = proj_births_by_mother,
              natural_change = proj_natural_change,
              fertility_rates = fertility_rates,
              mortality_rates = mortality_rates,
              int_out_rates = int_out_rates,
              domestic_rates = domestic_rates))

}
