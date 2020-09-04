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
#' @param upc Dataframe or NULL. UPC component data.
#' @param fertility_rates Dataframe. Model fertility rates
#' @param mortality_rates Dataframe. Model mortality rates
#' @param int_out_rates Dataframe. Model international out migration rates
#' @param int_in_flows Dataframe. Model internation in migration flows
#' @param first_proj_yr Numeric. First projection year
#' @param last_proj_yr Numeric. Last projection year
#'
#' @return A list where each element is a data frame containing data for each year
#'   of the projection and the backseries.
#'
#' @importFrom data.table rbindlist
#' @import dplyr
#' @import popmodules

arrange_trend_core_outputs <- function(projection,
                                       population, births, deaths, int_out,
                                       int_in, dom_in, dom_out, upc,
                                       fertility_rates, mortality_rates,
                                       int_out_rates, int_in_flows,
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

  #complete upc
  if(!is.null(upc)){
    upc <- upc %>% 
      tidyr::complete(year = min(population$year):last_proj_yr,
                    gss_code = unique(population$gss_code),
                    sex = c("male", "female"),
                    age = 0:90,
                    fill = list(upc = 0)) %>% 
      data.frame() %>%
      filter(gss_code %in% unique(population$gss_code),
             year %in% unique(population$year)) %>% 
      aggregate_regions(england=TRUE)
  }

  regional_data <- list(proj_popn = proj_popn,
                        proj_deaths = proj_deaths,
                        proj_births = proj_births,
                        proj_int_out = proj_int_out,
                        proj_int_in = proj_int_in,
                        proj_natural_change = proj_natural_change,
                        proj_births_by_mother = proj_births_by_mother)
  
  regional_data <- lapply(regional_data, function(x){
    data.table::rbindlist(x, use.names = TRUE) %>%
      data.frame() %>%
      aggregate_regions(england=TRUE)})
  
  proj_dom_out <- data.frame(data.table::rbindlist(proj_dom_out, use.names=TRUE))
  proj_dom_in <- data.frame(data.table::rbindlist(proj_dom_in, use.names=TRUE))
  
  
  #For int_out and domestic the rate is constant so there is no need to output all years
  int_out_rates <- filter(int_out_rates, year <= first_proj_yr)
  
  return(list(population = regional_data$proj_popn,
              deaths = regional_data$proj_deaths,
              births = regional_data$proj_births,
              int_out = regional_data$proj_int_out,
              int_in = regional_data$proj_int_in,
              dom_out = proj_dom_out,
              dom_in = proj_dom_in,
              births_by_mothers_age = regional_data$proj_births_by_mother,
              natural_change = regional_data$proj_natural_change,
              fertility_rates = fertility_rates,
              mortality_rates = mortality_rates,
              int_out_rates = int_out_rates,
              upc = upc))
  
}