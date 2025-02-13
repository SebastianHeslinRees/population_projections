#' Arrange outputs from the \code{trend_core} function into inputs for the
#' \code{trend_model_outputs} function
#'
#' The \code{trend_core} outputs a list for each year of the projection.
#' Each list is a list of components. This function rearranges those outputs
#' into a list of dataframes of components.
#'
#' @param projection A list. The output list from the \code{trend_core} function
#' @param population Dataframe. Backseries population data
#' @param births Dataframe. Backseries births data
#' @param deaths Dataframe. Backseries deaths data
#' @param int_out Dataframe. Backseries international out migration data
#' @param int_in Dataframe. Backseries international in migration data
#' @param dom_in Dataframe. Backseries domestic in migration data
#' @param dom_out Dataframe. Backseries domestic out migration data
#' @param upc_mye Dataframe or NULL. Backseries UPC data
#' @param popn_adjustment Dataframe or NULL. Population adjustment data for
#'   the projection period. Used for UPC or other adjustments
#' @param fertility_rates Dataframe. Model fertility rates
#' @param mortality_rates Dataframe. Model mortality rates
#' @param int_out_rates_flows Dataframe. Model international out migration rates
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
                                       int_in, dom_in, dom_out,
                                       upc_mye, popn_adjustment,
                                       fertility_rates, mortality_rates,
                                       int_out_rates_flows,
                                       first_proj_yr, last_proj_yr){
  
# Initialize lists to store filtered data for each year of the projection
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_int_out <- list(int_out %>% filter(year < first_proj_yr))
  proj_int_in <- list(int_in %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_dom_out <- list(dom_out %>% filter(year < first_proj_yr))
  proj_dom_in <- list(dom_in %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()

  # Loop through each year of the projection and add the data to the lists 
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
  
  #complete popn_adjustment
  # If the 'upc_mye' data is not provided, create an empty data frame with the necessary columns
  if(is.null(upc_mye)){
    upc_mye <- data.frame(year = numeric(), gss_code = character(),
                          sex = character(), age = numeric(), upc = numeric())
  }
  # Same if 'popn_adjustment' data is not provided
  if(is.null(popn_adjustment)){
    popn_adjustment <- data.frame(year = numeric(), gss_code = character(),
                                  sex = character(), age = numeric(), upc = numeric())
  }
  
# Combine 'popn_adjustment' with 'upc_mye' to ensure all necessary years and regions are covered.
  popn_adjustment <- popn_adjustment %>% 
    filter(year >= first_proj_yr) %>% 
    rbind(upc_mye) %>% 
    tidyr::complete(year = min(population$year):last_proj_yr,
                    gss_code = unique(population$gss_code),
                    sex = c("male", "female"),
                    age = 0:90,
                    fill = list(upc = 0)) %>% 
    data.frame() %>%

 # Filter the data frame to include only relevant gss_codes and years.   
    filter(gss_code %in% unique(population$gss_code),
           year %in% min(population$year):last_proj_yr) %>% 
    aggregate_regions(england=TRUE) %>% 
    rename(adjustment = upc)
  
  #Sum regional and national totals
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
  
  # Calculate net migration for domestic outflow
  proj_dom_net <- proj_dom_out %>% 
    mutate(dom_in = dom_out*-1) %>% 
    select(-dom_out) %>% 
    rbind(proj_dom_in) %>% 
    group_by(year, gss_code, age, sex) %>%  
    summarise(dom_net = sum(dom_in), .groups = 'drop_last') %>% 
    data.frame()

  # Calculate net migration for international migration
  proj_int_net <- regional_data$proj_int_out %>% 
    mutate(int_in = int_out*-1) %>% 
    select(-int_out) %>% 
    rbind(regional_data$proj_int_in) %>% 
    group_by(year, gss_code, age, sex) %>% 
    summarise(int_net = sum(int_in), .groups = 'drop_last') %>% 
    data.frame()
  
  # Combine domestic and international net migration and calculate total net migration
  total_net <- left_join(proj_int_net,
                         proj_dom_net,
                         by = c("year", "gss_code", "age", "sex")) %>% 
    mutate(total_net = dom_net + int_net) %>% 
    select(-dom_net, -int_net)
  
  
  #For int_in its always a flows so outputting the input and the output would be duplication
  #For domestic the rates dataframe is too large to output
  #For int_out if the model is setup to do flows then this will be a duplicate of the
  #output file, if its rates it will be a useful output
  
  return(list(population = regional_data$proj_popn,
              deaths = regional_data$proj_deaths,
              births = regional_data$proj_births,
              int_out = regional_data$proj_int_out,
              int_in = regional_data$proj_int_in,
              dom_out = proj_dom_out,
              dom_in = proj_dom_in,
              int_net = proj_int_net,
              dom_net = proj_dom_net,
              total_net = total_net,
              births_by_mothers_age = regional_data$proj_births_by_mother,
              natural_change = regional_data$proj_natural_change,
              fertility_rates = fertility_rates,
              mortality_rates = mortality_rates,
              int_out_rates_flows = int_out_rates_flows,
              popn_adjustment = popn_adjustment))
}