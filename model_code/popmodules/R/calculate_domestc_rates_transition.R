#' Calculate 2 sets of average origin-destination domestic migration rates based on past
#' data
#'
#' A wrapper for \code{get_rate_backseries} and \code{calculate_mean_domestic_rates}.
#' Given dataframes of past population and past domestic migration flows, calculate mean rates
#' over a given number of years. If the sum of the outrates for any age/sex/LA combination is
#' greater than a defined cap then scale all rates so that sum is equal to the cap. The process
#' is run twice with the first set of rates saved as \code{initial} and the second set as
#' \code{longterm} in an output list.
#'
#' @param popn_mye_path Path to population mid-year estimates.
#' @param births_mye_path Path to births mid-year estimates.
#' @param col_aggregation Vector of column names used for aggregation in the
#'   input in \code{component_mye_path}. If column names are different from MYE
#'   columns, specify the mapping from MYE to the component data with a named
#'   character vector. Default \code{c("year","gss_code"="gss_out","gss_in","sex","age")}.
#' @param col_component String. Column name for data in the
#'   \code{component_mye_path} data frame. Default \code{value}
#' @param last_data_year_initial numeric. The last year of data on which to calculate
#'   averages. For initial rates upto transition year.
#' @param n_years_to_avg_initial numeric. The number of years to use in calculating averages.
#'   For initial rates upto transition year.
#' @param last_data_year_longterm numeric. The last year of data on which to calculate
#'   averages. For long-term rates after and including transition year.
#' @param n_years_to_avg_longterm numeric. The number of years to use in calculating averages.
#'   For long-term rates after and including transition year.   
#' @param rate_cap numeric. A value which calculated rates cannot exceed. Where rates exceed \code{rate_cap}
#'   they are limited to the value of \code{rate_cap}.
#'
#' @return A data frame of origin-destination migration probabilities by LA, year, sex and age.
#'
#' @export

calculate_domestic_rates_transition <- function(dom_origin_destination_path,
                                                popn_mye_path,
                                                births_mye_path,
                                                col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                col_component = "value",
                                                rate_cap = 0.8,
                                                last_data_year_initial,
                                                years_to_avg_initial,
                                                last_data_year_longterm=NULL,
                                                years_to_avg_longterm=NULL,
                                                domestic_transition_yr=NULL){
  
  min_yr <- min(c(last_data_year_initial-years_to_avg_initial+1,
                  last_data_year_longterm-years_to_avg_longterm+1))
  max_yr <- max(last_data_year_initial, last_data_year_longterm)
  years_backseries <- min_yr:max_yr
  
  domestic_rate_backseries <- get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                  popn_mye_path = popn_mye_path,
                                                  births_mye_path = births_mye_path,
                                                  years_backseries = years_backseries,
                                                  col_partial_match = c("gss_out","gss_in"),
                                                  col_aggregation = col_aggregation,
                                                  col_component = col_component,
                                                  rate_cap = NULL)
  
  domestic_rates <- list(initial = calculate_mean_domestic_rates(domestic_rate_backseries,
                                                                 last_data_year = last_data_year_initial,
                                                                 n_years_to_avg = years_to_avg_initial,
                                                                 rate_cap = rate_cap))
  if(!is.null(domestic_transition_yr)){
    domestic_rates[['longterm']] <- calculate_mean_domestic_rates(domestic_rate_backseries,
                                                                  last_data_year = last_data_year_longterm,
                                                                  n_years_to_avg = years_to_avg_longterm,
                                                                  rate_cap = rate_cap)
  }
  
  return(domestic_rates)
  
}