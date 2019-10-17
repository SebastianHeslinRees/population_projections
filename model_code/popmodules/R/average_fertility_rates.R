#' Calculate average fertility rates based on past data
#'
#' Compares past fertility rates by LA, age and sex to an input fertility
#' curve to derive a set of scaling factors. The scaling factors are then
#' averaged or trended forward using regression. The resulting scaling
#' factor is applied to the input fertility curve to produce a set of
#' fertility rates.
#'
#' @param mye_population Dataframe. The mye backseries population.
#' @param births Dataframe. Births componet from the mye.
#' @param deaths Dataframe. Deaths component_data from the mye.
#' @param target_curves Dataframe. Age-and-sex-specific fertility
#'   rate by local authority.
#' @param last_data_year Integer. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg Integer. The number of years to use in calculating averages.
#' @param avg_or_trend Character. Should the averaged be caulated as the mean \code{Average},
#'   or by linear regression \code{Trend}.
#'
#' @return A data frame of fertility probabilities.
#'
#' @import purrr
#' @import dplyr
#' @import assertthat
#'
#' @export
# 
# initial_year_fertility_rate <- function(mye_population, component_data, target_curves, last_data_year, years_to_avg, avg_or_trend){
#   
#   
#   # check_init_rates(mye_population, component_data, deaths, target_curves,
#   #                       last_data_year, years_to_avg, avg_or_trend)
#   
#   population <- mutate(mye_population, year=year+1) %>%
#     filter(year != max(year)) %>%
#     left_join(mye_population, by=c("gss_code","year","sex","age")) %>%   
#     mutate(popn = (popn.x + popn.y)/2)       %>%                     
#     mutate(popn = ifelse(is.na(popn),0,popn)) %>%
#     select(-popn.x, -popn.y) %>%
#     data.table(key=c("gss_code","sex","age","year"))
  
initial_year_fertility_rate <- function(population, component_data, target_curves, last_data_year, years_to_avg,
                                        avg_or_trend, data_col, output_col){
    
  #target_curves <- select(target_curves, -year)
  
  scaling_backseries <- left_join(population, target_curves, by = c("gss_code", "age", "sex")) %>%
    mutate(curve_rate = rate * popn) %>%
    left_join(component_data, by = c("gss_code", "age", "sex", "year")) %>%
    rename(value = data_col) %>%
    group_by(gss_code, year, sex) %>%
    summarise(actual = sum(value),
              curve_count = sum(curve_count)) %>%
    ungroup() %>%
    mutate(scaling = ifelse(actual == 0,
                            0,
                            actual / curve_count)) %>%
    select(gss_code, year, sex, scaling)
  

  #check_rate_forward(scaling_backseries)
  
  if(avg_or_trend == "trend"){
    jump_off_rates <- calc_trend_rate(scaling_backseries, years_to_avg, last_data_year, rate_col="scaling")
  }
  
  if(avg_or_trend == "average"){
    jump_off_rates <- calc_mean_rate(scaling_backseries, years_to_avg, last_data_year, rate_col="scaling")
  }
  
  
  jump_off_rates <- target_curves %>%
    left_join(jump_off_rates, by = c("gss_code", "sex")) %>%
    mutate(jump_off_rate = scaling * rate) %>%
    rename(!!output_col := jump_off_rate)
  
  return(jump_off_rates)
  
}


