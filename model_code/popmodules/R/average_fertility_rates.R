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
#' @param deaths Dataframe. Deaths component from the mye.
#' @param fertility_curves Dataframe. Age-and-sex-specific fertility
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

initial_year_fertility_rate <- function(mye_population, births, fertility_curves, last_data_year, years_to_avg, avg_or_trend){
  
  
  # check_init_rates(mye_population, births, deaths, fertility_curves,
  #                       last_data_year, years_to_avg, avg_or_trend)
  
  population <- mutate(mye_population, year=year+1) %>%
    filter(year != max(year)) %>%
    left_join(mye_population, by=c("gss_code","year","sex","age")) %>%   
    mutate(popn = (popn.x + popn.y)/2)       %>%                     
    mutate(popn = ifelse(is.na(popn),0,popn)) %>%
    select(-popn.x, -popn.y) %>%
    data.table(key=c("gss_code","sex","age","year"))
  
  fertility_curves <- select(fertility_curves, -year)
  
  fertility_scaling_backseries <- left_join(population, fertility_curves, by = c("gss_code", "age", "sex")) %>%
    mutate(curve_births = birth_rate * popn) %>%
    left_join(births, by = c("gss_code", "age", "sex", "year")) %>%
    group_by(gss_code, year, sex) %>%
    summarise(actual_births = sum(births),
              curve_births = sum(curve_deaths)) %>%
    ungroup() %>%
    mutate(scaling = ifelse(actual_births == 0,
                            0,
                            actual_births / births_deaths)) %>%
    select(gss_code, year, sex, scaling)
  

  #check_rate_forward(fertility_scaling_backseries)
  
  if(avg_or_trend == "trend"){
    jump_off_fertility <- calc_trend_rate(fertility_scaling_backseries, years_to_avg, last_data_year)
  }
  
  if(avg_or_trend == "average"){
    jump_off_fertility <- calc_mean_rate(fertility_scaling_backseries, years_to_avg, last_data_year)
  }
  
  jump_off_fertility <- fertility_curves %>%
    left_join(jump_off_fertility, by = c("gss_code", "sex")) %>%
    mutate(birth_rate = scaling * birth_rate)
  
  return(jump_off_fertility)
  
}


