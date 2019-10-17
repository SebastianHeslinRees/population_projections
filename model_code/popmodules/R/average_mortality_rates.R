#' Calculate average rates based on past data
#'
#' Compares past rates by LA, age and sex to an input 
#' curve to derive a set of scaling factors. The scaling factors are then
#' averaged or trended forward using regression. The resulting scaling
#' factor is applied to the input curve to produce a set of rates.
#'
#' @param mye_population Dataframe or List. The mye backseries population.
#' For mortality rates the input is a list of 2 elements \code{population} aand \code{births}.
#' For fertility rates the input is a dataframe of \code{population}
#' @param component_data Dataframe. \code{Births} or \code{deaths} by sex and age.
#' @param target_curves Dataframe. Age-and-sex-specific mortality or fertiltiy
#'   rates by local authority.
#' @param last_data_year numeric. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg numeric. The number of years to use in calculating averages.
#' @param avg_or_trend Character. Should the averaged be caulated as the mean \code{Average},
#'   or by linear regression \code{Trend}.
#' @param data_col Character. The column in the \code{component_data} dataframe containing the rates
#' @param output_col Character. The name of the column in the output dataframe containing the calculated rates
#'
#' @return A data frame of mortality probabilities or fertility rates by LA, year, sex and age.
#'
#' @import purrr
#' @import dplyr
#' @import assertthat
#'
#' @export


initial_year_rate <- function(population, component_data, target_curves, last_data_year, years_to_avg,
                              avg_or_trend, data_col, output_col){
  
  check_init_rate(population, component_data, target_curves, last_data_year, years_to_avg,
                          avg_or_trend, data_col, output_col)
  
  
  if(data_col == "deaths"){
    population <- deaths_denominator(population)
  }
  if(data_col== "births"){
    population <- births_denominator(population)
  }
  
  if("year" %in% names(target_curves)){target_curves <- select(target_curves, -year)}
  
  scaling_backseries <- left_join(population, target_curves, by = c("gss_code", "age", "sex")) %>%
    mutate(curve_count = rate * popn) %>%
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
    select(gss_code, year, sex, age, jump_off_rate) %>%
    rename(!!output_col := jump_off_rate)
  
  return(jump_off_rates)
  
}


#--------------------------------------------------------------------

#' Calculate average rates based on past data
#'
#' Given a backseries of rates the function will return an average rate
#'
#' @param rate_backseries Dataframe. A set of rates by LA, sex and age
#' @param years_to_avg numeric. Number of years data to include in the average
#' @param last_data_year numeric. The final year of data to include in the average
#' @param rate_col Character. The name of the column containing the rates
#' 
#' @return A data frame of mortality probabilities or fertility rates.
#'
#' @import dplyr
#' @import assertthat
#'
#' @export

calc_mean_rate <- function(rate_backseries, years_to_avg, last_data_year, rate_col){
  
  assert_that(is.data.frame(rate_backseries),
              msg="calc_trend_rate expects that rate_backseries is a dataframe")
  assert_that(is.numeric(years_to_avg),
              msg="calc_trend_rate expects that years_to_avg is an numeric")
  assert_that(is.numeric(last_data_year),
              msg="calc_trend_rate expects that last_data_year is an numeric")
  assert_that(is.character(rate_col),
              msg="calc_trend_rate expects that rate_col is a column")
  assert_that(rate_col %in% names(rate_backseries),
              msg = "in calc_trend_rate the rtae_col variable must be the name of a column in the rate_backseries dataframe")
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  averaged <- rate_backseries %>%
    rename(rate = rate_col) %>%
    filter(year %in% back_years) %>%
group_by(gss_code, sex, age) %>%
    summarise(rate = sum(rate)/years_to_avg) %>%
    ungroup() %>%
    mutate(year = last_data_year+1) %>%
    select(gss_code, year, sex, age, rate) %>%
    rename(!!rate_col := rate)
  
  return(averaged)
  
}


#--------------------------------------------------------------------

# Function to apply linear regression to a set of rates

calc_trend_rate <- function(rate_backseries, years_to_avg, last_data_year, rate_col){
  
  assert_that(is.data.frame(rate_backseries),
              msg="calc_trend_rate expects that rate_backseries is a dataframe")
  assert_that(is.numeric(years_to_avg),
              msg="calc_trend_rate expects that years_to_avg is an numeric")
  assert_that(is.numeric(last_data_year),
              msg="calc_trend_rate expects that last_data_year is an numeric")
  assert_that(is.character(rate_col),
              msg="calc_trend_rate expects that rate_col is a column")
  assert_that(rate_col %in% names(rate_backseries),
              msg = "in calc_trend_rate the rtae_col variable must be the name of a column in the rate_backseries dataframe")
  
  
  regression <- function(df){
    lm(rate ~ year, data=df)
  }
  
  get_coef <- function(df){
    coef(df)
  }
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  trended <- as.data.frame(rate_backseries) %>%
    rename(rate = rate_col) %>%
    filter(year %in% back_years) %>%
    mutate(year = years_to_avg - last_data_year + year)%>%
    group_by(gss_code, sex, age) %>%
    tidyr::nest() %>%
    mutate(
      Model = map(data, regression),
      Coef = Model %>% map(get_coef),
      Intercept = Coef %>% map_dbl("(Intercept)"),
      Slope = Coef %>% map_dbl("year"),
      rate = Slope * (years_to_avg+1) + Intercept,
      rate = ifelse(rate < 0, 0, rate)) %>%
    as.data.frame()  %>%
    mutate(year = last_data_year + 1) %>%
    select(gss_code, year, sex, age, rate) %>%
    rename(!!rate_col := rate)
  
  return(trended)
  
}


#--------------------------------------------------------------------

#Function to create denominators for initial rate calculation

deaths_denominator <- function(population){
  
  assert_that(is.data.frame(population[[1]]),
              msg="deaths_denominator expects population[[1]] to be a dataframe")
  assert_that(is.data.frame(population[[2]]),
              msg="deaths_denominator expects population[[2]] to be a dataframe")
  
  #Deaths denominator
  births <- population[[2]] %>%
    filter(age==0) %>%
    rename(popn = births)
  
  population <- population[[1]] %>%
    popmodules::popn_age_on() %>%
    filter(year != max(year)) %>%
    rbind(births)%>%
    select(gss_code, year, sex, age, popn) %>%
    arrange(gss_code, year, sex, age)
  
  return(population)
  
}

#-----------------------------------------------

#Function for creating birth denominator population

births_denominator <- function(population) {
  
  assert_that(is.data.frame(population),
              msg="births_denominator expects population to be a dataframe")
  
  population <- mutate(population, year=year+1) %>%
    filter(year != max(year)) %>%
    left_join(population, by=c("gss_code","year","sex","age")) %>%
    mutate(popn = (popn.x + popn.y)/2)       %>%
    mutate(popn = ifelse(is.na(popn),0,popn)) %>%
    select(gss_code, year, sex, age, popn) %>%
    arrange(gss_code, year, sex, age)
  
  return(population)
}




#-----------------------------------------------

# Function to check that the input to initial_year_mortality_rates is all legal

check_init_rate <- function(population, component_data, target_curves, last_data_year, years_to_avg,
                avg_or_trend, data_col, output_col){
  
  # test input parameters are of the correct type
  assert_that(is.data.frame(population)|is.list(population),
              msg="initial_year_rate expects that population is a data frame or a list")
  
  assert_that(is.data.frame(component_data),
              msg="initial_year_rate expects that component_data is a data frame")
  assert_that(is.data.frame(target_curves),
              msg="initial_year_rate expects that target_curves is a data frame")
  assert_that(is.numeric(last_data_year),
              msg="initial_year_rate expects that last_data_year is an numeric")
  assert_that(is.numeric(years_to_avg),
              msg="initial_year_rate expects that years_to_avg is an numeric")
  assert_that(is.character(avg_or_trend),
              msg="initial_year_rate expects that avg_or_trend is a character")
  assert_that(is.character(data_col),
              msg="initial_year_rate expects that data_col is a character")
  assert_that(is.character(output_col),
              msg="initial_year_rate expects that output_col is a character")
  assert_that(data_col %in% names(component_data),
              msg="initial_year_rate expects that data_col is a column in component_data dataframe")
  
 }



