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
  
  
  # check_init_mort_rates(mye_population, births, deaths, fertility_curves,
  #                       last_data_year, years_to_avg, avg_or_trend)
  
  # actual_births <- births %>%
  #   group_by(gss_code, year) %>%
  #   summarise(actual = sum(births)) %>%
  #   data.frame()
  
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
  
  #TODO make it work without this line
  #fertility_scaling_backseries <- filter(fertility_scaling_backseries, !is.na(scaling))
  
  #check_fert_forward(fertility_scaling_backseries)
  
  if(avg_or_trend == "trend"){
    jump_off_fertility <- trend_fertility_forward(fertility_scaling_backseries, years_to_avg, last_data_year)
  }
  
  if(avg_or_trend == "average"){
    jump_off_fertility <- mean_fertility_forward(fertility_scaling_backseries, years_to_avg, last_data_year)
  }
  
  jump_off_fertility <- fertility_curves %>%
    left_join(jump_off_fertility, by = c("gss_code", "sex")) %>%
    mutate(birth_rate = scaling * birth_rate)
  
  return(jump_off_fertility)
  
}



#--------------------------------------------------------------------

# Function to apply linear regression to fertility scaling factors

trend_fertility_forward <- function(fertility_scaling_backseries, years_to_avg, last_data_year){
  
  regression <- function(df){
    lm(scaling ~ year, data=df)
  }
  
  get_coef <- function(df){
    coef(df)
  }
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  trended <- as.data.frame(fertility_scaling_backseries) %>%
    filter(year %in% back_years) %>%
    mutate(year = years_to_avg - last_data_year + year)%>%
    group_by(gss_code, sex)%>%
    tidyr::nest() %>%
    mutate(
      Model = map(data, regression),
      Coef = Model %>% map(get_coef),
      Intercept = Coef %>% map_dbl("(Intercept)"),
      Slope = Coef %>% map_dbl("year"),
      scaling = Slope * (years_to_avg+1) + Intercept,
      scaling = ifelse(scaling < 0, 0, scaling)) %>%
    as.data.frame()  %>%
    mutate(year = last_data_year + 1) %>%
    select(gss_code, sex, year, scaling)
  
  return(trended)
  
}


#--------------------------------------------------------------------

# Function to calculate the mean of fertility scaling factors

mean_fertility_forward <- function(fertility_scaling_backseries, years_to_avg, last_data_year){
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  averaged <- filter(fertility_scaling_backseries, year %in% back_years) %>%
    group_by(gss_code, sex) %>%
    summarise(scaling = sum(scaling)/years_to_avg) %>%
    data.frame() %>%
    mutate(year = last_data_year+1) %>%
    select(gss_code, sex, year, scaling)
  
  return(averaged)
  
}


#--------------------------------------------------------------------

# Function to check that the input to initial_year_fertility_rates is all legal

check_init_fert_rates <- function(mye_population, births, fertility_curves,
                                  last_data_year, years_to_avg, avg_or_trend){
  
  # test input parameters are of the correct type
  assert_that(is.data.frame(mye_population),
              msg="mye_population expects a data frame as input")
  assert_that(is.data.frame(births),
              msg="births expects a data frame as input")
  assert_that(is.data.frame(fertility_curves),
              msg="fertility_curves expects a data frame as input")
  assert_that(is.numeric(last_data_year),
              msg="last_data_year expects numeric input")
  assert_that(is.numeric(years_to_avg),
              msg="years_to_avg expects numeric input")
  assert_that(is.character(avg_or_trend),
              msg="avg_or_trend expects character input")
  

  validate_population(mye_population, col_data = "popn")
  validate_population(births, col_data = "births",
                      comparison_pop = mye_population, col_comparison = c("gss_code", "age", "sex"))
  validate_population(fertility_curves, col_data = "death_rate",
                      comparison_pop = mye_population, col_comparison = c("gss_code", "age", "sex"))
  
}


#--------------------------------------------------------------------

# Function to check that the input to trend_fertility_forward
# and mean_fertility_forward is all legal

check_fert_forward <- function(fertility_scaling_backseries){
  
  assert_that(is.data.frame(fertility_scaling_backseries),
              msg="fertility_scaling_backseries expects a dataframe")
  
  validate_population(fertility_scaling_backseries, col_aggregation = c("gss_code", "year", "sex"), data = "scaling")
}

