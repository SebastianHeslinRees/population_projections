#' Calculate average mortality rates based on past data
#'
#' Compares past mortality rates by LA, age and sex to an input mortality
#' curve to derive a set of scaling factors. The scaling factors are then
#' averaged or trended forward using regression. The resulting scaling
#' factor is applied to the input mortality curve to produce a set of
#' mortality rates.
#'
#' @param mye_population Dataframe. The mye backseries population.
#' @param births Dataframe. Births componet from the mye.
#' @param deaths Dataframe. Deaths component from the mye.
#' @param mortality_curves Dataframe. Age-and-sex-specific mortality
#'   rate by local authority.
#' @param last_data_year Integer. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg Integer. The number of years to use in calculating averages.
#' @param avg_or_trend Character. Should the averaged be caulated as the mean \code{Average},
#'   or by linear regression \code{Trend}.
#'
#' @return A data frame of mortality probabilities.
#'
#' @import purrr
#' @import dplyr
#' @import assertthat
#'
#' @export

initial_year_mortality_rate <- function(mye_population, births, deaths, mortality_curves, last_data_year, years_to_avg, avg_or_trend){
  
  
  check_init_mort_rates(mye_population, births, deaths, mortality_curves,
                        last_data_year, years_to_avg, avg_or_trend)
  
  aged_on_population <- mye_population %>%
    popmodules::popn_age_on() %>%
    filter(year != max(year)) %>%
    rbind(filter(births, age==0) %>% rename(popn = births))
  
  mortality_curves <- select(mortality_curves, -year)
  
  mortality_scaling_backseries <- left_join(aged_on_population, mortality_curves, by = c("gss_code", "age", "sex")) %>%
    mutate(curve_deaths = death_rate * popn) %>%
    left_join(deaths, by = c("gss_code", "age", "sex", "year")) %>%
    group_by(gss_code, year, sex) %>%
    summarise(actual_deaths = sum(deaths),
              curve_deaths = sum(curve_deaths)) %>%
    ungroup() %>%
    mutate(scaling = ifelse(actual_deaths == 0,
                            0,
                            actual_deaths / curve_deaths)) %>%
    select(gss_code, year, sex, scaling)
  
  #TODO make it work without this line
  #mortality_scaling_backseries <- filter(mortality_scaling_backseries, !is.na(scaling))
  
  check_mort_forward(mortality_scaling_backseries)
  
  if(avg_or_trend == "trend"){
    jump_off_mortality <- trend_mortality_forward(mortality_scaling_backseries, years_to_avg, last_data_year)
  }
  
  if(avg_or_trend == "average"){
    jump_off_mortality <- mean_mortality_forward(mortality_scaling_backseries, years_to_avg, last_data_year)
  }
  
  jump_off_mortality <- mortality_curves %>%
    left_join(jump_off_mortality, by = c("gss_code", "sex")) %>%
    mutate(death_rate = scaling * death_rate)
  
  return(jump_off_mortality)
  
}



#--------------------------------------------------------------------

# Function to apply linear regression to mortality scaling factors

trend_mortality_forward <- function(mortality_scaling_backseries, years_to_avg, last_data_year){
  
  regression <- function(df){
    lm(scaling ~ year, data=df)
  }
  
  get_coef <- function(df){
    coef(df)
  }
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  trended <- as.data.frame(mortality_scaling_backseries) %>%
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

# Function to calculate the mean of mortality scaling factors

mean_mortality_forward <- function(mortality_scaling_backseries, years_to_avg, last_data_year){
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  averaged <- filter(mortality_scaling_backseries, year %in% back_years) %>%
    group_by(gss_code, sex) %>%
    summarise(scaling = sum(scaling)/years_to_avg) %>%
    data.frame() %>%
    mutate(year = last_data_year+1) %>%
    select(gss_code, sex, year, scaling)
  
  return(averaged)
  
}


#--------------------------------------------------------------------

# Function to check that the input to initial_year_mortality_rates is all legal

check_init_mort_rates <- function(mye_population, births, deaths, mortality_curves,
                                  last_data_year, years_to_avg, avg_or_trend){
  
  # test input parameters are of the correct type
  assert_that(is.data.frame(mye_population),
              msg="mye_population expects a data frame as input")
  assert_that(is.data.frame(births),
              msg="births expects a data frame as input")
  assert_that(is.data.frame(deaths),
              msg="deaths expects a data frame as input")
  assert_that(is.data.frame(mortality_curves),
              msg="mortality_curves expects a data frame as input")
  assert_that(is.numeric(last_data_year),
              msg="last_data_year expects numeric input")
  assert_that(is.numeric(years_to_avg),
              msg="years_to_avg expects numeric input")
  assert_that(is.character(avg_or_trend),
              msg="avg_or_trend expects character input")
  

  validate_population(mye_population, col_data = "popn")
  validate_population(births, col_data = "births",
                      comparison_pop = mye_population, col_comparison = c("gss_code", "age", "sex"))
  validate_population(deaths, col_data = "deaths",
                      comparison_pop = mye_population, col_comparison = c("gss_code", "age", "sex"))
  validate_population(mortality_curves, col_data = "death_rate",
                      comparison_pop = mye_population, col_comparison = c("gss_code", "age", "sex"))
  
}


#--------------------------------------------------------------------

# Function to check that the input to trend_mortality_forward
# and mean_mortality_forward is all legal

check_mort_forward <- function(mortality_scaling_backseries){
  
  assert_that(is.data.frame(mortality_scaling_backseries),
              msg="mortality_scaling_backseries expects a dataframe")
  
  validate_population(mortality_scaling_backseries, col_aggregation = c("gss_code", "year", "sex"), data = "scaling")
}

