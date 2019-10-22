#int_in <- readRDS("~/Projects/population_projections/input_data/mye/2018/international_in_ons_2019-10-11.rds")
#int_out <- readRDS("~/Projects/population_projections/input_data/mye/2018/international_out_ons_2019-10-11.rds")
#pop <- readRDS("~/Projects/population_projections/input_data/mye/2018/population_ons_2019-10-11.rds")

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

#' Calculate average international rates based on past data
#'
#' A wrepper for \code{calc_mean_rate} which prepares mye
#' and international in/out flow component data.
#'
#' @param population Dataframe. The mye backseries population.
#' @param component Dataframe. International in or out migration
#'   componet from the mye.
#' @param last_data_year Integer. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg Integer. The number of years to use in calculating averages.
#' @param data_col Character. The column in the component dataframe containing
#'   the flow data.
#' @param n_proj_years Numeric. The number of years to project forward
#'
#' @return A data frame of international migration probabilities.
#'
#' @import dplyr
#' @import assertthat
#' @import data.table
#'
#' @export

international_rates <- function(population, component, last_data_year, years_to_avg, data_col, n_proj_years) {

  population <- popn_age_on(population) %>%
    filter(population, year %in% unique(component$year))

  rate_backseries <- left_join(population, component, by=c("gss_code","year","sex","age")) %>%
    rename(value = data_col) %>%
    mutate(rate = ifelse(popn == 0, 0, value/popn)) %>%
    select(gss_code, year, sex, age, rate)

  rates <- calc_mean_rate(rate_backseries, years_to_avg, last_data_year, "rate")

  rates <- international_trajectory(rates, last_data_year, n_proj_years) %>%
    rbind(rate_backseries)

  return(rates)

}

#' Calculate average international flows based on past data
#'
#' Use past international flow data to calculate an average.
#' Produce a trajectory of internation flows for a projection
#' period.
#'
#' @param component Dataframe. International in or out migration
#'   componet from the mye.
#' @param last_data_year Integer. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg Integer. The number of years to use in calculating averages.
#' @param flow_col Character. The column in the component dataframe containing
#'   the flow data.
#' @param n_proj_years Numeric. The number of years to project forward
#'
#' @return A data frame of international migration probabilities.
#'
#' @import dplyr
#' @import assertthat
#' @import data.table
#'
#' @export

international_flows <- function(component, last_data_year, years_to_avg, flow_col, n_proj_years){

  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)

  flows <- component %>%
    rename(value = flow_col) %>%
    filter(year %in% back_years) %>%
    group_by(gss_code, sex, age) %>%
    summarise(value = sum(value)/years_to_avg) %>%
    ungroup() %>%
    rename(!!flow_col := value)

  flows <- international_trajectory(flows, last_data_year, n_proj_years) %>%
    select(names(component)) %>%
    rbind(component)

  return(flows)

}

#--------------------------------------------------

international_trajectory <- function(component, last_data_year, n_proj_years){

  trajectory <- vector("list", length = last_data_year+n_proj_years)

  for(yr in c((last_data_year+1):(last_data_year+n_proj_years))){
    trajectory[[yr]] <- mutate(component, year = yr)
  }

  trajectory <- data.table::rbindlist(trajectory)

  return(trajectory)

}



  # population <- pop
  # component <- int_out
  # years_to_avg <- 5
  # last_data_year <- max(component$year)
  # data_col <- "int_out"
  # n_proj_years <- 25
  # flow_col <- "int_in"
  #
  # rates <- international_rates(population, component, years_to_avg, last_data_year, rate_col, n_proj_years)
  # flows <- international_flows(int_in, years_to_avg, last_data_year, flow_col, n_proj_years)
  #

