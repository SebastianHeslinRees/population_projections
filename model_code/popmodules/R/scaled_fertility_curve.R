#' Calculate average fertility rates based on past data and assign to first
#' projection year
#'
#' Compares past rates by LA, age and sex to an input
#' curve to derive a set of scaling factors. The scaling factors are then
#' averaged or trended forward using regression. The resulting scaling
#' factor is applied to the input curve to produce a set of rates.
#'
#' @param popn_mye_path Character. Path to the MYE population data
#' @param births_mye_path Character. Path to the MYE births component data
#' @param target_curves_filepath Character. Path to the SNPP target fertility curves
#' @param last_data_year numeric. The last year of births data on which to calculate
#'   averages.
#' @param n_years_to_avg numeric. The number of years to use in calculating averages/trend forward.
#' @param avg_or_trend Character. Should the averaged be caulated as the mean \code{Average},
#'   or by linear regression \code{Trend}.
#' @param data_col Character. The column in the \code{births} dataframe containing the rates. Defaults to \code{births}
#' @param output_col Character. The name of the column in the output dataframe containing the calculated rates
#'
#' @return A data frame of mortality probabilities or fertility rates by LA, year, sex and age with the same age structure
#' as the target curves and overall rates scaled so that they are consistent with past births.
#'
#' @import dplyr
#' @import assertthat
#' @importFrom data.table setnames
#' @importFrom dtplyr lazy_dt
#'
#' @export


scaled_fertility_curve <- function(popn_mye_path, births_mye_path, target_curves_filepath, last_data_year,
                                   n_years_to_avg, avg_or_trend, data_col="births", output_col){

  population <- data.frame(readRDS(popn_mye_path))
  births <- data.frame(readRDS(births_mye_path))
  target_curves <- readRDS(target_curves_filepath) %>% select(-year)

  validate_scaled_fertility_curve_input(population, births, target_curves, last_data_year, n_years_to_avg,
                                        avg_or_trend, data_col, output_col)

  births <- lazy_dt(births) %>%
    group_by(year, gss_code) %>%
    summarise(births = sum(births)) %>%
    as.data.frame()

  population <-  lazy_dt(population) %>%
    filter(sex == "female", age %in% unique(target_curves$age))


  # Calculate the total births per year for each geography and sex that the target fertility curve would would create from population
  # Compare to the total births per year for each geography and sex in the actual births
  # *scaling* tells you what you would need to scale each geog and sex of the target curve by to get the same total births as the actuals
  # This is done because we prefer the ONS age structure for the first projection year to the previous actuals age structures
  scaling_backseries <- lazy_dt(target_curves) %>%
    left_join(population, by = c("gss_code", "age", "sex")) %>%
    mutate(curve_count = rate * popn) %>%
    group_by(year, gss_code) %>%
    summarise(curve_count = sum(curve_count)) %>%
    ungroup() %>%
    as.data.frame() %>%   # dtplyr needs to take a break part way through
    lazy_dt() %>%
    left_join(births, by = c("gss_code", "year"))  %>%
    as.data.frame() %>%
    rename(actual = data_col) %>%
    mutate(scaling = ifelse(curve_count == 0,
                            0,
                            actual / curve_count)) %>%
    select(gss_code, year, scaling)

  if(avg_or_trend == "trend"){
    averaged_scaling_factors <- calculate_rate_by_regression(scaling_backseries, n_years_regression = n_years_to_avg, last_data_year, data_col="scaling",
                                                             col_aggregation = "gss_code")
  }

  if(avg_or_trend == "average"){
    averaged_scaling_factors <- calculate_mean_from_backseries(scaling_backseries, n_years_to_avg, last_data_year, data_col="scaling",
                                                               col_aggregation = "gss_code")
  }

  validate_population(averaged_scaling_factors, col_aggregation = c("year", "gss_code"), col_data = "scaling")

  jump_off_rates <- target_curves %>%
    left_join(averaged_scaling_factors, by = "gss_code") %>%
    mutate(jump_off_rate = scaling * rate) %>%
    select(gss_code, year, sex, age, jump_off_rate) %>%
    rename(!!output_col := jump_off_rate)

  rates_backseries <- left_join(scaling_backseries, target_curves, by="gss_code") %>%
    filter(!is.na(scaling)) %>%
    mutate(rate = rate*scaling) %>%
    select(gss_code, year, sex, age, rate) %>%
    rename(!!output_col := rate)

  return(rbind(rates_backseries, jump_off_rates) %>%
           arrange(gss_code, year, sex, age))

}

#--------------------------------------------------------------------

#Function for creating birth denominator population

births_denominator <- function(population) {

  #Not used at them moment
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

validate_scaled_fertility_curve_input <- function(population, births, target_curves, last_data_year, n_years_to_avg,
                                                  avg_or_trend, data_col, output_col){

  # test input parameters are of the correct type
  assert_that(is.data.frame(population),
              msg="scaled_fertility_curve expects that population is a data frame")
  assert_that(is.data.frame(births),
              msg="scaled_fertility_curve expects that births is a data frame")
  assert_that(is.data.frame(target_curves),
              msg="scaled_fertility_curve expects that target_curves is a data frame")
  assert_that(is.numeric(last_data_year),
              msg="scaled_fertility_curve expects that last_data_year is an numeric")
  assert_that(is.numeric(n_years_to_avg),
              msg="scaled_fertility_curve expects that n_years_to_avg is an numeric")
  assert_that(is.character(avg_or_trend),
              msg="scaled_fertility_curve expects that avg_or_trend is a character")
  assert_that(is.character(data_col),
              msg="scaled_fertility_curve expects that data_col is a character")
  assert_that(is.character(output_col),
              msg="scaled_fertility_curve expects that output_col is a character")
  assert_that(data_col %in% names(births),
              msg="scaled_fertility_curve expects that data_col is a column in births dataframe")

  validate_population(population, col_aggregation = c("gss_code", "year", "sex", "age"), col_data = "popn")
  validate_population(births, col_aggregation = c("gss_code", "year", "sex", "age"), col_data = "births")
  validate_population(target_curves, col_aggregation = c("gss_code", "age", "sex"), col_data = "rate")

  invisible(TRUE)
}


