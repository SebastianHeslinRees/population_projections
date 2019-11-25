#' Calculate average mortality probabilities based on past data and assign to
#' first projection year
#'
#' Compares past rates by LA and sex to an input
#' curve to derive a set of scaling factors. The scaling factors are then
#' averaged or trended forward using regression. The resulting scaling
#' factor is applied to the age specific input curve to produce a set of rates.
#'
#' @param popn_mye_path Character. Path to the MYE population data
#' @param births_mye_path Character. Path to the MYE births component data
#' @param deaths_mye_path Character. Path to the MYE deaths component data
#' @param target_curves_filepath Character. Path to the SNPP target mortality curves
#' @param last_data_year numeric. The last year of death and population data on which to calculate averages.
#' @param n_years_to_avg numeric. The number of years to use in calculating averages/trending forward.
#' @param avg_or_trend Character. Should the averaged be caulated as the mean \code{Average},
#'   or by linear regression \code{Trend}.
#' @param data_col Character. The column in the \code{deaths} dataframe containing the rates. Defaults to \code{deaths}
#' @param output_col Character. The name of the column in the output dataframe containing the calculated rates
#'
#' @return A data frame of mortality probabilities rates by LA, year, sex and age with the same age structure
#' as the target curves and overall rates scaled so that they are consistent with past deaths.
#'
#' @import dplyr
#' @import assertthat
#'
#' @export


scaled_mortality_curve <- function(popn_mye_path, births_mye_path, deaths_mye_path, target_curves_filepath,
                                   last_data_year, n_years_to_avg, avg_or_trend, data_col="deaths", output_col){

  population <- data.frame(readRDS(popn_mye_path))
  births <- data.frame(readRDS(births_mye_path))
  deaths <- data.frame(readRDS(deaths_mye_path))
  target_curves <- data.frame(readRDS(target_curves_filepath)) %>% select(-year)

  validate_scaled_mortality_curve_inputs(population, births, deaths, target_curves, last_data_year, n_years_to_avg,
                  avg_or_trend, data_col, output_col)

  #Deaths denominator
  population <- population %>%
    popmodules::popn_age_on(births=births) %>%
    select(gss_code, year, sex, age, popn) %>%
    arrange(gss_code, year, sex, age)

  # Calculate the total deaths per year for each geography and sex that the target mortality curve would would create from population
  # Compare to the total deaths per year for each geography and sex in the actual deaths
  # *scaling* tells you what you would need to scale each geog and sex of the target curve by to get the same total deaths as the actuals
  # This is done because we prefer the ONS age structure for the first projection year to the previous actuals age structures

  scaling_backseries <- left_join(population, target_curves, by = c("gss_code", "age", "sex")) %>%
    mutate(curve_count = rate * popn) %>%
    left_join(deaths, by = c("gss_code", "age", "sex", "year")) %>%
    rename(value = data_col) %>%
    group_by(gss_code, year, sex) %>%
    summarise(actual = sum(value),
              curve_count = sum(curve_count)) %>%
    as.data.frame() %>%
    mutate(scaling = ifelse(curve_count == 0,
                            0,
                            actual / curve_count)) %>%
    select(gss_code, year, sex, scaling)

  if(avg_or_trend == "trend"){
    averaged_scaling_factors <- calculate_rate_by_regression(scaling_backseries, n_years_regression = n_years_to_avg, last_data_year, data_col="scaling")
  }

  if(avg_or_trend == "average"){
    averaged_scaling_factors <- calculate_mean_from_backseries(scaling_backseries, n_years_to_avg, last_data_year, data_col="scaling")
  }

  validate_population(averaged_scaling_factors, col_aggregation = c("year", "gss_code", "sex"), col_data = "scaling")

  jump_off_rates <- target_curves %>%
    left_join(averaged_scaling_factors, by = c("gss_code", "sex")) %>%
    mutate(jump_off_rate = scaling * rate) %>%
    select(gss_code, year, sex, age, jump_off_rate) %>%
    rename(!!output_col := jump_off_rate)

  return(jump_off_rates)

}

#-----------------------------------------------

# Function to check that the input to scaled_mortality_curves is all legal
validate_scaled_mortality_curve_inputs <- function(population, births, deaths, target_curves, last_data_year, n_years_to_avg,
                                                avg_or_trend, data_col, output_col){

  # test input parameters are of the correct type
  assert_that(is.data.frame(population),
              msg="scaled_mortality_curve expects that population is a data frame")
  assert_that(is.data.frame(births),
              msg="scaled_mortality_curve expects that births is a data frame")
  assert_that(is.data.frame(deaths),
              msg="scaled_mortality_curve expects that deaths is a data frame")
  assert_that(is.data.frame(target_curves),
              msg="scaled_mortality_curve expects that target_curves is a data frame")
  assert_that(is.numeric(last_data_year),
              msg="scaled_mortality_curve expects that last_data_year is an numeric")
  assert_that(is.numeric(n_years_to_avg),
              msg="scaled_mortality_curve expects that n_years_to_avg is an numeric")
  assert_that(is.character(avg_or_trend),
              msg="scaled_mortality_curve expects that avg_or_trend is a character")
  assert_that(is.character(data_col),
              msg="scaled_mortality_curve expects that data_col is a character")
  assert_that(is.character(output_col),
              msg="scaled_mortality_curve expects that output_col is a character")
  assert_that(data_col %in% names(deaths),
              msg="scaled_mortality_curve expects that data_col is a column in deaths dataframe")

  validate_population(population, col_aggregation = c("gss_code", "year", "sex", "age"), col_data = "popn")
  validate_population(births, col_aggregation = c("gss_code", "year", "sex", "age"), col_data = "births")
  validate_population(deaths, col_aggregation = c("gss_code", "year", "sex", "age"), col_data = "deaths")
  validate_population(target_curves, col_aggregation = c("gss_code", "age", "sex"), col_data = "rate")

  invisible(TRUE)

  }

