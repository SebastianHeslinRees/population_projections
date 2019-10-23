#' Calculate average fertility rates based on past data
#'
#' Compares past rates by LA, age and sex to an input
#' curve to derive a set of scaling factors. The scaling factors are then
#' averaged or trended forward using regression. The resulting scaling
#' factor is applied to the input curve to produce a set of rates.
#'
#' @param popn_mye_path Character. Path to the MYE population data
#' @param births_mye_path Character. Path to the MYE births component data
#' @param target_curves_filepath Character. Path to the SNPP target fertility curves
#' @param last_data_year numeric. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg numeric. The number of years to use in calculating averages.
#' @param avg_or_trend Character. Should the averaged be caulated as the mean \code{Average},
#'   or by linear regression \code{Trend}.
#' @param data_col Character. The column in the \code{births} dataframe containing the rates. Defaults to \code{births}
#' @param output_col Character. The name of the column in the output dataframe containing the calculated rates
#'
#' @return A data frame of mortality probabilities or fertility rates by LA, year, sex and age.
#'
#' @import purrr
#' @import dplyr
#' @import assertthat
#'
#' @export


scaled_fertility_curve <- function(popn_mye_path, births_mye_path, target_curves_filepath, last_data_year,
                                  years_to_avg, avg_or_trend, data_col="births", output_col){

  validate_scaled_fertility_curve_filetype(popn_mye_path, births_mye_path, target_curves_filepath)

  population <- data.frame(readRDS(popn_mye_path))
  births <- data.frame(readRDS(births_mye_path))
  target_curves <- readRDS(target_curves_filepath) %>% select(-year)

  check_init_rate(population, component_data, target_curves, last_data_year, years_to_avg,
                  avg_or_trend, data_col, output_col)

    population <- births_denominator(population)

    births <- group_by(births, year, gss_code) %>%
      summarise(births = sum(births))

    population <- filter(population, sex == "female", age %in% unique(target_curves$age))

    scaling_backseries <- left_join(target_curves, population, by = c("gss_code", "age", "sex")) %>%
      mutate(curve_count = rate * popn) %>%
      group_by(year, gss_code) %>%
      summarise(curve_count = sum(curve_count)) %>%
      ungroup() %>%
      left_join(births, by = c("gss_code", "year")) %>%
      rename(actual = data_col) %>%
      mutate(scaling = ifelse(actual == 0,
                              0,
                              actual / curve_count)) %>%
      select(gss_code, year, scaling)

    if(avg_or_trend == "trend"){
      averaged_scaling_factors <- calculate_rate_by_regression(scaling_backseries, years_to_avg, last_data_year, data_col="scaling",
                                                               col_aggregation = "gss_code")
    }

    if(avg_or_trend == "average"){
      averaged_scaling_factors <- calculate_mean_from_backseries(scaling_backseries, years_to_avg, last_data_year, data_col="scaling",
                                                                 col_aggregation = "gss_code")
    }

    jump_off_rates <- target_curves %>%
      left_join(averaged_scaling_factors, by = "gss_code") %>%
      mutate(jump_off_rate = scaling * rate) %>%
      select(gss_code, year, sex, age, jump_off_rate) %>%
      rename(!!output_col := jump_off_rate)

  return(jump_off_rates)

}

#--------------------------------------------------------------------

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

check_init_rate <- function(population, births, target_curves, last_data_year, years_to_avg,
                            avg_or_trend, data_col, output_col){

  # test input parameters are of the correct type
  assert_that(is.data.frame(population)|is.list(population),
              msg="initial_year_rate expects that population is a data frame or a list")

  assert_that(is.data.frame(births),
              msg="initial_year_rate expects that births is a data frame")
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
  assert_that(data_col %in% names(births),
              msg="initial_year_rate expects that data_col is a column in births dataframe")

}

#---------------------------------------------

validate_scaled_fertility_curve_filetype <- function(path_1, path_2, path_3) {

  for(i in c(path_1, path_2, path_3)){
    filepath <- i
    file_ext <- tolower(strsplit(basename(filepath), split="\\.")[[1]][[2]])

    assertthat::assert_that(file_ext == "rds",
                            msg = paste(i,": file must be a .rds file"))
  }

  invisible(TRUE)

}

