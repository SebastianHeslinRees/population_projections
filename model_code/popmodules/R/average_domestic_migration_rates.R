#' Calculate average origin-destination domestic migration rates based on past
#' data
#'
#' [description]
#'
#' @param origin_destination_rates Dataframe. Origin-destination flows by geography, year, sex and age.
#' @param last_data_year numeric. The last year of data on which to calculate
#'   averages.
#' @param years_to_avg numeric. The number of years to use in calculating averages.
#' @param col_rate Character. The column in the \code{origin_destinatation_flows} dataframe containing the rates
#'
#' @return A data frame of origin-destination migration probabilities by LA, year, sex and age.
#'
#' @import purrr
#' @import dplyr
#' @import assertthat
#' @import data.table
#'
#' @export

average_domestic_migration_rates <- function(origin_destination_rates,
                                             last_data_year,
                                             years_to_avg,
                                             col_rate = "rate",
                                             rate_cap = 0.8) {
  # validate input
  check_average_domestic_rate_input(origin_destination_rates, last_data_year, years_to_avg, col_rate)
  if(col_rate != "rate") {
    i <- names(origin_destination_rates) == col_rate
    names(origin_destination_rates)[i] <- "rate"
  }

  backseries_years <- (last_data_year - years_to_avg + 1):last_data_year

  # data.table because it's so big
  data.table::setDT(origin_destination_rates)
  origin_destination_rates[year %in% backseries_years, ]
    origin_destination_rates <- origin_destination_rates[, .(rate = sum(rate)/years_to_avg),
                                                         .(gss_out, gss_in, age, sex)]

    # tidyverse equivalent
  if(FALSE) {
    origin_destination_rates <- filter(origin_destination_rates, year %in% backseries_years)

    origin_destination_rates %>%
      group_by(gss_out, gss_in, age, sex) %>%
      summarise(rate = rate/years_to_avg)
  }

  if(col_rate != "rate") {
    i <- names(origin_destination_rates) == "rate"
    names(origin_destination_rates)[i] <- col_rate
  }
    
  return(origin_destination_rates)

}




# ================================================================

# validate inputs for the above
check_average_domestic_rate_input <- function(origin_destination_rates, last_data_year, years_to_avg, col_rate) {

  assert_that(is.data.frame(origin_destination_rates),
              msg="average_domestic_migration_rates expects that origin_destination_rates is a data frame")
  assert_that(is.count(last_data_year),
              msg="average_domestic_migration_rates expects that last_data_year is a numeric")
  assert_that(is.count(years_to_avg),
              msg="average_domestic_migration_rates expects that years_to_avg is a numeric")
  assert_that(is.character(col_rate),
              msg="average_domestic_migration_rates expects that data_col is a character")
  assert_that(col_rate %in% names(origin_destination_rates),
              msg="average_domestic_migration_rates expects that data_col is a column in component_data dataframe")

  backseries_years <- (last_data_year - years_to_avg + 1):last_data_year
  assert_that(all(backseries_years %in% origin_destination_rates$year),
              msg = paste(c("average_domestic_migration_rates expects these years to be present in the origin-destination data:",
                            backseries_years), collapse=" "))
}
