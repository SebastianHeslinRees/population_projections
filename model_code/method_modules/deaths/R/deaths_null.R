#' Template deaths function that returns a fixed number of deaths (default zero)
#' for each aggregation level.
#'
#' This is designed to be used as a placeholder deaths function during model
#' testing and a template for other deaths functions, along with its tests.
#'
#' @param pop A data frame containing population data
#' @param col_aggregation A string giving the names of columns to which the
#'   output deaths will be aggregated to. Default \code{c("year", "gss_code",
#'   "age", "sex")}
#' @param count String. Name of column with population counts. Default "value"
#' @param const Numeric. Number of deaths to return per geography. Defaults to
#'   zero, but can be set to any positive number
#' @param error_negative_pop Logical. Throw error if deaths result in a negative
#'   population count. Setting to FALSE creates a warning instead. Default TRUE.
#'
#' @return A data frame of deaths with one row for each distinct value of the
#'   \code{col_aggregation} columns, a column named deaths with value
#'   \code{const} and a column named age with value 0.
#'
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases
#'
#' @export
#'
deaths_null <- function(pop,
                        col_aggregation = c("year", "gss_code", "age", "sex"),
                        count="value",
                        const = 0,
                        error_negative_pop = TRUE) {

  validate_deaths_input(pop, col_aggregation, count, const, error_negative_pop)
  col_aggregation <- names(pop)[ names(pop) %in% col_aggregation] # order to match input ordering, remove duplicates

  # Calculate deaths
  deaths <- dplyr::mutate(pop, deaths = const)

  # Check for negative population
  ix_negative <- deaths[["deaths"]] > deaths[[count]]
  if(any(ix_negative) & error_negative_pop) {
    stop(paste("deaths_null created more deaths than population in", sum(ix_negative), "rows"))
  }
  if(any(ix_negative) & !error_negative_pop) {
    warning(paste("deaths_null created more deaths than population in", sum(ix_negative), "rows.",
                  "Adjusting deaths to prevent negative population"))
    deaths[ix_negative, "deaths"] <- deaths[ix_negative, count]
  }

  # Select the columns we want to output
  deaths <- deaths[c(col_aggregation, "deaths")]

  validate_deaths_output(pop, col_aggregation, count, error_negative_pop, deaths)

  deaths
}




# ---------------------------------------------------------------------------

# Check the function input is valid
validate_deaths_input <- function(pop, col_aggregation, count, const, error_negative_pop) {

  assert_that(is.data.frame(pop),
              msg = "deaths_null needs a data frame as input")
  assert_that(is.character(col_aggregation),
              msg = "deaths_null needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(count),
              msg = "deaths_null needs a string as the count parameter")
  assert_that(is.numeric(const) && length(const) == 1,
              msg = "deaths_null needs a single numeric value as the const parameter")
  assert_that(const >= 0,
              msg = "deaths_null needs a positive value of the const parameter")
  assert_that(is.logical(error_negative_pop),
              msg = "deaths_null needs a TRUE/FALSE value for the error_negative_pop parameter")
  assert_that(!count %in% col_aggregation,
              msg = "deaths_null was given a population count column that is also a named aggregation column")
  assert_that(!"deaths" %in% col_aggregation,
              msg = "deaths_null can't handle an aggregation column called also deaths. If this is really important to you, update the function to give a customisable name to the output deaths column.")
  if("deaths" %in% names(pop)) {
    warning("deaths is already a column name in the input to deaths_null. The output will contain the calculated deaths instead and this will probably muck up subsequent joins or binds!!")
  }

  if(any(duplicated(col_aggregation))) {
    warning("duplicated column names were provided to deaths_null: these will be removed")
    col_aggregation <- unique(col_aggregation)
  }

  if(requireNamespace("validatepop", quietly=TRUE)) {
    validatepop::validate_population(pop,
                                     col_aggregation = col_aggregation,
                                     col_data = count,
                                     test_complete = TRUE,
                                     test_unique = TRUE,
                                     check_negative_values = TRUE)
  }

  if(nrow(pop) == 0) {
    warning("deaths_null was given an empty input table")
  }

  # If we were working with a mortality dataset as well, we would validate its aggregation levels
  # and check a join is possible with validate_join_population()
}




# Check the function output isn't doing anything unexpected
validate_deaths_output <- function(pop, col_aggregation, count, error_negative_pop, deaths) {

  assert_that(all(col_aggregation %in% names(deaths)))

  assert_that("deaths" %in% names(deaths))

  assert_that(all(complete.cases(deaths)))

  if(requireNamespace("validatepop", quietly = TRUE)) {
    validatepop::validate_join_population(pop,
                                          deaths,
                                          cols_common_aggregation = col_aggregation,
                                          pop1_is_subset = FALSE,
                                          many2one = TRUE,
                                          one2many = FALSE)
  }

}
