#' Apply mortality rates to a population.
#'
#' Given a cohort population and a data frame of mortality rates that can be
#' joined to the population, return a data table with the population's
#' aggregation levels and a deaths count.
#'
#' If deaths would result in a negative population, an error will be thrown
#' (default) or the deaths will be capped at the population count.
#'
#' @param popn A data frame containing population data
#' @param mortality A data frame containing mortality rates (per aggregation
#'   level per year)
#' @param col_aggregation A string or character vector giving the names of
#'   columns to which the output deaths will be aggregated to. If names differ
#'   between input data frames, use a named character vector, e.g.
#'   \code{c("popn_age"="mortality_age")}. Can contain columns that are not in
#'   \code{mortality}. Default \code{c("year", "gss_code", "age", "sex")}
#' @param col_count String. Name of column in \code{popn} containing population
#'   counts. Default "count"
#' @param col_rate String. Name of column in \code{mortality} containing
#'   mortality rates. Default "rate"
#' @param col_deaths String. Name of column to write deaths to in the output.
#'   Default "deaths"
#'
#' @return A data frame of deaths with one row for each distinct value of the
#'   input \code{col_aggregation} column.
#'
#' @import assertthat
#'
#' @examples
#'
#' library(deaths)
#'
#' popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), count = 100)
#' mortality <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)
#'
#' deaths <- popn_apply_rate(popn,
#'                           mortality,
#'                           col_aggregation = c("year", "gss_code", "sex", "age"),
#'                           col_count = "count",
#'                           col_rate = "rate",
#'                           col_deaths = "deaths")
#'
#' # Due to default parameter values, this is equivalent to
#' count <- popn_apply_rate(popn, mortality)
#'
#' @export
#'

# TODO: make this a more general CoC from rates, popn
# TODO: add a by argument to the function, with default c("gss_code", "sex", "age", "year")

deaths_from_popn_mort <- function(popn,
                                  mortality,
                                  col_aggregation = c("year", "gss_code", "sex", "age"),
                                  col_count = "count",
                                  col_rate = "rate",
                                  col_deaths = "deaths") {

  # Validate input
  # --------------
  validate_deaths_from_popn_mort_input(popn, mortality, col_aggregation, col_count, col_rate, col_deaths)


  # Calculate deaths
  # ----------------
  # This will deal with most checks on the input and output
  deaths <- generalpop::popn_apply_rate(popn,
                                        mortality,
                                        col_aggregation,
                                        col_count,
                                        col_rate,
                                        col_deaths)

  # Validate output
  # ---------------

  # All that's left is to make sure there are no deaths < 0 or which exceed the population
  assert_that(!any(deaths[[col_deaths]] < 0),
              msg = "deaths_from_popn_mort produced negative deaths")
  assert_that(!any(popn[[col_count]] < popn[[col_deaths]]),
              msg = "deaths_from_popn_mort produced more deaths than the population size")

  return(deaths)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_deaths_from_popn_mort_input <- function(popn, mortality, col_aggregation, col_count, col_rate, col_deaths) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "deaths_from_popn_mort needs a data frame as input")
  assert_that(is.data.frame(mortality),
              msg = "deaths_from_popn_mort needs a data frame of mortality data")
  assert_that(is.character(col_aggregation),
              msg = "deaths_from_popn_mort needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_count),
              msg = "deaths_from_popn_mort needs a string as the col_count parameter")
  assert_that(is.string(col_rate),
              msg = "deaths_from_popn_mort needs a string as the col_rate parameter")
  assert_that(is.string(col_deaths),
              msg = "deaths_from_popn_mort needs a string as the col_deaths parameter")

  # Other checks
  col_aggregation <- convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and mortality aggregation levels
  assert_that(!col_count %in% names(col_aggregation),
              msg = "deaths_from_popn_mort was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% col_aggregation,
              msg = "deaths_from_popn_mort was given a mortality rate column name that is also a named aggregation column")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in deaths_from_popn_mort, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to deaths_from_popn_mort")
  assert_that(!any(duplicated(col_aggregation)),
              msg = "duplicated mortality column names were provided to deaths_from_popn_mort")
  assert_that(!col_rate %in% names(col_aggregation),
              msg = "deaths_from_popn_mort can't have a col_rate that is also a named aggregation column in the input")
  assert_that(is.numeric(popn[[col_count]]),
              msg = paste("deaths_from_popn_mort needs a numeric column in the specified population count col:", col_count))
  assert_that(is.numeric(mortality[[col_rate]]),
              msg = paste("deaths_from_popn_mort needs a numeric column in the specified mortality rate col:", col_rate))
  assert_that(all(mortality[[col_rate]] >= 0) && all(mortality[[col_rate]] <= 1),
              msg = "mortality rates in deaths_from_popn_mort must be between 0 and 1")
  assert_that(!col_deaths %in% names(col_aggregation),
              msg = paste("deaths_from_popn_mort can't handle a deaths column with the same name as one of the aggregation columns",
                          "\nDeaths column:", col_deaths,
                          "\nAggregation columns:", col_aggregation))
  if(col_deaths %in% names(popn)) {
    warning(paste("deaths_from_popn_mort is writing deaths to a column name that was also in the input:", col_deaths,
                  "\nThe output will contain the calculated deaths in this column, so be careful with subsequent joins or binds."))
  }

  join_by <- col_aggregation[ col_aggregation %in% names(mortality) ]
  assert_that(length(join_by) > 0,
              msg = "deaths_from_popn_mort must share some aggregation column names with the input mortality, or a column mapping must be included in the col_aggregation parameter")


  # Other checks (by the more expensive validatepop functions) are done within popn_apply_rates

  invisible(TRUE)
}

# -----------



# Function: convert character vector (unnamed or partially named) to one where every element is named
# TODO split this out into the general or helper_functions package. it's used in validate_pop::validate_join_population as well, and will be in births
convert_to_named_vector <- function(vec) {
  assert_that(is.vector(vec))

  if(is.null(names(vec))) {
    names(vec) <- vec
  } else {
    ix <- names(vec) == ""
    names(vec)[ix] <- vec[ix]
  }

  return(vec)
}
