#' Apply component rates to a population.
#'
#' Given a cohort population and a data frame of component rates that can be
#' joined to the population, return a data table with the population's
#' aggregation levels and a component count.
#'
#' If component would result in a negative population, an error will be thrown
#' (default) or the component will be capped at the population count.
#'
#' @param popn A data frame containing population data
#' @param component_rate A data frame containing component rates (per aggregation
#'   level per year)
#' @param col_aggregation A string or character vector giving the names of
#'   columns to which the output component will be aggregated to. If names differ
#'   between input data frames, use a named character vector, e.g.
#'   \code{c("popn_age"="mortality_age")}. Can contain columns that are not in
#'   \code{component_rate}. Default \code{c("year", "gss_code", "age", "sex")}
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn"
#' @param col_rate String. Name of column in \code{component_rate} containing
#'   component rates. Default "rate"
#' @param col_component String. Name of column to write component to in the output.
#'   Default "component"
#'
#' @return A data frame of the component with one row for each distinct value of the
#'   input \code{col_aggregation} column.
#'
#' @import assertthat
#'
#' @examples
#'
#'
#' popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), popn = 100)
#' mortality <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)
#'
#' deaths <- component_from_popn_rate(popn,
#'                           mortality,
#'                           col_aggregation = c("year", "gss_code", "sex", "age"),
#'                           col_popn = "popn",
#'                           col_rate = "rate",
#'                           col_component = "deaths")
#'
#' # Due to default parameter values, this is equivalent to
#' deaths <- component_from_popn_rate(popn, mortality, col_component = "deaths")
#'
#' @export
#'

# TODO: add a by argument to the function, with default c("gss_code", "sex", "age", "year")

component_from_popn_rate <- function(popn,
                                  component_rate,
                                  col_aggregation = c("year", "gss_code", "sex", "age"),
                                  col_popn = "popn",
                                  col_rate = "rate",
                                  col_component = "component") {

  # Validate input
  # --------------
  validate_component_from_popn_rate_input(popn, component_rate, col_aggregation, col_popn, col_rate, col_component)


  # Calculate component
  # ----------------
  # This will deal with most checks on the input and output
  component <- popn_apply_rate(popn,
                            component_rate,
                            col_aggregation,
                            col_popn,
                            col_rate,
                            col_component)

  # Validate output
  # ---------------

  # All that's left is to make sure there are no components < 0 or which exceed the population
  assert_that(!any(component[[col_component]] < 0),
              msg = "component_from_popn_rate produced negative component counts")
  assert_that(!any(popn[[col_popn]] < component[[col_component]]),
              msg = "component_from_popn_rate produced a higher component than the population size")

  return(component)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_component_from_popn_rate_input <- function(popn, component_rate, col_aggregation, col_popn, col_rate, col_component) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "component_from_popn_rate needs a data frame as input")
  assert_that(is.data.frame(component_rate),
              msg = "component_from_popn_rate needs a data frame of component rate data")
  assert_that(is.character(col_aggregation),
              msg = "component_from_popn_rate needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_popn),
              msg = "component_from_popn_rate needs a string as the col_popn parameter")
  assert_that(is.string(col_rate),
              msg = "component_from_popn_rate needs a string as the col_rate parameter")
  assert_that(is.string(col_component),
              msg = "component_from_popn_rate needs a string as the col_component parameter")

  # Other checks
  col_aggregation <- convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and component_rate aggregation levels
  assert_that(!col_popn %in% names(col_aggregation),
              msg = "component_from_popn_rate was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% col_aggregation,
              msg = "component_from_popn_rate was given a component rate rate column name that is also a named aggregation column")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in component_from_popn_rate, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to component_from_popn_rate")
  assert_that(!any(duplicated(col_aggregation)),
              msg = "duplicated component rate column names were provided to component_from_popn_rate")
  assert_that(!col_rate %in% names(col_aggregation),
              msg = "component_from_popn_rate can't have a col_rate that is also a named aggregation column in the input")
  assert_that(is.numeric(popn[[col_popn]]),
              msg = paste("component_from_popn_rate needs a numeric column in the specified population count col:", col_popn))
  assert_that(is.numeric(component_rate[[col_rate]]),
              msg = paste("component_from_popn_rate needs a numeric column in the specified component rate rate col:", col_rate))
  assert_that(all(component_rate[[col_rate]] >= 0) && all(component_rate[[col_rate]] <= 1),
              msg = "component rate rates in component_from_popn_rate must be between 0 and 1")
  assert_that(!col_component %in% names(col_aggregation),
              msg = paste("component_from_popn_rate can't handle a component column with the same name as one of the aggregation columns",
                          "\ncomponent column:", col_component,
                          "\nAggregation columns:", col_aggregation))
  if(col_component %in% names(popn)) {
    warning(paste("component_from_popn_rate is writing component counts to a column name that was also in the input:", col_component,
                  "\nThe output will contain the calculated component in this column, so be careful with subsequent joins or binds."))
  }

  join_by <- col_aggregation[ col_aggregation %in% names(component_rate) ]
  assert_that(length(join_by) > 0,
              msg = "component_from_popn_rate must share some aggregation column names with the input component rate, or a column mapping must be included in the col_aggregation parameter")


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
