#' Apply rates to a population.
#'
#' Given a cohort population and a data frame of rates at the same or lower
#' resolution, return a data table of the population's aggregation levels, and a
#' component count resulting from the rate applied to the population.
#' 
#' The output dataframe will consist of the columns in the \code{popn} input
#' dataframe, plus any columns specified in \code{additional_rate_levels}, and the
#' output data column \code{col_out}. Output has 1 row for each distinct level of
#' the \code{col_aggregation}.
#'
#' @param popn A data frame containing population data
#' @param popn_rate A data frame containing rates data
#' @param col_aggregation A string or character vector giving the names of
#'   columns on which to join (\code{by} in \code{dplyr::left_join}).
#'   If names differ between the two input data frames, use a named character
#'   vector, e.g. \code{c("gss_code"="LSOA11CD")}. Default \code{c("year",
#'   "gss_code", "age", "sex")}.
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn".
#' @param col_rate String. Name of column in \code{popn_rate} containing rate
#'   data. Default "rate".
#' @param col_out String. Name of the resulting component count column (popn*rate)
#'   in the output dataframe. Default "component".
#' @param aggregation_levels_match Logical. If the two input data frames cover
#'   the same domain and you expect every level of \code{popn_rate} to be matched
#'   to by a level in \code{popn} set this to TRUE, and this will be checked.
#'   Default FALSE.
#' @param many2one Logical. Setting this to FALSE will check that no more than
#'   one level from \code{popn} matches to each level of \code{rate}. Default
#'   FALSE.
#' @param additional_rate_levels String. Names of columns in \code{popn_rate}
#'   which should be included in the output but which are not included in
#'   \code{col_aggregation}. Used when multiple rates apply to the same
#'   aggregation level, e.g. outmigration to multiple locations. The resulting
#'   left join with \code{popn_rate} can therefore return a much larger data
#'   frame. Default NULL.
#' @param missing_levels_popn Logical. Is the popn data frame missing any
#'   levels? Used to relax validation. Reminder: \code{aggregation_levels_match} will
#'   probably be TRUE in this case. Default FALSE.
#' @param missing_levels_rate Logical or character vector. Is the rates data
#'   missing any levels? If joining the missing levels to the input population
#'   would create NAs, the missing values in the output column will be NA. Note:
#'   setting this to TRUE will disable some of the checks on the input and join
#'   datasets. Default FALSE.
#'
#' @return A data frame of component counts calculated as input popn * rate, with one row
#'   for each distinct level of the input \code{col_aggregation} columns.
#'
#' @import assertthat
#' @import dplyr
#'
#' @examples
#'
#' popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), popn = 100)
#' rate <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)
#'
#' component <- apply_rate_to_population(popn,
#'                              rate,
#'                              col_aggregation = c("year", "gss_code", "sex", "age"),
#'                              col_popn = "popn",
#'                              col_rate = "rate",
#'                              col_out = "component",
#'                              aggregation_levels_match = FALSE,
#'                              many2one = TRUE,
#                               additional_rate_levels = NULL,
#'                              missing_levels_popn = FALSE,
#'                              missing_levels_rate = FALSE)
#'
#' # Due to default parameter values, this is equivalent to
#' component <- apply_rate_to_population(popn, rate)
#'
#' @export
#'

apply_rate_to_population <- function(popn,
                            popn_rate,
                            col_aggregation = c("year", "gss_code", "sex", "age"),
                            col_popn = "popn",
                            col_rate = "rate",
                            col_out = "component",
                            aggregation_levels_match = FALSE,
                            many2one = TRUE,
                            additional_rate_levels = NULL,
                            missing_levels_popn = FALSE,
                            missing_levels_rate = FALSE) {

  # Validate input
  # --------------
  validate_apply_rate_to_population_input(popn, popn_rate, col_aggregation, col_popn, col_rate, col_out,
                                 aggregation_levels_match, many2one, additional_rate_levels,
                                 missing_levels_popn, missing_levels_rate)


  join_by <- .convert_to_named_vector(col_aggregation)

  if(anyDuplicated(data.table::as.data.table(popn_rate[join_by]))) {
    one2many <- TRUE
  } else {
    one2many = FALSE
  }

  all_rate_cols <- c(as.character(join_by), additional_rate_levels, col_rate)
  
  col_output_order <- c(setdiff(names(popn), col_popn),
                        additional_rate_levels, col_out)
  
  popn <- popn %>% rename(popn__ = col_popn)
  popn_rate <- select_at(popn_rate, all_rate_cols) %>%
    rename(rate__ = col_rate)
  
  # Make sure the columns that are factors match
  popn_rate <- .match_factors(popn, popn_rate, col_aggregation)

  # Apply rates
  output <- left_join(popn, popn_rate, by = join_by) %>%
    mutate(component__ = popn__ * rate__) %>%
    rename(!!col_out := component__) %>% 
    select_at(col_output_order)

  # Validate output
  validate_apply_rate_to_population_output(popn,
                                  col_aggregation,
                                  col_out,
                                  output,
                                  one2many,
                                  additional_rate_levels,
                                  missing_levels_popn,
                                  missing_levels_rate)

  return(output)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_apply_rate_to_population_input <- function(popn, popn_rate, col_aggregation, col_popn, col_rate, col_out,
                                           aggregation_levels_match, many2one, additional_rate_levels,
                                           missing_levels_popn, missing_levels_rate) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "apply_rate_to_population needs a data frame as input")
  assert_that(is.data.frame(popn_rate),
              msg = "apply_rate_to_population needs a data frame of popn_rate data")
  assert_that(is.character(col_aggregation),
              msg = "apply_rate_to_population needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_popn),
              msg = "apply_rate_to_population needs a string as the col_popn parameter")
  assert_that(is.string(col_rate),
              msg = "apply_rate_to_population needs a string as the col_rate parameter")
  assert_that(is.string(col_out),
              msg = "apply_rate_to_population needs a string as the col_out parameter")
  assert_that(is.string(col_out),
              msg = "apply_rate_to_population needs a string as the col_out parameter")
  assert_that(rlang::is_bool(aggregation_levels_match),
              msg = "apply_rate_to_population needs a logical value as the aggregation_levels_match parameter")
  assert_that(rlang::is_bool(many2one),
              msg = "apply_rate_to_population needs a logical value as the many2one parameter")
  assert_that(identical(additional_rate_levels, NULL) | is.character(additional_rate_levels),
              msg = "additional_rate_levels should be NA or a character vector")
  assert_that(rlang::is_bool(missing_levels_popn),
              msg = "apply_rate_to_population needs a logical value for missing_levels_popn")
  assert_that(rlang::is_bool(missing_levels_rate),
              msg = "apply_rate_to_population needs a logical value for missing_levels_rate")
assert_that(col_popn != col_rate,
              msg = "col_popn and col_rate cannot be the same")
  
  # Other checks
  join_by <- .convert_to_named_vector(col_aggregation)
  assertthat::assert_that(all(names(join_by) %in% names(popn)),
                          msg = "in apply_rate_to_population join columns not in popn dataframe")
  
  assertthat::assert_that(all(as.character(join_by) %in% names(popn_rate)),
                          msg = "in apply_rate_to_population join columns not in rates dataframe")
  
 
  if(!identical(additional_rate_levels, NA)) {
    assert_that(all(additional_rate_levels %in% names(popn_rate)))
  } else {
    additional_rate_levels <- NULL
  }
  all_rate_cols <- c(as.character(col_aggregation), additional_rate_levels)
  all_rate_cols <- intersect(all_rate_cols, names(popn_rate))

  assertthat::assert_that(!col_out %in% names(popn),
                          msg = "in apply_rate_to_population col_out cannot exist in input pop dataframe")
  assertthat::assert_that(!col_out %in% all_rate_cols,
                          msg = "in apply_rate_to_population col_out cannot exist in input rates dataframe")
  
  col_aggregation <- .convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and popn_rate aggregation levels

  assert_that(!col_popn %in% names(col_aggregation),
              msg = "apply_rate_to_population was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% all_rate_cols,
              msg = "apply_rate_to_population was given a rate column name that is also a named aggregation column")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in apply_rate_to_population, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to apply_rate_to_population")
  assert_that(!any(duplicated(as.character(col_aggregation))),
              msg = "duplicated popn_rate column names were provided to apply_rate_to_population")
  assert_that(any(col_aggregation %in% names(popn_rate)),
              msg = "in apply_rate_to_population, no aggregation column names were found in popn_rate - at least one must be present")
  assert_that(!col_rate %in% names(col_aggregation),
              msg = "apply_rate_to_population can't have a col_rate that is also a named aggregation column in the input")
  assert_that(is.numeric(popn[[col_popn]]),
              msg = paste("apply_rate_to_population needs a numeric column in the specified population count col:", col_popn))
  assert_that(is.numeric(popn_rate[[col_rate]]),
              msg = paste("apply_rate_to_population needs a numeric column in the specified popn_rate rate col:", col_rate))
  assert_that(!col_out %in% names(col_aggregation),
              msg = paste("apply_rate_to_population can't handle an output count column with the same name as one of the aggregation columns",
                          "\nOutput column:", col_out,
                          "\nAggregation columns:", col_aggregation))


  if(any(popn_rate[[col_rate]] < 0)) {
    warning("apply_rate_to_population was passed negative rates")
  }
  if(col_out %in% names(popn)) {
    warning(paste("apply_rate_to_population is writing output count to a column name that was also in the input:", col_out,
                  "\nThe output will contain the output in this column, so be careful with subsequent joins or binds."))
  }

  join_by <- col_aggregation[ col_aggregation %in% names(popn_rate) ]
  if(anyDuplicated(data.table::as.data.table(popn_rate[join_by]))) {
    one2many <- TRUE
  } else {
    one2many = FALSE
  }

  if(!identical(additional_rate_levels, NA)) {
    col_aggregation_rate <- c(join_by, additional_rate_levels)
  } else {
    col_aggregation_rate <- join_by
  }

  assert_that(length(join_by) > 0,
              msg = "apply_rate_to_population must share some aggregation column names with the input popn_rate, or a column mapping must be included in the col_aggregation parameter")

  validate_population(popn,
                      col_aggregation = setdiff(names(popn), col_popn),
                      col_data = col_popn,
                      test_complete = !missing_levels_popn,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  validate_population(popn_rate,
                      col_aggregation = setdiff(names(popn_rate), col_rate),
                      col_data = col_rate,
                      test_complete = !missing_levels_rate,
                      test_unique = FALSE,
                      check_negative_values = FALSE)
  if(!missing_levels_rate) {
    validate_join_population(popn,
                             popn_rate,
                             cols_common_aggregation = join_by,
                             aggregation_levels_match = aggregation_levels_match,
                             many2one = many2one,
                             one2many = one2many,
                             warn_unused_shared_cols = FALSE)
  }

  invisible(TRUE)
}

# -----------


# Check the function output isn't doing anything unexpected

validate_apply_rate_to_population_output <- function(popn, col_aggregation, col_out, output, one2many, additional_rate_levels, missing_levels_popn, missing_levels_rate) {
  
  col_aggregation <- .convert_to_named_vector(col_aggregation)

  output_col_aggregation <- unique(c(setdiff(names(popn), "popn__"), additional_rate_levels))
  assert_that(all(output_col_aggregation %in% names(output)))

  assert_that(col_out %in% names(output))

  if(!missing_levels_rate) {
    assert_that(all(stats::complete.cases(output)))
  }

  if(one2many | missing_levels_popn) {
    output_comparison <- NA
  } else {
    output_comparison <- popn
  }

  if(!missing_levels_rate) {
    validate_population(output,
                        col_aggregation = output_col_aggregation,
                        col_data = col_out,
                        test_complete = !missing_levels_popn,
                        test_unique = TRUE,
                        check_negative_values = FALSE,
                        comparison_pop = output_comparison)
  }

  invisible(TRUE)
}
