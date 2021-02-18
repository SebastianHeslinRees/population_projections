#' Apply rates to a population.
#'
#' Given a cohort population and a data frame of rates at the same or lower
#' resolution, return a data table of the population's aggregation levels, and a
#' component count resulting from the rate applied to the population.
#' 
#' The output dataframe will consist of the columns in the \code{popn} input
#' dataframe, plus any columns specified in \code{additional_rate_cols}, and the
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
#' @param one2many Logical. Setting this to FALSE will check that no more than
#'   one level from \code{popn_rate} matches to each level of \code{popn}. Default
#'   FALSE.
#' @param many2one Logical. Setting this to FALSE will check that no more than
#'   one level from \code{popn} matches to each level of \code{rate}. Default
#'   FALSE.
#' @param additional_popn_cols String. Names of columns in \code{popn}
#'   which should be included in the output but which are not included in
#'   \code{col_aggregation}. Used when multiple aggregation levels apply to the
#'   same rates. Default NULL.
#' @param additional_rate_cols String. Names of columns in \code{popn_rate}
#'   which should be included in the output but which are not included in
#'   \code{col_aggregation}. Used when multiple rates apply to the same
#'   aggregation level, e.g. outmigration to multiple locations. The resulting
#'   left join with \code{popn_rate} can therefore return a much larger data
#'   frame. Default NULL.
#' @param missing_levels_popn Logical. Is the popn data frame missing any
#'   levels? Used to relax validation. Reminder: \code{one2many} will
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
#'                              one2many = FALSE,
#'                              many2one = TRUE,
#'                              additional_popn_cols = NULL,
#                               additional_rate_cols = NULL,
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
                                     one2many = FALSE,
                                     many2one = FALSE,
                                     additional_popn_cols = NULL,
                                     additional_rate_cols = NULL,
                                     missing_levels_popn = FALSE,
                                     missing_levels_rate = FALSE) {

  # Validate input
  # --------------
  validate_apply_rate_to_population_input(popn, popn_rate, col_aggregation, col_popn, col_rate, col_out,
                                          one2many, many2one,
                                          additional_popn_cols, additional_rate_cols,
                                          missing_levels_popn, missing_levels_rate)
  
  
  join_by <- .convert_to_named_vector(col_aggregation)
  
  all_rate_cols <- c(as.character(join_by), additional_rate_cols, col_rate)
  all_popn_cols <- c(names(join_by), additional_popn_cols, col_popn)
  
  col_output_order <- c(setdiff(all_popn_cols, col_popn),
                        additional_rate_cols, col_out) 
  
  popn <- popn %>% 
    select_at(all_popn_cols) %>% 
    rename(popn__ = col_popn)
  
  popn_rate <- popn_rate %>% 
    select_at(all_rate_cols) %>%
    rename(rate__ = col_rate)
  
  # Make sure the columns that are factors match
  popn_rate <- .match_factors(popn, popn_rate, col_aggregation)
  
  # Apply rates
  output <- left_join(popn, popn_rate, by = join_by) %>%
    mutate(component__ = popn__ * rate__) %>%
    rename(!!col_out := component__) %>% 
    select_at(col_output_order)
  
  # Validate output
  one2many <- ifelse(anyDuplicated(data.table::as.data.table(popn[names(join_by)])),
                     TRUE, FALSE) 
  
  # if(anyDuplicated(data.table::as.data.table(popn_rate[popn_rate_join_by]))) {
  #   one2many <- TRUE
  # } else {
  #   one2many = FALSE
  # }
  validate_apply_rate_to_population_output(output,
                                           col_out,
                                           popn,
                                           one2many,
                                           col_output_order,
                                           missing_levels_popn,
                                           missing_levels_rate)
  
  return(output)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_apply_rate_to_population_input <- function(popn, popn_rate, col_aggregation,
                                                    col_popn, col_rate, col_out,
                                                    one2many, many2one,
                                                    additional_popn_cols, additional_rate_cols,
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
  assert_that(rlang::is_bool(one2many),
              msg = "apply_rate_to_population needs a logical value as the one2many parameter")
  assert_that(rlang::is_bool(many2one),
              msg = "apply_rate_to_population needs a logical value as the many2one parameter")
  assert_that(identical(additional_rate_cols, NULL) | is.character(additional_rate_cols),
              msg = "additional_rate_cols should be NA or a character vector")
  assert_that(rlang::is_bool(missing_levels_popn),
              msg = "apply_rate_to_population needs a logical value for missing_levels_popn")
  assert_that(rlang::is_bool(missing_levels_rate),
              msg = "apply_rate_to_population needs a logical value for missing_levels_rate")
  assert_that(col_popn != col_rate,
              msg = "col_popn and col_rate cannot be the same")
  
  # Col_aggreagtion columns are in the input dataframes
  join_by <- .convert_to_named_vector(col_aggregation)
  assertthat::assert_that(all(names(join_by) %in% names(popn)),
                          msg = "in apply_rate_to_population join columns not in popn dataframe")
  
  assertthat::assert_that(all(as.character(join_by) %in% names(popn_rate)),
                          msg = "in apply_rate_to_population join columns not in rates dataframe")
  
  
  #one2many variable is set properly
  if(anyDuplicated(data.table::as.data.table(popn_rate[join_by]))) {
    assertthat::assert_that(one2many==TRUE,
                            msg = "in apply_rate_to_population, one2many set as FALSE but data contains duplicates")
  }
  
  #many2one variable is set properly
  if(anyDuplicated(data.table::as.data.table(popn[names(join_by)]))) {
    assertthat::assert_that(many2one==TRUE,
                            msg = "in apply_rate_to_population, many2one set as FALSE but data contains duplicates")
  }
  
  #Additional columns are in the relevant dataframes
  if(!identical(additional_rate_cols, NA)) {
    assert_that(all(additional_rate_cols %in% names(popn_rate)),
                msg = "in apply_rate_to_population some columns named in additional_rate_cols not in rates dataframe")
  } else {
    additional_rate_cols <- NULL
  }
  
  if(!identical(additional_popn_cols, NA)) {
    assert_that(all(additional_popn_cols %in% names(popn)),
                msg = "in apply_rate_to_population some columns named in additional_popn_cols not in popn dataframe")
  } else {
    additional_popn_cols <- NULL
  }
  
  #output column name doesn't exist elsewhere
  all_rate_cols <- c(as.character(join_by), additional_rate_cols)
  all_popn_cols <- c(names(join_by), additional_popn_cols)
  
  assertthat::assert_that(!col_out %in% all_popn_cols,
                          msg = "in apply_rate_to_population col_out cannot exist in input pop dataframe")
  assertthat::assert_that(!col_out %in% all_rate_cols,
                          msg = "in apply_rate_to_population col_out cannot exist in input rates dataframe")
  
  assert_that(!col_popn %in% names(col_aggregation),
              msg = "apply_rate_to_population was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% as.character(col_aggregation),
              msg = "apply_rate_to_population was given a rate column name that is also a named aggregation column")
  
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
  
  popn_rate_join_by <- as.character(join_by)
  
  if(!identical(additional_rate_cols, NA)) {
    col_aggregation_rate <- unname(c(popn_rate_join_by, additional_rate_cols))
  } else {
    col_aggregation_rate <- unname(popn_rate_join_by)
  }
  
  col_aggregation_popn <- names(join_by)
  
  assert_that(length(join_by) > 0,
              msg = "apply_rate_to_population must share some aggregation column names with the input popn_rate, or a column mapping must be included in the col_aggregation parameter")
  
  
  # if(anyDuplicated(data.table::as.data.table(popn[names(join_by)]))) {
  #   many2one <- TRUE
  # } else {
  #   many2one = FALSE
  # }
  
  validate_population(popn,
                      col_aggregation = col_aggregation_popn,
                      col_data = col_popn,
                      test_complete = !missing_levels_popn,
                      test_unique = !many2one,
                      check_negative_values = TRUE)
  
  # if(anyDuplicated(data.table::as.data.table(popn_rate[as.character(join_by)]))) {
  #   one2many <- TRUE
  # } else {
  #   one2many = FALSE
  # }
  
  validate_population(popn_rate,
                      col_aggregation = col_aggregation_rate,
                      col_data = col_rate,
                      test_complete = !missing_levels_rate,
                      test_unique = !one2many,
                      check_negative_values = FALSE)
  
  if(!missing_levels_rate & !missing_levels_popn) {
    
    aggregation_levels_match <- ifelse(many2one | one2many, FALSE, TRUE)
    validate_join_population(popn,
                             popn_rate,
                             cols_common_aggregation = join_by,
                             aggregation_levels_match =  aggregation_levels_match,
                             many2one = many2one,
                             one2many = one2many,
                             warn_unused_shared_cols = FALSE)
  }
  
  invisible(TRUE)
}

# -----------


# Check the function output isn't doing anything unexpected

validate_apply_rate_to_population_output <- function(output, col_out, popn, one2many, col_output_order,
                                                     missing_levels_popn, missing_levels_rate) {
  
  assert_that(all(col_output_order %in% names(output)))
  
  assert_that(col_out %in% names(output))
  
  if(!missing_levels_rate) {
    assert_that(all(stats::complete.cases(output)))
  }
  
  if(!missing_levels_rate) {
    validate_population(output,
                        col_aggregation = setdiff(col_output_order, col_out),
                        col_data = col_out,
                        test_complete = !missing_levels_popn,
                        test_unique = !one2many,
                        check_negative_values = FALSE,
                        comparison_pop = NA)
  }
  
  invisible(TRUE)
}
