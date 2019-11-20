#' Apply rates to a population.
#'
#' Given a cohort population and a data frame of rates at the same or lower
#' resolution, return a data table of the population's aggregation levels, and a
#' component count resulting from the rate applied to the population.
#'
#' @param popn A data frame containing population data.
#' @param popn_rate A data frame containing rates data per time step (usually
#'   year).
#' @param col_aggregation A string or character vector giving the names of
#'   columns in \code{popn} which the output will be aggregated to. All elements
#'   must give columns in \code{popn} but not all need to be in
#'   \code{popn_rate}, (that is, \code{popn_rate} can be at a lower resolution).
#'   If names differ between the two input data frames, use a named character
#'   vector, e.g. \code{c("gss_code"="LSOA11CD")}. Default \code{c("year",
#'   "gss_code", "age", "sex")}.
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn".
#' @param col_rate String. Name of column in \code{popn_rate} containing rate
#'   data. Default "rate".
#' @param col_out String. Name of column for the output component count
#'   (popn*rate) in the output. Default "component".
#' @param pop1_is_subset Logical. If the two input data frames cover the same
#'   domain and you expect every level of \code{popn_rate} to be matched to by a
#'   level in \code{popn} set this to TRUE, and this will be checked. Default
#'   FALSE.
#' @param many2one Logical. Setting this to FALSE will check that no more than
#'   one level from \code{popn} matches to each level of \code{rate}. Default
#'   FALSE.
#' @param additional_rate_levels String. Names of columns in \code{popn_rate}
#'   which should be included in the join beyond those named in
#'   \code{col_aggregation}. Used when multiple rates apply to the same
#'   aggregation level, e.g. outmigration to multiple locations. The resulting
#'   left join with \code{popn_rate} can therefore return a much larger data
#'   frame. Default NA.
#' @param missing_levels_popn Logical. Is the popn data frame missing any
#'   levels? Reminder: \code{pop1_is_subset} will probably be TRUE in this case.
#'   Default FALSE.
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
#' library(generalpop)
#'
#' popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), popn = 100)
#' rate <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)
#'
#' component <- popn_apply_rate(popn,
#'                              rate,
#'                              col_aggregation = c("year", "gss_code", "sex", "age"),
#'                              col_popn = "popn",
#'                              col_rate = "rate",
#'                              col_out = "component",
#'                              pop1_is_subset = FALSE,
#'                              many2one = TRUE,
#                               additional_rate_levels = NA,
#'                              missing_levels_popn = FALSE,
#'                              missing_levels_rate = FALSE)
#'
#' # Due to default parameter values, this is equivalent to
#' component <- popn_apply_rate(popn, rate)
#'
#' @export
#'

# TODO simplify the function - the validation steps are making the inputs complicated.

# TODO Would it be useful to add an option to return the output as the input +
# an extra column, rather than the current version which strips down to
# aggregation levels + an extra column? Of course you can just use
#     left_join(input, popn_apply_rate(input, rate))
#  but for frequent operations on large datasets, it could be sped up if we
# just don't subset inside this function.


popn_apply_rate <- function(popn,
                            popn_rate,
                            col_aggregation = c("year", "gss_code", "sex", "age"),
                            col_popn = "popn",
                            col_rate = "rate",
                            col_out = "component",
                            pop1_is_subset = FALSE,
                            many2one = TRUE,
                            additional_rate_levels = NA,
                            missing_levels_popn = FALSE,
                            missing_levels_rate = FALSE) {

  # Validate input
  # --------------
  validate_popn_apply_rate_input(popn, popn_rate, col_aggregation, col_popn, col_rate, col_out,
                                 pop1_is_subset, many2one, additional_rate_levels,
                                 missing_levels_popn, missing_levels_rate)


  # Standardise data
  # ----------------

  # Reformat col_aggregation to a named vector mapping between popn columns and popn_rate columns
  col_aggregation <- .convert_to_named_vector(col_aggregation)
  # and reorder it to match popn's column ordering
  popn_cols_to_aggregate <- intersect( names(popn), names(col_aggregation) )
  col_aggregation <- col_aggregation[ popn_cols_to_aggregate ]

  join_by <- col_aggregation[col_aggregation %in% names(popn_rate)]  # this is a named character vector

  # if we want to include extra columns from the rate in the output, check if
  # these are a one2many matching in the join
  if(identical(additional_rate_levels, NA)) {
    additional_rate_levels <- NULL
  }

  all_rate_cols <- c(as.character(col_aggregation), additional_rate_levels)
  all_rate_cols <- intersect(all_rate_cols, names(popn_rate))

  if(anyDuplicated(data.table::as.data.table(popn_rate[join_by]))) {
    one2many <- TRUE
  } else {
    one2many = FALSE
  }

  # Trim inputs to the columns we care about (reduces the chances of column name conflicts)
  popn_cols <- names(col_aggregation)
  popn <- popn[c(popn_cols, col_popn)]
  popn_rate <- popn_rate[c(all_rate_cols, col_rate)]

  # Make sure the columns that are factors match
  popn_rate <- .match_factors(popn, popn_rate, col_aggregation)

  # Deal with the possibility of duplicate data column names
  if(col_popn == col_rate) {
    col_popn <- paste0(col_popn, ".x")
    col_rate  <- paste0(col_rate,  ".y")
  }

  # Apply rates
  # ----------------
  output <- left_join(popn, popn_rate, by = join_by) %>%
    mutate(!!sym(col_out) := !!sym(col_popn) * !!sym(col_rate) ) %>%
    select(!!!syms(popn_cols), additional_rate_levels, !!sym(col_out))

  # Validate output
  # ---------------
  validate_popn_apply_rate_output(popn,
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
validate_popn_apply_rate_input <- function(popn, popn_rate, col_aggregation, col_popn, col_rate, col_out,
                                           pop1_is_subset, many2one, additional_rate_levels,
                                           missing_levels_popn, missing_levels_rate) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "popn_apply_rate needs a data frame as input")
  assert_that(is.data.frame(popn_rate),
              msg = "popn_apply_rate needs a data frame of popn_rate data")
  assert_that(is.character(col_aggregation),
              msg = "popn_apply_rate needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_popn),
              msg = "popn_apply_rate needs a string as the col_popn parameter")
  assert_that(is.string(col_rate),
              msg = "popn_apply_rate needs a string as the col_rate parameter")
  assert_that(is.string(col_out),
              msg = "popn_apply_rate needs a string as the col_out parameter")
  assert_that(is.string(col_out),
              msg = "popn_apply_rate needs a string as the col_out parameter")
  assert_that(rlang::is_bool(pop1_is_subset),
              msg = "popn_apply_rate needs a logical value as the pop1_is_subset parameter")
  assert_that(rlang::is_bool(many2one),
              msg = "popn_apply_rate needs a logical value as the many2one parameter")
  assert_that(identical(additional_rate_levels, NA) | is.character(additional_rate_levels),
              msg = "additional_rate_levels should be NA or a character vector")
  assert_that(rlang::is_bool(missing_levels_popn),
              msg = "popn_apply_rate needs a logical value for missing_levels_popn")
  assert_that(rlang::is_bool(missing_levels_rate),
              msg = "popn_apply_rate needs a logical value for missing_levels_rate")


  # Other checks
  if(!identical(additional_rate_levels, NA)) {
    assert_that(all(additional_rate_levels %in% names(popn_rate)))
  } else {
    additional_rate_levels <- NULL
  }
  all_rate_cols <- c(as.character(col_aggregation), additional_rate_levels)
  all_rate_cols <- intersect(all_rate_cols, names(popn_rate))


  col_aggregation <- .convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and popn_rate aggregation levels

  assert_that(!col_popn %in% names(col_aggregation),
              msg = "popn_apply_rate was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% all_rate_cols,
              msg = "popn_apply_rate was given a rate column name that is also a named aggregation column")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in popn_apply_rate, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to popn_apply_rate")
  assert_that(!any(duplicated(as.character(col_aggregation))),
              msg = "duplicated popn_rate column names were provided to popn_apply_rate")
  assert_that(any(col_aggregation %in% names(popn_rate)),
              msg = "in popn_apply_rate, no aggregation column names were found in popn_rate - at least one must be present")
  assert_that(!col_rate %in% names(col_aggregation),
              msg = "popn_apply_rate can't have a col_rate that is also a named aggregation column in the input")
  assert_that(is.numeric(popn[[col_popn]]),
              msg = paste("popn_apply_rate needs a numeric column in the specified population count col:", col_popn))
  assert_that(is.numeric(popn_rate[[col_rate]]),
              msg = paste("popn_apply_rate needs a numeric column in the specified popn_rate rate col:", col_rate))
  assert_that(!col_out %in% names(col_aggregation),
              msg = paste("popn_apply_rate can't handle an output count column with the same name as one of the aggregation columns",
                          "\nOutput column:", col_out,
                          "\nAggregation columns:", col_aggregation))


  if(any(popn_rate[[col_rate]] < 0)) {
    warning("popn_apply_rate was passed negative rates")
  }
  if(col_out %in% names(popn)) {
    warning(paste("popn_apply_rate is writing output count to a column name that was also in the input:", col_out,
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
              msg = "popn_apply_rate must share some aggregation column names with the input popn_rate, or a column mapping must be included in the col_aggregation parameter")

  validate_population(popn,
                      col_aggregation = names(col_aggregation),
                      col_data = col_popn,
                      test_complete = !missing_levels_popn,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  validate_population(popn_rate,
                      col_aggregation = unname(col_aggregation_rate),
                      col_data = col_rate,
                      test_complete = !missing_levels_rate,
                      test_unique = FALSE,
                      check_negative_values = FALSE)
  if(!missing_levels_rate) {
    validate_join_population(popn,
                             popn_rate,
                             cols_common_aggregation = join_by,
                             pop1_is_subset = pop1_is_subset,
                             many2one = many2one,
                             one2many = one2many,
                             warn_unused_shared_cols = FALSE)
  }

  invisible(TRUE)
}

# -----------


# Check the function output isn't doing anything unexpected

validate_popn_apply_rate_output <- function(popn, col_aggregation, col_out, output, one2many, additional_rate_levels, missing_levels_popn, missing_levels_rate) {

  col_aggregation <- .convert_to_named_vector(col_aggregation)

  output_col_aggregation <- unique(c(names(col_aggregation), additional_rate_levels))
  assert_that(all(output_col_aggregation %in% names(output)))

  assert_that(col_out %in% names(output))

  if(!missing_levels_rate) {
    assert_that(all(stats::complete.cases(output)))
  }

  if(one2many | missing_levels_popn) {
    output_comparison <- NA # TODO: could we use semi_join(popn_rate, popn, by=...) here?
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
