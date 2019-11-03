#' Scale one population to match the totals of another
#'
#' Given a cohort population and a data frame of target marginal population
#' subtotals, return the input population scaled so that its population
#' subtotals match the target's at each grouping level.
#' Scaling is only perfomed within England, i.e. on entries in the
#' \code{gss_code} column starting with \code{E}
#' @param popn A data frame containing population data.
#' @param constraint A data frame containing population data at the same
#'   resolution or lower.
#' @param col_aggregation A string or character vector giving the join mapping between
#'   \code{popn} and \code{constraint}. Equivalent to \code{by} in \code{dplyr} joins
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn".
#' @param col_constraint String. Name of column in \code{constraint} containing population
#'   counts. Default col_popn.
#' @param pop1_is_subset Logical. If the two input data frames cover the same
#'   domain and you expect every level of \code{constraint} to be matched to by a
#'   level in \code{popn} set this to TRUE, and this will be checked. Default
#'   FALSE.
#' @param missing_levels_popn Logical. Is the popn data frame missing any
#'   levels? Reminder: \code{pop1_is_subset} will probably be TRUE in this case.
#'   Default FALSE.
#' @param missing_levels_constraint Logical or character vector. Is the rates data
#'   missing any levels? If joining the missing levels to the input population
#'   would create NAs, the missing values in the output column will be NA. Note:
#'   setting this to TRUE will disable some of the checks on the input and join
#'   datasets. Default FALSE.
#'
#' @return A data frame of component counts calculated as input popn * rate, with one row
#'   for each distinct level of the input \code{popn} dataframe
#'
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate select_at group_by_at rename add_tally sym syms
#'
#' @examples
#'
#' library(popmodules)
#'
#' popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), popn = 100)
#' constraint <- expand.grid(year=2000, age=20:21, sex=c("f","m"), popn = 400)
#'
#' scaled <- popn_constrain(popn,
#'                          constraint,
#'                          col_aggregation = c("year", "gss_code", "sex", "age"),
#'                          col_popn = "popn",
#'                          pop1_is_subset = FALSE,
#'                          missing_levels_popn = FALSE,
#'                          missing_levels_constraint = FALSE)
#'
#' # Due to default parameter values, this is equivalent to
#' scaled <- popn_constrain(popn, constraint)
#'
#' constraint <- expand.grid(xyear=2000, xage=20:21, xsex=c("f","m"), xpopn = 400)
#'
#' scaled <- popn_constrain(popn,
#'                          constraint,
#'                          col_aggregation = c("year"="xyear", "gss_code", "sex"="xsex", "age"="xage"),
#'                          col_popn = c("popn"="xpopn"),
#'                          pop1_is_subset = FALSE,
#'                          missing_levels_popn = FALSE,
#'                          missing_levels_constraint = FALSE)
#'
#'
#' @export
#'

# TODO simplify the function - the validation steps are making the inputs complicated.
# TODO expand the function so that the constraint parameter can take a single
# population number to scale to

# TODO add nesting!

popn_constrain <- function(popn,
                           constraint,
                           col_aggregation = c("year", "sex", "age"),
                           col_popn,
                           col_constraint = col_popn,
                           pop1_is_subset = FALSE,
                           missing_levels_popn = FALSE,
                           missing_levels_constraint = FALSE) {

  # Validate input
  # --------------
  #validate_popn_constrain_input(popn, constraint, col_aggregation, col_popn,
  #                              pop1_is_subset, missing_levels_popn, missing_levels_constraint)

  popn <- mutate(popn, country = substr(gss_code,1,1))  
  do_scale <- filter(popn, country %in% unique(constraint$country))
  dont_scale <- filter(popn, !country %in% unique(constraint$country))

  #constraint_cols <- names(constraint)[names(constraint)!=col_popn]

  scaling_factors <- get_scaling_factors(do_scale, constraint,
                                         col_aggregation = col_aggregation, col_popn=col_popn)

  scaled_popn <- scaling_factors %>%
    mutate(!!sym(col_popn) := !!sym(col_popn) * scaling) %>%
    select(names(popn)) %>%
    data.frame() %>%
    rbind(dont_scale)

  # Validate output
  # ---------------
  # TODO: Should this be taking the output dataframe?
  # validate_popn_constrain_output(popn,
  #                                col_aggregation,
  #                                col_popn,
  #                                output,
  #                                missing_levels_popn,
  #                                missing_levels_constraint)

  return(scaled_popn)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_popn_constrain_input <- function(popn, constraint, col_aggregation, col_popn,
                                          pop1_is_subset, missing_levels_popn, missing_levels_constraint) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "popn_constrain needs a data frame as input")
  assert_that(is.data.frame(constraint),
              msg = "popn_constrain needs a data frame of constraint data")
  assert_that(is.character(col_aggregation),
              msg = "popn_constrain needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_popn),
              msg = "popn_constrain needs a string as the col_popn parameter")
  assert_that(rlang::is_bool(pop1_is_subset),
              msg = "popn_constrain needs a logical value as the pop1_is_subset parameter")
  assert_that(rlang::is_bool(missing_levels_popn),
              msg = "popn_constrain needs a logical value for missing_levels_popn")
  assert_that(rlang::is_bool(missing_levels_constraint),
              msg = "popn_constrain needs a logical value for missing_levels_constraint")


  # Other checks
  all_constraint_cols <- intersect(unname(col_aggregation), names(constraint))


  col_aggregation <- .convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and constraint aggregation levels
  col_popn <- .convert_to_named_vector(col_popn)

  assert_that(!names(col_popn) %in% names(col_aggregation),
              msg = "popn_constrain was given a population count column name that is also a named aggregation column in the population data frame")
  assert_that(!unname(col_popn) %in% all_constraint_cols,
              msg = "popn_constrain was given a population count column name that is also a named aggregation column in the constraint data frame")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in popn_constrain, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to popn_constrain")
  assert_that(!any(duplicated(unname(col_aggregation))),
              msg = "duplicated constraint column names were provided to popn_constrain")
  assert_that(any(col_aggregation %in% names(constraint)),
              msg = "in popn_constrain, no aggregation column names were found in constraint - at least one must be present")
  assert_that(is.numeric(popn[[names(col_popn)]]),
              msg = paste("popn_constrain needs a numeric column in the specified population count col:", names(col_popn)))
  assert_that(is.numeric(constraint[[col_popn]]),
              msg = paste("popn_constrain needs a numeric column in the specified constraint constraint col:", unname(col_popn)))

  join_by <- col_aggregation[ col_aggregation %in% names(constraint) ]
  if(anyDuplicated(data.table::as.data.table(constraint[join_by]))) {
    stop("popn_constrain was given a population that maps to multiple levels in the constraint population")
  }

  assert_that(length(join_by) > 0,
              msg = "popn_constrain must share some aggregation column names with the input constraint, or a column mapping must be included in the col_aggregation parameter")

  validate_population(popn,
                      col_aggregation = names(col_aggregation),
                      col_data = names(col_popn),
                      test_complete = !missing_levels_popn,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  validate_population(constraint,
                      col_aggregation = unname(join_by),
                      col_data = unname(col_popn),
                      test_complete = !missing_levels_constraint,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

  #TODO: Why does this fail with actual data
  # if(!missing_levels_constraint) {
  #   validate_join_population(popn,
  #                            constraint,
  #                            cols_common_aggregation = join_by,
  #                            pop1_is_subset = pop1_is_subset,
  #                            many2one = TRUE,
  #                            one2many = FALSE,
  #                            warn_unused_shared_cols = FALSE)
  # }

  invisible(TRUE)
}

# -----------


# Check the function output isn't doing anything unexpected

validate_popn_constrain_output <- function(popn, col_aggregation, col_popn, output, missing_levels_popn, missing_levels_constraint) {

  col_aggregation <- .convert_to_named_vector(col_aggregation)

  assert_that(all(names(col_aggregation) %in% names(output)))
  assert_that(names(col_popn) %in% names(output))

  if(!missing_levels_constraint) {
    assert_that(all(stats::complete.cases(output)))
  }

  if(missing_levels_popn) {
    output_comparison <- NA # TODO: could we use semi_join(constraint, popn, by=...) here?
  } else {
    output_comparison <- popn
  }

  if(!missing_levels_constraint) {
    validate_population(output,
                        col_aggregation = names(col_aggregation),
                        col_data = names(col_popn),
                        test_complete = !missing_levels_popn,
                        test_unique = TRUE,
                        check_negative_values = TRUE,
                        comparison_pop = output_comparison)
  }

  invisible(TRUE)
}
