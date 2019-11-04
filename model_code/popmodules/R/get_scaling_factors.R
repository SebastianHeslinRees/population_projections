#' Produce scaling factors to convert one population to another
#'
#' Given a cohort population and a data frame of target marginal population
#' subtotals, return a set of scaling factors which, when applied, will scale
#' the input population to match the target at each grouping level.
#'
#' @param popn A data frame containing population data.
#' @param constraint A data frame containing population data at the same
#'   resolution or lower.
#' @param col_aggregation A string or character vector giving the join mapping between
#'   \code{popn} and \code{constraint}. Equivalent to \code{by} in \code{dplyr} joins
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts.
#' @param col_constraint String. Name of column in \code{constraint} containing population
#'   counts. Defaults to col_popn.
#'
#' @return A data frame of scaling factors calculated as target population/input population,
#'   with one row for each level of the input population for each distinct level of the
#'   input \code{col_aggregation} columns.

get_scaling_factors <- function(popn, constraint, col_aggregation = c("year", "sex", "age", "country"), col_popn,
                                col_constraint = col_popn) {

  # Standardise data
  # ----------------

  # Reformat col_aggregation to a named vector mapping between popn columns and constraint columns
  col_aggregation <- .convert_to_named_vector(col_aggregation)
  # and reorder it to match popn's column ordering
  col_aggregation_popn <- intersect( names(popn), names(col_aggregation) )
  col_aggregation <- col_aggregation[ col_aggregation_popn ]

  join_by <- col_aggregation[col_aggregation %in% names(constraint)]  # this is a named character vector

  # Make sure the columns that are factors match
  constraint <- .match_factors(popn, constraint, col_aggregation)

  # Deal with possible duplicate data column names
  col_popn_new <- paste0(col_popn, ".x")
  col_constraint_new <- paste0(col_constraint, ".y")
  rename(popn, !!sym(col_popn_new) := !!sym(col_popn))
  rename(constraint, !!sym(col_constraint_new) := !!sym(col_constraint))

  #Calculate scaling rates
  scaling <- popn %>%
    left_join(constraint, by = join_by) %>%
    group_by_at(join_by) %>%
    mutate(popn_total = sum(!!sym(col_popn_new)),
           scaling = !!sym(col_constraint_new)/popn_total) %>%
    ungroup() %>%
    data.frame() %>%
    rename(!!col_popn := !!sym(col_popn_new)) %>%
    select(names(popn), scaling)

  return(scaling)
}
