#' Produce scaling factors to convert one population to another
#'
#' Given a cohort population and a data frame of target marginal population
#' subtotals, return a set of scaling factors which, when applied, will scale
#' the input population to match the target at each grouping level.
#'
#' @param popn A data frame containing population data.
#' @param constraint A data frame containing population data at the same
#'   resolution or lower.
#' @param col_aggregation A string or character vector giving the names of
#'   columns in \code{popn} contain aggregation levels. All elements
#'   must give columns in \code{popn} but not all need to be in
#'   \code{constraint}, (that is, \code{constraint} can be at a lower resolution).
#'   If names differ between the two input data frames, use a named character
#'   vector, e.g. \code{c("gss_code"="LSOA11CD")}. Default \code{c("year",
#'   "gss_code", "age", "sex")}.
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts.
#'
#' @return A data frame of scaling factors calculated as
#'   target population/input population, with one row
#'   for each distinct level of the input \code{col_aggregation} columns.

get_scaling_factors <- function(popn, constraint, col_aggregation = c("year", "sex", "age"), col_popn) {
  
  # Standardise data
  # ----------------
  
  # Reformat col_aggregation to a named vector mapping between popn columns and constraint columns
  col_aggregation <- .convert_to_named_vector(col_aggregation)
  # and reorder it to match popn's column ordering
  col_aggregation_popn <- intersect( names(popn), names(col_aggregation) )
  col_aggregation <- col_aggregation[ col_aggregation_popn ]
  
  join_by <- col_aggregation[col_aggregation %in% names(constraint)]  # this is a named character vector
  
  # TODO split these out into two variables rather than a named vector
  col_popn <- .convert_to_named_vector(col_popn)
  
  one2many = FALSE
  
  # Trim inputs to the columns we care about (reduces the chances of column name conflicts)
  popn_cols <- names(col_aggregation)
  popn <- popn[c(popn_cols, names(col_popn))]
  constraint <- constraint[c(join_by, col_popn)]
  
  # Make sure the columns that are factors match
  constraint <- .match_factors(popn, constraint, col_aggregation)
  
  # Deal with possible duplicate data column names
  col_popn_x <- names(col_popn)
  col_popn_y <- unname(col_popn)
  if(col_popn_x == col_popn_y) {
    col_popn_x_new <- paste0(col_popn_x, ".x")
    col_popn_y_new <- paste0(col_popn_y, ".y")
    
    rename(popn, !!sym(col_popn_x_new) := !!sym(col_popn_x))
    rename(constraint, !!sym(col_popn_y_new) := !!sym(col_popn_y))
    
    col_popn_x <- col_popn_x_new
    col_popn_y <- col_popn_y_new
  }
  
  #Calculate scaling rates
  
  scaling <- popn %>%
    left_join(constraint, by = join_by) %>%
    group_by_at(join_by) %>%
    add_tally() %>%
    ungroup() %>%
    data.frame() %>%
    mutate(popn_total = sum(!!sym(col_popn_x)),
           scaling = !!sym(col_popn_y)/popn_total) %>%
    select(c(!!col_aggregation, scaling))
  
  return(scaling)
}