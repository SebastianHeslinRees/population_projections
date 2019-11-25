#' Scale one population to match the totals of another
#'
#' Given a cohort population and a data frame of target marginal population
#' subtotals, return the input population scaled so that its population
#' subtotals match the target's at each grouping level.
#'
#' @param popn A data frame containing population data.
#' @param constraint A data frame containing population data at the same
#'   resolution or lower.
#' @param col_aggregation A string or character vector giving the join mapping between
#'   \code{popn} and \code{constraint}. Equivalent to \code{by} in \code{dplyr} joins
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn".
#' @param col_constraint String. Name of column in \code{constraint} containing population
#'   counts. Default col_popn.
#'   
#' @return A data frame of component counts calculated as input popn * rate, with one row
#'   for each distinct level of the input \code{popn} dataframe
#'
#' @import assertthat
#' @import dplyr
#'
#' @examples
#'
#' popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), popn = 100)
#' constraint <- expand.grid(year=2000, age=20:21, sex=c("f","m"), country = "a", popn = 400)
#'
#' scaled <- popn_constrain(popn,
#'                          constraint,
#'                          col_aggregation = c("year", "sex", "age", "country"),
#'                          col_popn = "popn",
#'                          col_constraint = "popn")
#'
#' Due to default parameter values, this is equivalent to
#' scaled <- popn_constrain(popn, constraint)
#'
#' @export
#'

# TODO add nesting!

popn_constrain <- function(popn,
                           constraint,
                           col_aggregation = c("year", "sex", "age", "country"),
                           col_popn,
                           col_constraint = col_popn){
  
  country <- ifelse("country" %in% col_aggregation, TRUE, FALSE)
  
  validate_popn_constrain_input(popn, constraint, col_aggregation, col_popn, col_constraint, country)
  
  cols <- names(popn)
  
 #if different countries have different scaling
  if(country){
    if(!"country" %in% names(popn)){
      popn <- mutate(popn, country = substr(gss_code,1,1))
    }
    
    do_scale <- filter(popn, country %in% unique(constraint$country))
    dont_scale <- filter(popn, !country %in% unique(constraint$country))
    
  } else {
    
    do_scale <- popn
    dont_scale <- popn[NULL, ]
    
  }
  
  scaling_factors <- get_scaling_factors(do_scale, constraint,
                                         col_aggregation = col_aggregation,
                                         col_popn=col_popn)
  
  scaled_popn <- scaling_factors %>%
    mutate(!!sym(col_popn) := !!sym(col_popn) * scaling) %>%
    select(names(popn)) %>%
    data.frame() %>%
    rbind(dont_scale) %>%
    select(cols)

  return(scaled_popn)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_popn_constrain_input <- function(popn, constraint, col_aggregation, col_popn, col_constraint, country) {
  
  all_constraint_cols <- intersect(unname(col_aggregation), names(constraint))
  
  col_aggregation <- .convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and constraint aggregation levels
  col_popn <- .convert_to_named_vector(col_popn)
  
  assert_that(col_popn %in% names(popn),
              msg="in popn_contraint col_popn must be a column in popn dataframe")
  assert_that(col_constraint %in% names(constraint),
              msg="in popn_contraint col_constraint must be a column in constraint dataframe")
  assert_that(!names(col_popn) %in% names(col_aggregation),
              msg = "popn_constrain was given a population count column name that is also a named aggregation column in the population data frame")
  assert_that(!unname(col_popn) %in% all_constraint_cols,
              msg = "popn_constrain was given a population count column name that is also a named aggregation column in the constraint data frame")
  assert_that(all(col_aggregation %in% names(constraint)),
              msg = "in popn_constrain some columns in col_aggregation not found in constraint")
  if(country){
    assert_that("country" %in% names(constraint),
                msg = "in popn_constrain a country column must be present in the constraint dataframe if country=TRUE")
  }
  assert_that(all(names(col_aggregation)[names(col_aggregation) != "country"] %in% names(popn)),
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
  
  
  
  # Type checking
  assert_that(is.data.frame(popn),
              msg = "popn_constrain needs popn to be a dataframe")
  assert_that(is.data.frame(constraint),
              msg = "popn_constrain needs constraint data to be a dataframe")
  assert_that(is.character(col_aggregation),
              msg = "popn_constrain needs the col_aggregation parameter to be a string")
  assert_that(is.string(col_popn),
              msg = "popn_constrain needs a string as the col_popn parameter")
  assert_that(is.string(col_constraint),
              msg = "popn_constrain needs a string as the col_constraint parameter")
  
  invisible(TRUE)
}
