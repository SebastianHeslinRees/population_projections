#' Template births function that returns a fixed number of births (default zero)
#' for each aggregation level, and sets their age to zero
#'
#' This is designed to be used as a placeholder births function during model
#' testing and a template for other births functions, along with its tests.
#'
#' @param pop A data frame containing population data
#' @param col_aggregation A string giving the names of columns to which the
#'   output births will be aggregated to. Default \code{c("gss_code","sex")}
#' @param const Numeric. Number of births to return per geography. Defaults to
#'   zero, but can be set to any positive number
#'
#' @return A data frame of births with one row for each distinct value of the
#'   \code{col_aggregation} columns, a column named births with value
#'   \code{const} and a column named age with value 0.
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'
#' @export
#'
births_null <- function(pop, col_aggregation = c("gss_code", "sex"), const = 0) {

  validate_births_input(pop, col_aggregation, const)
  col_aggregation <- col_aggregation[ col_aggregation != "age"] # validate_births_input warns if this is necessary

  births <- dplyr::group_by_at(pop, .vars = col_aggregation) %>%
    dplyr::summarise(births = const, age = 0) %>%
    dplyr::ungroup()

  validate_births_output(pop, col_aggregation, births)

  births
}




# ---------------------------------------------------------------------------

# Check the function input is valid
validate_births_input <- function(pop, col_aggregation, const) {

  assert_that(is.data.frame(pop),
              msg = "births_null needs a data frame as input")
  assert_that(is.character(col_aggregation),
              msg = "births_null needs a character vector as the col_aggregation parameter")
  assert_that(is.numeric(const) && length(const) == 1,
              msg = "births_null needs a single numeric value as the const parameter")
  assert_that(const >= 0,
              msg = "births_null needs a positive value of the const parameter")
  assert_that(all(col_aggregation %in% names(pop)),
              msg = paste(c("births_null was given column name(s) not present in the input population data",
                            "\nColumn names provided:",col_aggregation,
                            "\nColumn names of input population:",names(pop)),
                          collapse=" "))

  if(requireNamespace("validatepop", quietly=TRUE)) {
    validatepop::validate_population(pop,
                                     col_aggregation = col_aggregation,
                                     test_complete = TRUE,
                                     test_unique = FALSE)
  }

  if("age" %in% col_aggregation) {
    warning("births_null won't use age as an aggregation level, as the column must is added to the output with value zero")
  }

  if(nrow(pop) == 0) {
    warning("births_null was given an empty input table")
  }

  # If we were working with a fertility dataset as well, we would validate its aggregation levels
  # and check a join is possible with validate_join_population()
}




# Check the function output isn't doing anything unexpected
validate_births_output <- function(pop, col_aggregation, births) {

  assert_that(all(col_aggregation %in% names(births)))

  assert_that("births" %in% names(births))
  assert_that("age" %in% names(births))

  assert_that(all(complete.cases(births)))

  if(requireNamespace("validatepop", quietly = TRUE)) {
    validatepop::validate_join_population(pop,
                                          births,
                                          cols_common_aggregation = col_aggregation,
                                          pop1_is_subset = FALSE,
                                          many2one = TRUE,
                                          one2many = FALSE)
  }
}



# Detect and warn when a factor has unused levels
warn_unused_factor_levels <- function(pop) {
  for(x in names(pop)) {
    if(is.factor(pop[[x]]) && length(setdiff(levels(pop[[x]]), pop[[x]])) != 0 ) {
      warning(paste("births_null found unused factor levels in the input's", x, "column"))
    }
  }
  invisible(TRUE)
}
