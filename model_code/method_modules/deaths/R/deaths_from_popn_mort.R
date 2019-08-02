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
#'   counts. Default "value"
#' @param col_rate String. Name of column in \code{mortality} containing
#'   mortality rates. Default "value"
#' @param col_deaths String. Name of column to write deaths to in the output.
#'   Default "value"
#'
#' @return A data frame of deaths with one row for each distinct value of the
#'   input \code{col_aggregation} column.
#'
#' @import assertthat
#' @importFrom magrittr %>%
#'
#' @export
#'

# TODO: make this a more general CoC from rates, popn
# TODO: add a by argument to the function, with default c("gss_code", "sex", "age", "year")

deaths_from_popn_mort <- function(popn,
                                  mortality,
                                  col_aggregation = c("year", "gss_code", "sex", "age"),
                                  col_count = "value",
                                  col_rate = "value",
                                  col_deaths = "value") {

  # Validate input
  # --------------
  validate_deaths_from_popn_mort_input(popn, mortality, col_aggregation, col_count, col_rate, col_deaths)


  # Standardise data
  # ----------------

  # Reformat col_aggregation to a named vector mapping between popn columns and mortality columns
  col_aggregation <- convert_to_named_vector(col_aggregation)
  # and reorder it to match popn's column ordering
  popn_cols_to_aggregate <- intersect( names(popn), names(col_aggregation) )
  col_aggregation <- col_aggregation[ popn_cols_to_aggregate ]

  join_by <- col_aggregation[col_aggregation %in% names(mortality)]  # this is a named character vector

  # Trim inputs to the columns we care about (reduces the chances of column name conflicts)
  popn_cols <- names(col_aggregation)
  mort_cols <- as.character(join_by)
  popn <- popn[c(popn_cols, col_count)]
  mortality <- mortality[c(mort_cols, col_rate)]

  # Make sure the columns that are factors match
  mortality <- match_factors(popn, mortality, col_aggregation)

  # Deal with the possibility of duplicate data column names
  if(col_count == col_rate) {
    col_count <- paste0(col_count, ".x")
    col_rate  <- paste0(col_rate,  ".y")
  }

  # Calculate deaths
  # ----------------
  deaths <- left_join(popn, mortality, by = join_by) %>%
    mutate(!!sym(col_deaths) := !!sym(col_count) * !!sym(col_rate) ) %>%
    select(!!!syms(popn_cols), !!sym(col_deaths))

  # Validate output
  # ---------------
  validate_deaths_from_popn_mort_output(popn,
                                        col_aggregation,
                                        col_deaths,
                                        deaths )

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
  assert_that(all(mortality[[col_rate]] >= 0 && mortality[[col_rate]] <= 1),
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

  if(requireNamespace("validatepop", quietly=TRUE)) {
    validatepop::validate_population(popn,
                                     col_aggregation = names(col_aggregation),
                                     col_data = col_count,
                                     test_complete = TRUE,
                                     test_unique = TRUE,
                                     check_negative_values = TRUE)
    validatepop::validate_population(mortality,
                                     col_aggregation = join_by,
                                     col_data = col_rate,
                                     test_complete = TRUE,
                                     test_unique = TRUE,
                                     check_negative_values = TRUE)
    validatepop::validate_join_population(popn,
                                          mortality,
                                          cols_common_aggregation = join_by,
                                          pop1_is_subset = TRUE,
                                          many2one = TRUE,
                                          one2many = FALSE)
  }

  invisible(TRUE)
}

# -----------


# Check the function output isn't doing anything unexpected

validate_deaths_from_popn_mort_output <- function(popn, col_aggregation, col_deaths, deaths) {

  col_aggregation <- convert_to_named_vector(col_aggregation)

  assert_that(all(names(col_aggregation) %in% names(deaths)))

  assert_that(col_deaths %in% names(deaths))

  assert_that(all(stats::complete.cases(deaths)))

  if(requireNamespace("validatepop", quietly = TRUE)) {
    validatepop::validate_population(deaths,
                                     col_aggregation = names(col_aggregation),
                                     col_data = col_deaths,
                                     test_complete = TRUE,
                                     test_unique = TRUE,
                                     check_negative_values = TRUE,
                                     comparison_pop = popn)
  }

  invisible(TRUE)
}




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


# Function: given source and target data frames with a column mapping, add or
# remove factoring in the target to match the source
match_factors <- function(dfsource, dftarget, col_mapping) {
  for(i in  seq_along(col_mapping)) {
    icol <- col_mapping[i]
    if(is.factor(dfsource[[names(icol)]]) & !is.factor(dftarget[[icol]])) {
      dftarget[[icol]] <- as.factor(dftarget[[icol]])
    }

    source_col <- dfsource[[names(icol)]]
    target_col <- dftarget[[icol]]
    if(!is.factor(source_col) & is.factor(target_col)) {
      col_class <- class(source_col)
      if(col_class == "numeric") {
        dftarget[[names(icol)]] <- levels(target_col)[target_col] %>%
          as.numeric()
      } else {
        dftarget[[names(icol)]] <- as.character(target_col)
      }
    }
  }
  return(dftarget)
}
