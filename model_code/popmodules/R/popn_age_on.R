#' Age on population
#'
#' Given a cohort population, advance the cohort's age by one timestep/age band
#' and (if specified) increase the date year.
#'
#' The function works with cohort age data as either numeric data or as an
#' ordered factor (e.g. age bands). If age is numeric then all age bands must be
#' equally spaced by the interval given in \code{timestep}, usually 1.
#'
#' Ageing of the population is done by either adding the timestep to the age
#' (for a numeric age) or by incrementing to the next level of the factor (for
#' age as an ordered factor)./ If \code{col_year} is given, the year will also
#' be increased by \code{timestep}.
#'
#' The highest age/age band is considered to be unbounded. This means that when
#' a population is aged on, the population in this band remains in this band,
#' and the age/band below it is added to it. An ageing population will all
#' eventually be moved into this age band if no other processes are operating.
#'
#' When age bands are merged like this, the function combines data in other
#' columns using the following (crude) rules: if the data are numeric, sum them,
#' otherwise check they're an identical category (i.e. not age-dependent) and
#' return the value, otherwise throw an error. (So make sure year data are
#' specified in \code{col_aggregation})
#'
#' @param popn Data frame containing a cohort population.
#' @param col_aggregation Character vector of columns in \code{popn} acting as
#'   aggregation levels. Default c("year","gss_code","age","sex"). In the case
#'   of multiple geography columns, only supply the highest resolution column;
#'   other columns will be preserved.
#' @param col_age String denoting the age column. Default "age".
#' @param col_year String denoting the year column. Can be NULL. Default "year".
#' @param timestep Numeric denoting the model time step in years. If age data
#'   are numeric, they must be spaced by this interval. If they are an ordered
#'   factor, the function will advance the age by one age band regardless of
#'   this parameter. The \code{col_year} column, if present, will also be
#'   increased by \code{timestep} years. Default 1.
#' @param template_age_levels Numeric vector or a factor giving the expected
#'   levels for the population's ages. This is an optional validation tool that
#'   runs an extra check to make sure all expected levels are present. Default
#'   NULL.
#'
#' @return A data frame containing the population with age advanced by one time
#'   step (or age band). The lowest age will not be present in this output. Rows
#'   are arranged iby \code{col_aggregation}, in that order.
#'
#' @import assertthat
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @export

popn_age_on <- function(popn,
                        col_aggregation=c("year","gss_code","age","sex"),
                        col_age = "age",
                        col_year = "year",
                        timestep = 1,
                        template_age_levels = NULL,
                        births=NULL) {

  # TODO This is a mental amount of code to do something simple
  # We need to think about what we're trying to achieve here
  # Is this doing everything twice?

  # TODO Set this up so with an additional parameters data_combine_method, a
  # named list of functions describing how to how to combine data when they are
  # merged into the top age band - currently it is always with done with sum()

  # TODO Set this up to work with nesting aggregation levels

  # Validate inputs
  # ---------------
  validate_popn_age_on_input(popn,
                             col_aggregation,
                             col_age,
                             col_year,
                             timestep,
                             template_age_levels)


  # Standardise data
  # ----------------

  if(!col_age %in% col_aggregation) {
    col_aggregation <- c(col_aggregation, col_age)
  }
  if(floor(timestep)==timestep) {
    timestep <- as.integer(timestep)
  }

  # reorder col_aggregation to match input column order
  col_aggregation <- intersect(names(popn), col_aggregation)

  # Save some properties of the input for later
  popn_is_tibble <- "tbl" %in% class(popn)
  popn_groups <- dplyr::group_vars(popn)
  popn_factors <- names(popn)[ sapply(popn, is.factor) ]
  age_is_integer <- is.integer(popn[[col_age]]) && (is.null(col_year) || floor(timestep)==timestep)


  # This is a data.table implementation of the code. See below for the (slower) equivalent in the tidyverse.
  if(requireNamespace("data.table", quietly=TRUE)) {

    # Age on (data.table)
    # -------------------

    popn <- data.table::setDT(popn)

    # Increment the year
    if(!is.null(col_year)) {
      popn[, (col_year) := get(col_year) + timestep]
    }

    aged <- copy(popn)

    # If age is numeric, increment age
    if(is.numeric(popn[[col_age]])) {
      setkeyv(aged, col_age)
      max_age <- aged[, max(sort(unique(get(col_age))))]
      aged[get(col_age) != max_age, (col_age) := get(col_age) + timestep]
    }

    # Otherwise build vector of unique age bands and what they map to
    # (NB this method would also work for numeric age, but it's slower)
    else {
      old_age_values <- levels(popn[[col_age]]) # we've checked this is an ordered factor

      n <- length(old_age_values)
      max_age <- old_age_values[n]
      new_age_values <- c( old_age_values[-1], old_age_values[n] )  # Last two age bands map to the same band

      # Map old ages to new ages
      # TODO this might be faster in data.table
      aged[, (col_age) := plyr::mapvalues(get(col_age), old_age_values, new_age_values)]
    }

    # Our initial checks established that non-numeric data depends on age, so we can safely group by all non-numeric columns
    col_non_numeric <- names(popn)[ !sapply(popn, is.numeric)]
    col_agg_non_numeric <- union(col_aggregation, col_non_numeric)

    aged <- aged[, lapply(.SD, sum), by = col_agg_non_numeric]


    # Standardise and validate output (data.table)
    # --------------------------------------------

    data.table::setcolorder(aged, names(popn))
    data.table::setkeyv(aged, col_aggregation)

    # Match input formatting
    if(length(popn_factors) > 0) {
      aged[, lapply(.SD, as.factor), by=get(popn_factors)]
    }
    popn_not_factors <- setdiff(names(aged), popn_factors)
    if(length(popn_not_factors) > 0) {
      aged[, lapply(.SD, as.vector), by=get(popn_not_factors)]
    }

    if(age_is_integer) {
      aged[, (col_age) := as.integer(get(col_age))]
    }

    if(popn_is_tibble) {
      aged <- tibble::as_tibble(data.table::setDF(aged))
      if(length(popn_groups) > 0) {
        aged <- dplyr::group_by_at(aged, popn_groups, add=FALSE)
      }
    } else {
      aged <- data.table::setDF(aged)
    }
    popn <- data.table::setDF(popn)

  }

  # Equivalent to the above, but with the tidyverse

  if(!requireNamespace("data.table", quietly=TRUE)) {

    # Age on (tidyverse)
    # -------------------

    # Increment the year
    if(!is.null(col_year)) {
      popn[[col_year]] <- popn[[col_year]] + timestep
    }

    aged <- dplyr::ungroup(popn)
    # One way to speed up the function is to make this part of a pipeline with
    # the code below - but I'm not sure how much time it saves, and the code's
    # easier to read without the pipeline nonstandard evaluation etc.

    # If age is numeric, increment age
    if(is.numeric(popn[[col_age]])) {
      max_age <- max(popn[[col_age]])
      aged[[col_age]] <- ifelse(popn[[col_age]] == max_age, max_age, popn[[col_age]] + timestep)
    }

    # Otherwise build vector of unique age bands and what they map to
    # (NB this method would also work for numeric age, but it's slower)
    else {
      old_age_values <- levels(popn[[col_age]]) # we've checked this is an ordered factor

      n <- length(old_age_values)
      max_age <- old_age_values[n]
      new_age_values <- c( old_age_values[-1], old_age_values[n] )  # Last two age bands map to the same band

      # Map old ages to new ages
      aged[[col_age]] <- plyr::mapvalues(aged[[col_age]], old_age_values, new_age_values)
    }

    # Our initial checks established that non-numeric data depends on age, so we can safely group by all non-numeric columns
    col_non_numeric <- names(popn)[ !sapply(popn, is.numeric)]
    col_agg_non_numeric <- union(col_aggregation, col_non_numeric)

    aged <- aged %>%
      dplyr::group_by_at( col_agg_non_numeric, add=FALSE ) %>%
      dplyr::summarise_all( .funs = ~sum(.)) %>%
      dplyr::ungroup()


    # Standardise and validate output (tidyverse)
    # -------------------------------------------

    aged <- aged[names(popn)]

    # Match input formatting
    aged <- dplyr::mutate_at(aged, .vars = popn_factors, .funs = as.factor)
    popn_not_factors <- setdiff(names(aged), popn_factors)
    aged <- dplyr::mutate_at(aged, .vars = popn_not_factors, .funs = as.vector)

    if(age_is_integer) {
      aged[[col_age]] <- as.integer(aged[[col_age]])
    }

    if(popn_is_tibble) {
      aged <- tibble::as_tibble(aged)
      if(length(popn_groups) > 0) {
        aged <- dplyr::group_by_at(aged, popn_groups, add=FALSE)
      }
    } else {
      aged <- as.data.frame(aged)
    }
  }

  validate_popn_age_on_output(popn,
                              aged,
                              col_aggregation,
                              col_age)





  if(FALSE){

    #TODO Why can't this function look like this?

    aged <- popn %>%
      mutate(year = year +1,
             age = ifelse(age == 90, 90, age +1)) %>%
      group_by_at(col_aggregation) %>%
      summarise(popn = sum(popn)) %>%
      ungroup()

  }


  #Add births
  if(!is.null(births)){

    births <- births %>%
      filter(age == 0) %>%
      rename(popn = births) %>%
      select(names(aged))

    common_years <- intersect(aged[["year"]], births[["year"]])

    aged <- rbind(births, aged)%>%
      filter(year %in% common_years) %>%
      arrange(year, gss_code, age, sex)

  }

  return(aged)

}



# -------------------------------------------------------------------------



validate_popn_age_on_input <- function(popn,
                                       col_aggregation,
                                       col_age,
                                       col_year,
                                       timestep,
                                       template_age_levels) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "popn_age_on needs a data frame as input")
  assert_that(is.character(col_aggregation),
              msg = "popn_age_on needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_age),
              msg = "popn_age_on needs a string as the col_age parameter")
  assert_that(is.null(col_year) || is.string(col_year),
              msg = "popn_age_on needs NULL or a string as the col_year parameter")
  assert_that(is.number(timestep),
              msg = "popn_age_on needs a number as the timestep parameter")
  assert_that(is.null(template_age_levels) || is.vector(template_age_levels) || is.factor(template_age_levels)  ,
              msg = "popn_age_on needs a vector, a factor, or NULL as the template_age_levels parameter")

  # Check for naming conflicts
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in popn_age_on, all columns named in col_aggregation must be columns in the popn table")
  assert_that(col_age %in% names(popn),
              msg = "popn_age_on was given a col_age parameter that isn't a column in the input popn data frame")
  assert_that(is.null(col_year) || col_year %in% names(popn),
              msg = "popn_age_on was given a col_year parameter that isn't a column in the input popn data frame")
  assert_that(!any(duplicated(col_aggregation)),
              msg = "duplicated aggregation column names were provided to popn_age_on")

  # Validate template age levels (if supplied)
  if(!is.null(template_age_levels)) {
    template_age_levels <- unique(template_age_levels)

    if(is.factor(template_age_levels)) {
      assert_that(setequal(template_age_levels, levels(template_age_levels)),
                  msg = "popn_age_on's template_age_levels is a factor with the levels mismatching the contents")
    }

    missing_popn_age_levels <- setdiff(template_age_levels, popn[[col_age]])
    assert_that(length(missing_popn_age_levels) == 0,
                msg = paste(c("popn_age_on's input popn is missing levels present in the provided template_age_levels:",
                              missing_popn_age_levels), collapse = " "))

    missing_template_age_levels <- setdiff(popn[[col_age]], template_age_levels)
    assert_that(length(missing_template_age_levels) == 0,
                msg = paste(c("popn_age_on's input popn contains levels not present in the provided template_age_levels:",
                              missing_template_age_levels), collapse = " "))
  }

  # Other checks
  assert_that(nrow(popn) > 0,
              msg = "popn_age_on was given a data frame with 0 rows of input")
  assert_that(length(unique(popn[[col_age]])) > 1,
              msg = "popn_age_on was given a data frame containing only one age value")
  assert_that(is.null(col_year) || is.numeric(popn[[col_year]]),
              msg = "popn_age_on needs numeric values in the col_year year column")

  if(is.factor(popn[[col_age]])) {
    assert_that(setequal(popn[[col_age]], levels(popn[[col_age]])),
                msg = "popn_age_on's template_age_levels is a factor with the levels mismatching the contents")
  }

  assert_that(is.numeric(popn[[col_age]]) || is.ordered(popn[[col_age]]),
              msg = "popn_age_on: data in col_age must be numeric or an ordered factor")
  assert_that(timestep > 0,
              msg = "popn_age_on: timestep must be > 0")
  if(floor(timestep) != timestep) {
    warning("popn_age_on was given a non-integer timestep - it's not been tested for this situation so check the outputs")
  }
  if(!is.ordered(popn[[col_age]])) {
    assert_that(all( diff(sort(unique(popn[[col_age]]))) == timestep), # we know the data are complete so this simplification is valid
                msg = "popn_age_on needs all ages to be equally spaced (by timestep) in its input data (alternately, convert to an ordered factor)")
  }

  tryCatch(validate_population(popn,
                               col_aggregation,
                               test_complete = TRUE,
                               test_unique = TRUE,
                               check_negative_values = FALSE),
           error = function(e) stop(paste0("popn_age_on found an error in the input population it was given:\n",e,"\n")),
           warning = function(w) warning(paste0("popn_age_on threw a warning when checking the input population:\n",w,"\n"))
  )

  # Check no non-numeric columns depend on age
  col_non_numeric <- names(popn)[ !sapply(popn, is.numeric)]
  col_non_numeric <- union(col_aggregation, col_non_numeric)
  if(!identical(col_aggregation, col_non_numeric)) {
    col_agg_without_age <- setdiff(col_aggregation, col_age)
    test_unique <- dplyr::group_by_at(popn, col_agg_without_age) %>%
      dplyr::select(!!!syms(col_non_numeric), -!!sym(col_age))

    unique_levels_agg <- nrow(dplyr::summarise(test_unique)) # number of aggregation levels

    test_unique <- dplyr::group_by_all(test_unique)
    unique_levels_all <- nrow(dplyr::summarise(test_unique))       # number of unique rows of data

    assert_that(unique_levels_agg == unique_levels_all,
                msg = "popn_age_on: non-numeric, age-dependent data detected in input. The function can't deal with this.")
  }

  invisible(TRUE)
}


# -------------------------------------------------------------

validate_popn_age_on_output <- function(popn,
                                        aged,
                                        col_aggregation,
                                        col_age) {

  cols_numeric <- names(popn)[ sapply(popn, is.numeric) ] %>%
    setdiff(col_aggregation)
  popn_totals_in  <- sapply(popn[cols_numeric], sum) %>% round(digits=2)  # This will stop working if we let the function do more complex math with the numerics
  popn_totals_out <- sapply(aged[cols_numeric], sum) %>% round(digits=2)
  assert_that(identical(popn_totals_in, popn_totals_out),
              msg = "Error in popn_age_on: column sums have changed")

  # FIXME does this check take ages on large datasets?
  age_min <- min(popn[[col_age]])
  assert_that(setequal( setdiff(popn[[col_age]], age_min), aged[[col_age]]),
              msg = "Output age level mismatch")

  comparison_pop <- dplyr::filter(popn, .data[[col_age]] != age_min)
  if(is.factor(comparison_pop[[col_age]])) {
    comparison_pop[[col_age]] <- droplevels(comparison_pop[[col_age]])
  }
  validate_population(aged,
                      col_aggregation,
                      cols_numeric,
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = FALSE,
                      comparison_pop = comparison_pop)
}
