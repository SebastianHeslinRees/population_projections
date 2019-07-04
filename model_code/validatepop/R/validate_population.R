#' Validate a population dataset
#'
#' Performs common checks to validate a population dataset. The dataset is
#' expected to contain at least one 'aggregation' column (e.g. geography, age,
#' sex), and optionally 'data' columns, (e.g. count, rate).
#'
#' The function checks for missing values, duplicated aggregation levels, unused
#' aggregation levels (optional), negative data values (optional), and validates
#' the input against a provided 'comparison' input (optional).
#'
#' Currently the function can only process a single geography column - give it
#' the highest resolution contained in the data frame. Any columns not named are
#' ignored.
#'
#' The function will stop code execution with an error message if it finds a
#' problem.
#'
#' @param population A data frame containing population data.
#' @param colname_aggregation A string or character vector giving column names
#'   for levels of data aggregations: e.g. geographic area, age/age band, sex.
#'   It will be checked that these behave as a primary key. If there is more
#'   than one geography column (e.g. ward code, ward name, or ward code borough
#'   code) include only the highest resolution and only one column. Default
#'   value is columns for "area", "age", "sex".
#' @param colname_data A string of character vector giving column names for data
#'   (e.g. count, birth rate). Columns given will be checked for missing and
#'   negative values. Any columns not named in the input are currently ignored.
#'   Default value is NA.
#' @param test_completeness Logical. Run test to make sure all combinations of
#'   aggregation variables are present. The test will fail when lower geographic
#'   resolutions are present, so make sure these are not passed to or turn off
#'   \code{colname_aggregation}. Default TRUE.
#' @param check_negative_values Logical value requesting data are checked for
#'   negative values. Defaults to TRUE.
#' @param comparison_pop Optional. A data frame containing all aggregation
#'   levels of the population being tested (e.g. an initial population), so that
#'   they can be checked to match. Default to NA.
#' @param colname_comparison A character vector giving the names of the
#'   aggregation levels in the comparison population. If names are different
#'   between frames, provide a named character vector, as you would with by=c()
#'   in a join. Defaults to the value of \code{colname_aggregation}, accepts NA.
#' @return The input population, unchanged. Return is invisible, so the function
#'   can be called within a dplyr pipe or as a one-line validity test
#'
#' @importFrom assertthat assert_that
#' @import rlang
#' @export

# TODO: include a logical gss parameter that validates gss coding
# TODO: add an option to convert errors to warnings. Would anyone use that?

validate_population <- function( population,
                                 colname_aggregation = c("area", "age", "sex"),
                                 colname_data = NA,
                                 test_completeness = TRUE,
                                 check_negative_values = TRUE,
                                 comparison_pop = NA,
                                 colname_comparison = colname_aggregation) {

  # Check input parameters are legal and make sense
  check_validate_pop_input(population, colname_aggregation, colname_data, test_completeness, check_negative_values, comparison_pop, colname_comparison)

  # drop requested column names that don't exist (the above checks threw the necessary warnings)
  colname_aggregation <- colname_aggregation[ colname_aggregation %in% names(population) ]
  colname_data <- colname_data[ colname_data %in% names(population) ]  # works with colname_data = NA

  test_population <- dplyr::select(population, c({{colname_aggregation}}, {{colname_data}}))

  # For some reason things took ages with tibbles, so convert to data frame
  test_population <- as.data.frame(test_population)

  # CHECK: warn if there are any missing factor levels,
  for(colname in colname_aggregation) {
    if(is.factor(population[[colname]])) {
      n_missing_levels <- sum( !levels(population[[colname]]) %in% population[[colname]])
      if(n_missing_levels != 0) {
        warning(paste("Column", colname, "is a factor with", n_missing_levels, "level(s) not present in the input data"))
      }
    }
  }

  # drop unused factor levels
  test_population <- droplevels(test_population)
  # convert aggregation levels to factors
  test_population[colname_aggregation] <- lapply(test_population[colname_aggregation], as.factor)

  # CHECK: aggregation levels have no missing values
  for(colname in colname_aggregation) {
    # Error if missing aggregation vars
    assert_that( !any(is.na(test_population[[colname]])),
                 msg=paste("Aggregation column", colname, "contains missing values"))
  }

  # CHECK: warn if data columns have missing values
  if(length(colname_data)!=0) {
    for(colname in colname_data) {
      # Warning otherwise
      if(any(is.na(test_population[[colname]]))) {
        warning(paste0("validate_population found missing values in the",colname,"column"))
      }
    }
  }

  # CHECK: no duplicates in input aggregation levels
  n_duplicates <- sum(duplicated(test_population[,colname_aggregation]))
  assert_that(n_duplicates == 0,
              msg=paste("validate_population found", n_duplicates, "duplicated aggregation levels"))

  # CHECK: all combinations of aggregation levels are present (optional)
  if(test_completeness) {
    population_completed <- tidyr::complete( test_population, !!!dplyr::syms(colname_aggregation))
    n_missing_levels <- nrow(population_completed) - nrow(test_population)
    assert_that(n_missing_levels == 0,
                msg=paste("validate_population found", n_missing_levels, "missing aggregation levels"))
  }

  # CHECK: no negative counts in the data (optional)
  if(check_negative_values & length(colname_data)!=0) {
    for(colname in colname_data) {
      test_col <- as.character(test_population[[colname]]) # convert from factor
      assert_that( all( test_col >= 0),
                   msg=paste("validate_population found negative values in column",colname))
    }
  }


  # CHECK: if there's a comparison data frame, make sure the aggregation levels match
  if(!identical(comparison_pop, NA)) {

    # drop missing factor levels from the comparison population (we warned about them above)
    comparison_pop <- as.data.frame(droplevels(comparison_pop))
    # convert aggregation levels to factors
    comparison_pop[colname_comparison] <- lapply(comparison_pop[colname_comparison], as.factor)

    #browser()

    # rename columns in comparison table
    #TODO this is clumsy and someone could neaten it up
    if(!is.null(names(colname_comparison))) {
      ix <- names(colname_comparison) != "" # columns to rename
      colname_comparison <- colname_comparison[ix]
      for(i in 1:length(colname_comparison)) {
        ix <- which(names(comparison_pop) == colname_comparison[i])
        names(comparison_pop)[ix] <- names(colname_comparison)[i]
      }
      assert_that(!any(duplicated(names(comparison_pop))),
                  msg="Renaming columns in the comparison data frame in validate_population resulted in duplicate column names. This occurs when a column in pop2 isn't an aggregation level but has the same name as an aggregation level in pop1") # check no duplicate names
      assert_that(all(colname_aggregation %in% names(comparison_pop)))
    }

    # compare populations
    comparison <- dplyr::all_equal(test_population[colname_aggregation],
                                   comparison_pop[colname_aggregation],
                                   ignore_col_order = TRUE, ignore_row_order = TRUE, convert = TRUE)
    if(!isTRUE(comparison)) {
      stop(paste(c(
        "validate_population couldn't match aggregation levels in the input data compared to the provided comparison population",
        "(aggregating over",colname_aggregation,")",
        "with error message(s):\n", comparison),
        collapse=" "))
    }

  }

  #TODO valiate 'protected' column names

  invisible(population)
}






#--------------------------------------------------------------------

# Function to check that the input to validate_population is all legal

check_validate_pop_input <- function(population,
                                     colname_aggregation,
                                     colname_data,
                                     test_completeness,
                                     check_negative_values,
                                     comparison_pop,
                                     colname_comparison) {

  # test input parameters are of the correct type
  assert_that(is.data.frame(population),
              msg="validate_population expects a data frame as input")
  assert_that(is.character(colname_aggregation),
              msg="validate_population requires a vector of column names for input parameter colname_aggregation")
  assert_that(identical(colname_data, NA) | is.character(colname_data),
              msg="validate_population needs NA or a vector of column names for input parameter colname_data")
  assert_that(is.logical(test_completeness),
              msg="validate_population requires a logical value for input parameter test_completeness")
  assert_that(identical(comparison_pop, NA) | is.data.frame(comparison_pop),
              msg="validate_population requires NA or a data frame for input parameter comparison_pop")
  assert_that(is.logical(check_negative_values),
              msg="validate_population requires a logical value for input parameter check_negative_values")
  assert_that(identical(colname_comparison, NA) | is.character(colname_comparison),
              msg="validate_population requires NA or a vector of column names for input parameter colname_comparison")

  assert_that(identical(colname_comparison, NA) | length(colname_aggregation) == length(colname_comparison))

  # test at least one aggregation variable is present
  missing_cols <- colname_aggregation[ !colname_aggregation %in% names(population)]
  if(length(missing_cols) > 0) {
    warning(paste("Function validate_population expected column", missing_cols,"- not found", collapse="\n"))
  }

  colname_aggregation <- colname_aggregation[ colname_aggregation %in% names(population) ]
  assert_that(length(colname_aggregation) > 0,
              msg="validate_population found no valid aggregation columns")

  # test data column(s) exist, if given
  if(!identical(colname_data, NA)) {
    missing_cols <- colname_data[ !colname_data %in% names(population)]
    if(length(missing_cols) > 0) {
      warning(paste("Function validate_population expected column", missing_cols,"- not found", collapse="\n"))
    }
    colname_data <- colname_data[ colname_data %in% names(population) ]
  }
  if(identical(colname_data, NA) | length(colname_data)==0) {
    colname_data <- c()
  }

  # warn if there's overlap between data and aggregation columns
  if(any(colname_aggregation %in% colname_data)) {
    colname_overlap <- colname_aggregation[ colname_aggregation %in% colname_data ]
    warning(paste("Column",colname_overlap, "in both aggregation and data columns provided to validate_population", collapse="\n"))
  }

  # warn if population is empty
  if(nrow(population) == 0) {
    warning("Empty data frame passed to validate_population")
  }

  # validate the comparison population.
  if(!identical(comparison_pop, NA)) {
    tryCatch(validate_population(comparison_pop,
                                 colname_aggregation = unname(colname_comparison),
                                 colname_data = NA,
                                 comparison_pop = NA),
             error = function(e) stop(paste0("validate_population found an error in the comparison population it was given:\n",e)),
             warning = function(w) warning(paste0("validate_population threw a warning when checking the given comparison population:\n",w)))
  }

  invisible(TRUE)
}
