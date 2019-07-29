
#' Check one population is contained within another
#'
#' The function checks that every row of one population can be mapped to a row
#' in another, given common aggregation columns (e.g. age, geography). It is
#' designed to confirm that joins between population datasets, are going to
#' behave as you expect, and will throw errors if the mapping between datasets
#' is ambiguous or incomplete, or doesn't perform according to other optional
#' restrictions.
#'
#' By default the function just checks that one population's aggregation levels
#' are contained within another. The option \code{pop1_is_subset} requires the
#' levels to match exactly (ignoring duplicates). Turning off \code{one2many}
#' requires each row in \code{pop1} to map to exactly one level in \code{pop2} -
#' use this when you expect a left join to return the same number of rows as the
#' input. Turning off \code{many2one} requires levels in \code{pop2} to map to
#' at most one level in \code{pop1}, use this when you want to confirm a unique
#' mapping from \code{pop1} into code{pop2}.
#'
#' The function does not do a full validation of the population data: use
#' \code{validate_population} for that.
#'
#' @param pop1,pop2 Data frames.
#' @param cols_common_aggregation A character vector. Names of columns for
#'   data aggregation common to both input data frames (e.g. age, ward). If
#'   columns don't have the same name, use a named character vector (e.g.
#'   \code{"WARD"="WARD11CD"}). Defaults to column names shared between the
#'   input data frames, but it's best not to use this in case the overlap it
#'   finds is not what you expect.
#' @param pop1_is_subset Logical. When TRUE \code{pop1} is allowed to match to
#'   subset of \code{pop2}.When FALSE the aggregation levels should be the same
#'   (ignoring duplicates) and the mapping will be surjective, i.e. all
#'   aggregation levels must be present in both data frames. Default TRUE.
#' @param many2one Logical. Can multiple rows in \code{pop1} map to a single row
#'   in \code{pop2}? Set to FALSE when matching should be unique (i.e.
#'   injective), and leave TRUE when, e.g. \code{pop2} has fewer aggregation
#'   levels than code{pop2}, for example when validating a join from LSOA data
#'   to MSOA data on MSOA11CD.
#' @param one2many Logical. Can one row in the first population map to several
#'   in the second? Set to FALSE when testing a left join that expects the same
#'   number of rows out. Default TRUE.
#'
#' @return \code{pop1}, invisibly (stopping on failure).
#'
#' @importFrom assertthat assert_that
#' @export

validate_join_population <- function(pop1,
                                     pop2,
                                     cols_common_aggregation = intersect(names(pop1),names(pop2)),
                                     pop1_is_subset = TRUE,
                                     many2one = TRUE,
                                     one2many = TRUE) {

  validate_join_population_inputs(pop1, pop2, cols_common_aggregation, pop1_is_subset, many2one, one2many)

  cols_common_aggregation <- convert_to_named_vector(cols_common_aggregation)

  # swap names and values for joining from pop2 to pop1
  cols_common_aggregation_reverse <- names(cols_common_aggregation)
  names(cols_common_aggregation_reverse) <- cols_common_aggregation

  # warn if there are duplicated column names that *aren't* being joined on
  unjoined_pop1 <- names(pop1)[ !names(pop1) %in% names(cols_common_aggregation) ]
  unjoined_pop2 <- names(pop1)[ !names(pop2) %in% cols_common_aggregation ]

  shared_unjoined_intersection <- intersect(unjoined_pop1, unjoined_pop2)
  if(length(shared_unjoined_intersection) > 0) {
    warning(paste("Inputs to validate_join_population both contained a", shared_unjoined_intersection,
                  "column, but this was not specified as part of the join. A join output will contain these columns with .x and .y suffixes to the column"))
  }
  shared_unjoined_pop2  <- intersect(names(unjoined_pop2), names(cols_common_aggregation))
  if(length(shared_unjoined_pop2) > 0) {
    warning(paste("Inputs to validate_join_population are joining from column", shared_unjoined_pop2,
                  "but this is (separately) a column in the pop2 input. Output will include this column with a .y suffix"))
  }


  # cut down to only the columns we're interested in
  test_pop1 <- pop1[names(cols_common_aggregation)]
  test_pop2 <- pop2[cols_common_aggregation]
  # convert aggregation columns to factors with common levels
  for(i in 1:length(cols_common_aggregation)) {
    var1 <- names(cols_common_aggregation)[i]
    var2 <- cols_common_aggregation[i]
    agg_levels <- sort(union(test_pop1[[var1]], test_pop2[[var2]]))
    test_pop1[var1] <- factor(test_pop1[[var1]], levels=agg_levels)
    test_pop2[var2] <- factor(test_pop2[[var2]], levels=agg_levels)
  }


  assert_that(ncol(test_pop1) == ncol(test_pop2))
  # CHECK every level in pop1 maps to a level in pop2
  n_missing_levels <- nrow(test_pop1) - nrow(dplyr::semi_join(test_pop1, test_pop2, by=cols_common_aggregation))
  assert_that(n_missing_levels == 0,
             msg = paste("validate_join_population couldn't match", n_missing_levels,
                         "levels from the source to the target data frame"))

  # CHECK (optional) every level in pop2 also maps to a level in pop1
  if(!pop1_is_subset) {
    n_missing_levels <- nrow(test_pop2) - nrow(dplyr::semi_join(test_pop2, test_pop1, by=cols_common_aggregation_reverse))
    assert_that(n_missing_levels == 0,
               msg = paste("validate_join_population couldn't match all levels between inputs.",
                           n_missing_levels,"are present in the second data frame that aren't mapped to from the first",
                           "If it's ok that the first is a subset of the second, set pop1_is_subset = TRUE"))
  }

  # CHECK (optional) every level in pop2 matches at most one level in pop1
  if(!many2one) {
    pop2_trim_to_pop1 <- semi_join(test_pop2, test_pop1, by=cols_common_aggregation_reverse )
    assert_that(nrow(pop2_trim_to_pop1) == nrow(dplyr::left_join(pop2_trim_to_pop1, test_pop1, by=cols_common_aggregation_reverse)),
                msg = "validate_join_population detected several levels in the first population dataset matching to a single level in the second. If this is ok set many2one = TRUE")
  }

  # CHECK (optional) every level in pop1 matches to exactly one level in pop2
  if(!one2many) {
    assert_that(nrow(test_pop1) == nrow(dplyr::left_join(test_pop1, test_pop2, by=cols_common_aggregation)),
                msg = "validate_join_population detected levels in the first population dataset with several matches in the second. If this is ok set one2many = TRUE")
  }

  invisible(pop1)
}



#--------------------------------------------------------------------------------

# Type match the inputs to validate_join_population and check named columns are present
validate_join_population_inputs <-function(pop1,
                                           pop2,
                                           cols_common_aggregation,
                                           pop1_is_subset,
                                           many2one,
                                           one2many) {


  assert_that(is.data.frame(pop1),
              is.data.frame(pop2),
              msg="validate_overlapping_populations needs data frames as inputs for pop1 and pop2")
  assert_that(is.character(cols_common_aggregation),
              msg="validate_overlapping_populations needs a character vector of common aggregation column names as input")
  assert_that(length(cols_common_aggregation) > 0,
              msg="validate_overlapping_populations was given a zero-length vector of common column names to compare")
  assert_that(!any(is.na(cols_common_aggregation)),
              msg="validate_overlapping_populations can't handle missing in common aggreagation column names")
  assert_that(is.logical(pop1_is_subset),
              msg="validate_overlapping_populations needs a logical value for pop1_is_subset input parameter")
  assert_that(is.logical(many2one),
              msg="validate_overlapping_populations needs a logical value for many2one input parameter")
  assert_that(is.logical(one2many),
              msg="validate_overlapping_populations needs a logical value for one2many input parameter")

  if(nrow(pop1) == 0) {
    warning("An empty data frame was passed to validate_overlapping_populations as parameter pop1")
  }
  if(nrow(pop2) == 0) {
    warning("An empty data frame was passed to validate_overlapping_populations as parameter pop2")
  }

  cols_common_aggregation <- convert_to_named_vector(cols_common_aggregation)

  # Check all expected columns are present
  assert_that(all(names(cols_common_aggregation) %in% names(pop1)),
              msg=paste(c("validate_overlapping_populations couldn't find all expected column names in the input for the pop1 parameter.",
                          "\nExpected:",names(cols_common_aggregation),
                          "\nNames of input data frame:",names(pop1)),
                        collapse = " "))
  assert_that(all(cols_common_aggregation %in% names(pop2)),
              msg=paste(c("validate_overlapping_populations couldn't find all expected column names in the input for the pop2 parameter.",
                          "\nExpected:",cols_common_aggregation,
                          "\nNames of input data frame:",names(pop2)),
                        collapse = " "))

  # Check no missing values
  if(any(is.na(pop1[,names(cols_common_aggregation)]))) {
    warning("validate_overlapping_populations was given NA values in the aggregation levels of pop1")
  }
  if(any(is.na(pop2[,cols_common_aggregation]))) {
    warning("validate_overlapping_populations was given NA values in the aggregation levels of pop2")
  }


  invisible(TRUE)
}


#--------------------------------------------------------------------------------

# Convert non-named or partially-named character vector to a named character vector
convert_to_named_vector <- function(vec) {
  if(is.null(names(vec))) {
    vec <- setNames(vec, vec)
  } else {
    vec <- setNames(vec, ifelse(names(vec) == "", vec, names(vec)))
  }
  return(vec)
}
