#' Copy the hold year data into the projection years
#'
#' @param df A data frame
#' @param first_proj_yr The first year of the projection
#' @param n_proj_yr The number of years being projected
#' @param hold_yr The hold year
#'
#' @return A data frame containing the past years and the projection years
#'
#' @import assertthat
#'
#' @export

project_forward_flat <- function(df, first_proj_yr, n_proj_yr, hold_yr) {

  validate_inputs(df, first_proj_yr, hold_yr)

  # remove any data that is a projection year
  df <- dplyr::filter(df, year < first_proj_yr)

  # filter to the hold year
  hold_data <- dplyr::filter(df, year == hold_yr)
  # TODO check that the df is not empty
  # TODO validate popn

  # copy hold_yr into the projection yrs
  projection_yrs <- first_proj_yr:(first_proj_yr + n_proj_yr - 1)
  projection <- lapply(projection_yrs, function(x) dplyr::mutate(hold_data, year = x))

  projection <- dplyr::bind_rows(df, projection)

  # TODO check that there aren't any missing years between the past data and the projections
  # TODO check that there aren't any duplicate years
  # TODO validate projection
  # TODO add module function rules checks - must return fert/mort df

}

validate_inputs <- function(df, first_proj_yr, hold_yr) {
  assert_that("year" %in% names(df), msg = "dataframe must contain a year column")
  assert_that(hold_yr %in% unique(df$year), msg = "dataframe must contain the hold_year")
  assert_that(first_proj_yr > hold_yr, msg = "first projection year must be later than the hold year")
  invisible(TRUE)
}
