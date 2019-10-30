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

project_forward_flat <- function(df, last_proj_yr) {

  hold_yr <- max(df$year)
  first_proj_yr <- hold_yr+1

  # filter to the hold year
  hold_data <- dplyr::filter(df, year == hold_yr)

  # TODO check that the df is not empty
  # TODO validate popn
  # copy hold_yr into the projection yrs
  projection_yrs <- first_proj_yr:last_proj_yr
  projection <- lapply(projection_yrs, function(x) dplyr::mutate(hold_data, year = x))

  projection <- dplyr::bind_rows(df, projection)


  # TODO check that there aren't any missing years between the past data and the projections
  # TODO check that there aren't any duplicate years
  # TODO validate projection
  # TODO add module function rules checks - must return fert/mort df

}



