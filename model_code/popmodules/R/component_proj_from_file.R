#' Read component or component rate projection from file
#' (e.g. international in migration or fertility)
#'
#' Given a filepath to a RDS file that contains component projection, return a
#' component or component rate projection dataframe
#'
#' @param filepath A string giving the filepath to the component RDS file
#' @param proj_yrs A vector of years that should be present in projection data
#' @param col_data A string giving the name of the column which contains the data value
#' @param col_year A string giving the name of the column which contains the year. Defaults to "year"
#' @param col_sex A string giving the name of the column which contains the sex. Defaults to "sex".
#'   If there is no sex column set to NA
#' @param col_aggregation Character vector of columns acting as
#'   aggregation levels. Default c("year","gss_code","age","sex").  All columns not
#'   specified in \code{col_data}, \code{col_year} or \code{col_sex} must be
#'   specified here.
#' @return A data frame of component or component rate projections
#'
#' @import assertthat
#' @importFrom dplyr filter
#' @examples
#'
#' int_in <- popmodules::component_proj_from_file(filepath = "my_int_in_file.rds", first_proj_yr = 2018)
#'
#' @export
#'
component_proj_from_file <- function(filepath,
                                     proj_yrs,
                                     col_data,
                                     col_year = "year",
                                     col_sex = "sex",
                                     col_aggregation = c("year", "gss_code", "age", "sex")) {

  # validate file type
  validate_component_proj_from_file_filetype(filepath)

  component_proj <- readRDS(filepath)

  # validate component_proj dataframe
  # TODO should we enforce that component_proj dfs must be complete?
  validate_component_proj_from_file_data(component_proj, proj_yrs, col_data, col_year, col_sex, col_aggregation)

  component_proj <- filter_at(component_proj, vars(col_year), any_vars(. %in% proj_yrs))
  # return component_proj
  component_proj

}

#----------------------------------
validate_component_proj_from_file_filetype <- function(filepath) {
  file_ext <- tolower(strsplit(basename(filepath), split="\\.")[[1]][[2]])

  assertthat::assert_that(file_ext == "rds",
                          msg = "component projection file must be a .rds file")

  invisible(TRUE)

}

validate_component_proj_from_file_data <- function(component_proj, proj_yrs, col_data, col_year, col_sex, col_aggregation) {
  # TODO update to be able to handle more flexible dataframes

  cols <- c(col_data, col_year, col_sex, col_aggregation)

  assertthat::assert_that(all(cols %in% names(component_proj)),
                          msg = paste("component projection dataframe must contain all specified columns:", paste(cols, collapse = ", ")))

  assertthat::assert_that(all(names(component_proj) %in% cols),
                          msg = paste("all component projection dataframe columns must be specified:", paste(names(component_proj), collapse = ", ")))

  if(!is.na(col_sex)) {
  assertthat::assert_that(all(unique(component_proj[[col_sex]]) %in% c("male", "female")),
                          msg = "sex must be coded as: male, female")
  }

  assertthat::assert_that(all(proj_yrs %in% component_proj[[col_year]]),
                          msg = "the component projection data does not contain all the projection years")

  validate_population(component_proj, col_aggregation = c(col_aggregation), col_data = col_data)

  invisible(TRUE)

}
