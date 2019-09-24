#' Read component projection from file (e.g. international in migration)
#'
#' Given a filepath to a RDS file that contains component projection, return a
#' component projection dataframe
#'
#' @param filepath Filepath to the component RDS file
#' @return A data frame of component projection
#'
#' @import assertthat
#'
#' @examples
#'
#' int_in <- popmodules::component_proj_from_file(filepath = "my_int_in_file.rds")
#'
#' @export
#'
component_proj_from_file <- function(filepath) {

  # validate file type
  validate_component_proj_from_file_filetype(filepath)

  component_proj <- readRDS(filepath)

  # validate component_proj dataframe
  # TODO should we enforce that component_proj dfs must be complete?
  validate_component_proj_from_file_data(component_proj)

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

validate_component_proj_from_file_data <- function(component_proj) {
  # TODO update to be able to handle more flexible dataframes

  aggregation_cols <- c("age", "gss_code", "sex", "year")

  assertthat::assert_that(all(aggregation_cols %in% names(component_proj)),
                          msg = paste("component projection dataframe must contain the following columns:", paste(aggregation_cols, collapse = ", ")))

  component_col <- names(component_proj)[!names(component_proj) %in% aggregation_cols]
  assertthat::assert_that(length(component_col) == 1,
                          msg = paste("component projection dataframe should only contain one column which is not one of:", paste(aggregation_cols, collapse = ", ")))

  assertthat::assert_that(all(unique(component_proj$sex) %in% c("male", "female")),
                          msg = "sex must be coded as: male, female")

  validate_population(component_proj)

  invisible(TRUE)

}
