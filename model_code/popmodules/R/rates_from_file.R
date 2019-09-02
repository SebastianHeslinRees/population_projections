#' Read component rates from file (e.g. fertility)
#'
#' Given a filepath to a RDS file that contains component rates, return a
#' component rate dataframe
#'
#' @param filepath Filepath to the component RDS file
#' @return A data frame of component rates
#'
#' @import assertthat
#'
#' @examples
#'
#' fert <- popmodules::rates_from_file(filepath = "my_fert_file.rds")
#'
#' @export
#'
rates_from_file <- function(filepath) {

  # validate file type
  validate_rates_from_file_filetype(filepath)

  rates <- readRDS(filepath)

  # validate rates dataframe
  # TODO should we enforce that rates dfs must be complete?
  validate_rates_from_file_data(rates)

  # return rates
  rates

}

#----------------------------------
validate_rates_from_file_filetype <- function(filepath) {
  file_ext <- tolower(strsplit(basename(filepath), split="\\.")[[1]][[2]])

  assertthat::assert_that(file_ext == "rds",
                          msg = "rates file must be a .rds file")

  invisible(TRUE)

}

validate_rates_from_file_data <- function(rates) {
  # TODO update to be able to handle more flexible dataframes
  assertthat::assert_that(setequal(sort(names(rates)), c("age", "gss_code", "rate", "sex", "year")),
                          msg = "rates dataframe must have col names: age, gss_code, rate, sex, year")

  assertthat::assert_that(all(unique(rates$sex) %in% c("male", "female")),
                          msg = "sex must be coded as: male, female")

  validate_population(rates)

  invisible(TRUE)

}
