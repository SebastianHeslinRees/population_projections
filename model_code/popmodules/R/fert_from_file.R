#' Read fertility rates from file
#'
#' Given a filepath to a RDS file that contains fertility rates, return a
#' fertility rate dataframe
#'
#' @param filepath Filepath to the fertility RDS file
#' @return A data frame of fertility
#'
#' @import assertthat
#'
#' @examples
#'
#' fert <- popmodules::fert_from_file(filepath = "my_fert_file.rds")
#'
#' @export
#'
fert_from_file <- function(filepath) {

  # validate file type
  validate_fert_from_file_filetype(filepath)

  fertility <- readRDS(filepath)

  # validate fertility dataframe
  # TODO should we enforce that fertility dfs must be complete?
  validate_fert_from_file_fertility_df(fertility)

  # return fertility
  fertility

}

#----------------------------------
validate_fert_from_file_filetype <- function(filepath) {
  file_ext <- tolower(strsplit(basename(filepath), split="\\.")[[1]][[2]])

  assertthat::assert_that(file_ext == "rds",
                          msg = "fertility file must be a .rds file")

  invisible(TRUE)

}

validate_fert_from_file_fertility_df <- function(fertility) {
  # TODO update to be able to handle more flexible dataframes
  assertthat::assert_that(setequal(sort(names(fertility)), c("age", "gss_code", "rate", "sex", "year")),
                          msg = "fertility dataframe must have col names: age, gss_code, rate, sex, year")

  assertthat::assert_that(all(unique(fertility$sex) %in% c("male", "female")),
                          msg = "sex must be coded as: male, female")

  validate_population(fertility)

  invisible(TRUE)

}
