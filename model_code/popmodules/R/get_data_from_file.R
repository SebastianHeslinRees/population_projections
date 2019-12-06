#' Read data from files into a named list
#'
#' @param args Named list or named vector where the value is the file path and the
#'  name is the name of the list element to write the data to
#'
#' @return List of dataframes
#'
#' @export
#'
#' @example
#'
#' a <- list(cross_border_in_constraint = "input_data/constraints/npp_2016_cross_border_in_constraint.rds",
#' cross_border_out_constraint = "input_data/constraints/npp_2016_cross_border_out_constraint.rds")
#'
#' g <- get_data_from_file_2(args = a)

get_data_from_file <- function(args) {

  x <- list()

  for(i in seq(args)){
    x[[names(args)[[i]]]] <- readRDS(args[[i]])
  }

  return(x)

}


