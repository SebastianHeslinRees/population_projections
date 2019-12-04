#' Read in constraint files
#'
#' @param popn_path String
#' @param births_path String
#' @param deaths_path String
#' @param int_in_path String
#' @param int_out_path String
#' @param cross_in_path String
#' @param cross_out_path String
#'
#' @return List of dataframes
#'
#' @export

get_constraints_from_file <- function(popn_path, births_path, deaths_path,
                                      int_in_path, int_out_path, cross_in_path, cross_out_path) {

  popn <- readRDS(popn_path) %>% as.data.frame()
  births <- readRDS(births_path) %>% as.data.frame()
  deaths <- readRDS(deaths_path) %>% as.data.frame()
  int_in <- readRDS(int_in_path) %>% as.data.frame()
  int_out <- readRDS(int_out_path) %>% as.data.frame()
  cross_in <- readRDS(cross_in_path) %>% as.data.frame()
  cross_out <- readRDS(cross_out_path) %>% as.data.frame()

  constraints <- list(population_constraint = popn,
                      births_constraint = births,
                      deaths_constraint = deaths,
                      international_in_constraint = int_in,
                      international_out_constraint = int_out,
                      cross_border_in_constraint = cross_in,
                      cross_border_out_constraint = cross_out)

}

#----------------------

#' #' Read data from files into a named list
#'
#' @param paths Character. A vector of file paths
#' @param names Character. A vector of names for use in the output named list.
#'  Must be in the same order as the \code{paths} parameter.#'
#'
#' @return List of dataframes
#'
#' @export
#'
#' @example
#'
#' p <- c("input_data/constraints/npp_2016_cross_border_in_constraint.rds", "input_data/constraints/npp_2016_cross_border_out_constraint.rds")
#' n <- c("cross_border_in_constraint", "cross_border_out_constraint")
#'
#' f <- get_constraints_from_file_1(paths = p, names = n)

get_constraints_from_file_1 <- function(paths, names) {

  assertthat::assert_that(length(paths)==length(names))

  x <- list()

  for(i in seq(paths)){
    x[[names[i]]] <- readRDS(paths[i])
  }

  return(x)

}


#---------------


#' #' Read data from files into a named list
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
#' @example
#'
#' a <- list("cross_border_in_constraint" = "input_data/constraints/npp_2016_cross_border_in_constraint.rds",
#' "cross_border_out_constraint" = "input_data/constraints/npp_2016_cross_border_out_constraint.rds")
#'
#' g <- get_constraints_from_file_2(args = a)

get_constraints_from_file_2 <- function(args) {

  x <- list()

  for(i in seq(args)){
    x[[names(args)[[i]]]] <- readRDS(args[[i]])
  }

  return(x)

}


