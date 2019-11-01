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

get_constraints_from_file <- function(popn_path, births_path, deaths_path,
                                      int_in_path, int_out_path, cross_in_path, cross_out_path) {

  #TODO make this take a named list of arguments, and just loop through them to make the named list of data frames as its output

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
