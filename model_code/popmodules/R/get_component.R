#' Read in a component data RDS file 
#'
#' Read a RDS file containing population component data and filter
#' out years greater than a defined maximum year
#'
#' @param filepath String. A file path including the \code{.rds} suffix
#' @param max_yr Numeric. The maximum year of data required
#'
#' @return A data frame of component data
#' 
#' @import dplyr
#' 
#' @export

get_component <- function(filepath, max_yr) {
  
  component <- readRDS(filepath) %>%
    filter(year <= max_yr)
  
  return(component)
  
}