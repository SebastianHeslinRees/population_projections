#' Age on a population
#'
#' @param popn A data frame containing a cohort population, with year and age columns
#' 
#' @return A data frame with age and year columns advanced by one year
#'
#' @importFrom dplyr mutate filter
#' @importFrom magrittr %>%
#'
#' @export

# TODO Implement tests
# TODO Add checks, and user-specified age column

age_on_sya <- function(popn) {
  # TODO: fix so as not to kill off all the 91 yr olds
  aged_popn <- popn %>%
    mutate(year = year + 1, age = age + 1) %>%
    filter(age %in% popn$age)
  return(aged_popn)
  
}