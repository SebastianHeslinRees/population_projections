#TODO:
#checks and balances
#rename function

#' Create a single year of age population for an age band using the distribution
#' from another population 
#'
#' Where a population dataset has a maximum age of less than 90 this function will
#' distribute the final age group across single years from that age group upto 90.
#'
#' @param popn_1 A data frame containing a population with a maximum age less than 90.
#' @param popn_2 A data frame containing a population at sya upto 90
#' @param popn_1_col String. Name of columns in \code{popn_1} containing data
#' @param popn_1_col String. Name of columns in \code{popn_2} containing data
#' @param min_age
#' @param max_age
#' @param col_aggregation Character vector. The columns on whoich to join the two
#'  datasets \code{popn_1} and \code{popn_2}. Cannot contain \code{"age"}.
#'  Default \code{c("gss_code, "sex)}
#'
#' @return A dataframe of population data by single-year-of-age upto 90
#'
#' @import dplyr
#'
#' @export

distribute_age_band <- function(popn_1, popn_2, popn_1_col, popn_2_col,
                                min_age, max_age,
                                col_aggregation=c("gss_code","sex")){

  #TODO: This is still only set up for ward data, the elements are there for it to be expanded and made flexible
  #I just can't be bothered right now
  
  borough_distribution <- filter(popn_2, age >= min_age) %>%
    filter(age <= max_age) %>%
    group_by_at(col_aggregation) %>%
    mutate(borough_total = sum(!!sym(popn_2_col))) %>%
    as.data.frame() %>%
    mutate(age_distribution = ifelse(borough_total == 0, 0, !!sym(popn_2_col) / borough_total)) %>%
    select(c(col_aggregation, age, age_distribution))
  
  if("age" %in% names(popn_1)){
    popn_1 <- select(popn_1, -age)
  }
  
  extended <- popn_1 %>%
    left_join(borough_distribution, by=c(col_aggregation)) %>%
    mutate(!!popn_1_col := !!sym(popn_1_col)* age_distribution) %>%
    select(c(names(popn_1),"age"))%>%
    as.data.frame()
  
  return(extended)
  
}