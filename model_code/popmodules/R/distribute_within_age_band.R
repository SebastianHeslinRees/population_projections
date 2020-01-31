#' Create a single year of age population
#' 
#' Where a population is availble for an age band only use the sya distribution
#' from another population to distribute the total over the single ages in the band.
#'
#' @param popn_1 A data frame containing population data for an age group.
#' @param popn_2 A data frame containing a population at sya covering the same age group.
#' @param popn_1_col String. Name of column in \code{popn_1} containing the population data
#' @param popn_1_col String. Name of column in \code{popn_2} containing the population data
#' @param min_age Numeric. The minimum age of the age band.
#' @param max_age Numeric. The maximim age of the age band.
#' @param col_aggregation Character vector. The columns on whoich to join the two
#'  datasets \code{popn_1} and \code{popn_2}. Cannot contain \code{"age"}.
#'  Default \code{c("gss_code, "sex)}
#'
#' @return A dataframe of population data by single-year-of-age for the input age range
#'
#' @import dplyr
#' @importfrom assertthat assert_that
#' @export

distribute_within_age_band <- function(popn_1, popn_2, popn_1_col, popn_2_col,
                                       min_age, max_age,
                                       col_aggregation=c("gss_code","sex")){

  assert_that(!"age" %in% col_aggregation,
              msg = "In distribute_within_age_band, {age} cannot be specified in the col_aggreation variable")
  
  distribution <- filter(popn_2, age >= min_age) %>%
    filter(age <= max_age) %>%
    group_by_at(col_aggregation) %>%
    mutate(popn_total = sum(!!sym(popn_2_col))) %>%
    mutate(age_distribution = ifelse(popn_total == 0, 1/n(), !!sym(popn_2_col) / popn_total)) %>%
    as.data.frame() %>%
    select(c(col_aggregation, age, age_distribution))
  
  if("age" %in% names(popn_1)){
    popn_1 <- select(popn_1, -age)
  }
  
  distributed <- popn_1 %>%
    left_join(distribution, by=c(col_aggregation))
  
  unable_to_scale <- distributed %>%
    summarise(sum_popn_1 = sum(!!sym(popn_1_col)),
              max_age_dist = max(age_distribution)) %>%
    filter(max_age_dist == 0 & sum_popn_1 !=0)
  
  if(nrow(unable_to_scale) != 0) {
    warning("distribute_within_age_band was asked to scale populations of zero to non-zero sizes at ",
            nrow(unable_to_scale), " aggregation levels, with target populations summing to ",
            sum(unable_to_scale$sum_popn_1), ". These populations will be unscaled and ",
            "the output will be short by this many people.")
  }
  
  distributed <- distributed %>%
    mutate(!!popn_1_col := !!sym(popn_1_col)* age_distribution) %>%
    select(c(names(popn_1),"age"))%>%
    as.data.frame()
  
  return(distributed)
  
}