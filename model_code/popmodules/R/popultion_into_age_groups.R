#' Aggregate a single-year-of-age population into age groups
#'
#' Given an input population by sya the function will assign each age
#' to a defined age group using \code{base::cut} and then group and
#' summarise the data using \code{dplyr} functions
#'
#' @param population A data frame containing population data by single year of age
#' @param age_groups A numeric vector of breaks in the age data. The breaks are defined
#'  as the highest value in each bin. See \code{cut} for more details. Note: if
#'  \code{include.lowest} is set to \code{TRUE} (as it is by default) the lowest value
#'  must also be present
#' @param labels String. The labels to be used in the \code{age_group} column
#'   in the output. Must be the sanem length as age_groups.
#' @param data_cols. String. A vector conating the names of any columns
#'   containing data to be aggegated
#' @param include.lowest Logical. Passed to \code{cut}. Does the variable \code{age_groups}
#' include the lowest age. If set to FALSE the function will assume the first bin conatins any
#' value less than and including the stated value. Default \code{TRUE} 
#'
#' @return A data frame of population data grouped by age group
#'
#' @importFrom stats complete.cases
#' @importFrom dtplyr lazy_dt
#' 
#' @example 
#' sya_popn <- expand.grid(gss_code = "C-3P0", year = 2001:2005, sex = c("female","male"), age = 0:90, popn = 10)
#' 
#' age_group_popn <- population_into_age_groups(population = sya_popn,
#'                                              age_groups = c(0, 15, seq(19,89,5), Inf),
#'                                              labels = c("0_15","16_19","20_24","25_29","30_34","35_39","40_44",
#'                                                         "45_49","50_54","55_59","60_64","65_69",
#'                                                         "70_74","75_79","80_84","85_89","90+"),
#'                                              data_cols = "popn")

population_into_age_groups <- function(population, age_groups, labels, data_cols, include.lowest=T){
  
  pop <- population %>%
    mutate(age_group = cut(age,
                           breaks = age_groups,
                           include.lowest = include.lowest,
                           labels = labels)) %>%
    mutate(age_group = as.character(age_group)) %>%
    select(-age)
  
  cols <- names(pop)[!names(pop) %in% data_cols]
  
  pop <- lazy_dt(pop) %>%
    group_by_at(cols) %>%
    summarise_all(.funs=list(sum)) %>%
    ungroup() %>%
    as.data.frame()
  
  return(pop)
  
}
