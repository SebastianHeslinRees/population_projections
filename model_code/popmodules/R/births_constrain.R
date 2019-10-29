#' Scale one population to match the totals of another
#'
#' A wrapper for \code{get_scaling_factor} which prepares births data
#' to feed into the function and which appliued the function's output
#' back to the bith data resulting in sacled births.
#'
#' The NPP births constraint is provided for ages 15-46 while the
#' model produces births for ages 15-49. This function wrangles the
#' input birth data so that it can be scaled using the \code{get_scaling_factor}
#' function.
#'
#' @param births A data frame containing births by mother's age data.
#' @param constraint A data frame containing births data at the same
#'   resolution or lower.
#'
#' @return A data frame of scaled births by mother's year of age.

births_constrain <- function(births, constraint){
  
  births <- births %>%
    select(year, gss_code, sex, age, births)
  
  max_constraint_age <- max(constraint$age)
  
  do_scale <- filter(births, substr(gss_code,1,1)=="E")
  dont_scale <- filter(births, substr(gss_code,1,1)!="E")
  
  scaling <- filter(do_scale, age >= max_constraint_age) %>%
    mutate(age = max_constraint_age) %>%
    group_by(year, gss_code, sex, age) %>%
    summarise(births = sum(births)) %>%
    ungroup()%>%
    rbind(filter(do_scale, age < max_constraint_age)) %>%
    group_by(year, sex, age) %>%
    summarise(births = sum(births)) %>%
    ungroup()%>%
    get_scaling_factors(constraint, col_popn="births")
  
  max_age_scaling <- filter(scaling, age == max_constraint_age) %>%
    select(-age)
  
  other_scaling <- filter(scaling, age < max_constraint_age)
  
  max_age_scaled <- filter(do_scale, age >= max_constraint_age) %>%
    left_join(max_age_scaling, by=c("year", "sex")) %>%
    mutate(popn_scaled = scaling * births) %>%
    select(year, gss_code, sex, age, popn_scaled)
  
  other_scaled <- filter(do_scale, age < max_constraint_age) %>%
    left_join(other_scaling, by=c("year","sex","age")) %>%
    mutate(popn_scaled = scaling * births) %>%
    select(year, gss_code, sex, age, popn_scaled)
  
  scaled <- rbind(other_scaled, max_age_scaled) %>%
    rename(births = popn_scaled) %>%
    rbind(dont_scale)
  
  testthat::expect_equal(nrow(scaled),nrow(births))
  
  return(scaled)
  
}