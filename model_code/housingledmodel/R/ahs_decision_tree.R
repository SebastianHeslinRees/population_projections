#' Average Household Size Decision Tree 
#' 
#' A function to decide which of 3 AHS values to use: an external input value,
#' a capped value or a value consistent with trending the population forward
#'
#' @param external_ahs A dataframe of AHS values from the ONS or DCLG household model
#' @param trend_ahs A dataframe of AHS values from calculated from the
#'   current projection year's trend population
#' @param ahs_cap A ataframe or NULL. If the cap has been set then a dataframe of
#'  cap values. If the cap hasn't been set then NULL.
#' @param projection_year Numeric. The projection year
#' @param ldd_final_year Numeric. The final year for which LDD is available
#' @param gss_code_list Character. A vector of gss_codes which need to have an
#'  AHS value returned
#'
#' @return A list where element 1 is the choice the tree made and element 2 is a
#'   dataframe of AHS values
#'
#' @import dplyr
#'

ahs_decision_tree <- function(external_ahs, trend_ahs, ahs_cap,
                              projection_year, ldd_final_yr, gss_code_list){
  
  #AHS decision tree
  average_household_size <- external_ahs %>%
    filter(gss_code %in% gss_code_list) %>%
    rename(external = ahs) %>%
    left_join(trend_ahs, by = c("year","gss_code"))
  
  if(projection_year <= ldd_final_yr){
    if(is.null(ahs_cap)){
      #before the max LDD data year always select the trend
      average_household_size <- average_household_size %>%
        mutate(cap = NA, ahs = trend, ahs_choice = "trend")
    } else {
      average_household_size <- average_household_size %>%
        left_join(ahs_cap, by = "gss_code") %>%
        mutate(ahs_choice = case_when(trend > cap ~ "cap",
                                      trend <= cap ~ "trend"),
               ahs = case_when(trend > cap ~ cap,
                               trend <= cap ~ trend))
    }
  } else {
    if(is.null(ahs_cap)){
      #after LDD but before cap is set select the higher of the trend or external AHS
      average_household_size <- average_household_size %>%
        mutate(cap = NA,
               ahs_choice = case_when(trend > external ~ "trend",
                                      external > trend ~ "external"),
               ahs = case_when(trend > external ~ trend,
                               external > trend ~ external))
    } else {
      average_household_size <- external_ahs %>%
        filter(gss_code %in% gss_code_list) %>%
        rename(external = ahs) %>%
        left_join(trend_ahs, by = c("year","gss_code")) %>%
        left_join(ahs_cap, by = "gss_code") %>%
        mutate(ahs_choice = case_when(cap < trend ~ "cap",
                                      cap < external ~ "cap",
                                      trend > external ~ "trend",
                                      external > trend ~ "external",
                                      TRUE ~ "trend"),
               ahs = case_when(cap < trend ~ cap,
                               cap < external ~ cap,
                               trend > external ~ trend,
                               external > trend ~ external,
                               TRUE ~ trend))
    }
  }
  
  ahs_choice <- select(average_household_size, year, gss_code, external, cap, trend, ahs_choice)
  ahs <- select(average_household_size, year, gss_code, ahs)
  
  return(list(ahs_choice = ahs_choice,
              ahs = ahs,
              ahs_cap = ahs_cap))
  
}