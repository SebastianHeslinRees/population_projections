library(dplyr)

calc_housing_popn <- function(development, ahs) {
  
  housing_popn <- development %>% 
    mutate(value = value * ahs)
  return(housing_popn)
  
}
