library(dplyr)

simple_popn_from_dev <- function(development, ahs) {
  
  housing_popn <- development %>% 
    mutate(value = value * ahs)
  return(housing_popn)
  
}
