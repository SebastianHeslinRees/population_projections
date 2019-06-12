library(dplyr)

age_on <- function(popn) {
  
  aged_popn <- popn %>%
    mutate(year = year + 1, age = age + 1) %>%
    filter(age %in% popn$age)
  return(aged_popn)
  
}