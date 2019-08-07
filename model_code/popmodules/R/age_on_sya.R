library(dplyr)

age_on_sya <- function(popn) {
  # TODO: fix so as not to kill off all the 91 yr olds
  aged_popn <- popn %>%
    mutate(year = year + 1, age = age + 1) %>%
    filter(age %in% popn$age)
  return(aged_popn)
  
}