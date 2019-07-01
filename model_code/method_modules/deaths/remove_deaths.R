library(dplyr)

remove_deaths <- function(popn, deaths) {
  
  popn <- popn %>% left_join(deaths) %>%
    mutate(value = value * (1-deaths)) %>%
    select(-deaths)
  
  return(popn)
  
}