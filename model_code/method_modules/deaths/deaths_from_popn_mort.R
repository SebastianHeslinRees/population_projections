library(dplyr)

deaths_from_popn_mort <- function(popn, mortality) {
  
  deaths <- left_join(popn, mortality, by = c("gss_code", "sex", "age", "year")) %>%
    rename(popn = value.x, mortality = value.y) %>%
    mutate(deaths = popn * mortality) %>%
    select(-popn, -mortality)
  
  return(deaths)
  
}