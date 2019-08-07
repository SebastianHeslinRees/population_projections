library(dplyr)

add_base_to_dev_popn <- function(popn, housing_popn) {
  
  # This function is nowhere near general enough.  Think about how to match the popn and housing_popn.
  # Maybe group the popn by the columns that are present in the housing_popn minus value
  
  popn_base <- popn %>%
    group_by(gss_code, year) %>%
    summarise(value = sum(value)) %>%
    mutate(year = year + 1) %>%
    as.data.frame()
  
  housing_popn <- filter(housing_popn, year %in% popn_base$year)
  
  target_popn <- left_join(popn_base, housing_popn, by = c("gss_code", "year")) %>%
    mutate(value.y = ifelse(is.na(value.y), 0, value.y),
           value = value.x + value.y) %>%
    select(-value.x, -value.y)
  return(target_popn)
  
}