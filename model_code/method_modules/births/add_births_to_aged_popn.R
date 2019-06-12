library(dplyr)

add_births <- function(aged_popn, births) {
  
  # add the births to a df that looks like the original popn df
  age_0_popn <- aged_popn %>% filter(age == 1) %>%
    select(-value)
  
  age_0_popn <- left_join(age_0_popn, births, by = c("gss_code", "sex")) %>%
    mutate(age = 0)
  # test for any nas in the age 0 popn
  
  popn_w_births <- bind_rows(aged_popn, age_0_popn)
  
  return(popn_w_births)
  
}