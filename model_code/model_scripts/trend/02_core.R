# TODO make this take a list? 
trend_core <- function(popn_mye, mortality, n_proj_yr) {
  library(dplyr)
  source("model_code/helper_functions/assign_function.R")
  
  # Load core functions
  age_on <- assign_function("model_code/method_modules/age_on/age_on_sya.R")
  calc_deaths <- assign_function("model_code/method_modules/deaths/deaths_from_popn_mort.R")
  
  # set up projection
  # TODO pass first_proj_yr in via funtion
  first_proj_yr <- max(popn_mye$year) + 1
  last_proj_yr <-  max(popn_mye$year) + n_proj_yr
  
  proj_popn <- popn_mye
  curr_yr_popn <- popn_mye %>% filter(year == max(popn_mye$year))
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    aged_popn <- curr_yr_popn %>%
      age_on() 
    
    # aged on population is used due to definitions of MYE to ensure the correct denominator
    # population in population at 30th June
    # change rates are for changes that occured in the 12 months up to 30th June
    # age is the age the cohort is at 30th June
    deaths <- calc_deaths(popn = aged_popn, mortality = filter(mortality, year == my_year))
    
    # TODO validate joins
    next_yr_popn <- aged_popn %>% 
      left_join(rename(deaths, deaths = value), by = names(deaths)[names(deaths)!= "value"]) %>%
      mutate(value = value - deaths) %>%
      select(-deaths)
      
    proj_popn <- rbind(proj_popn, next_yr_popn)

    curr_yr_popn <- next_yr_popn
    
  }
  
  return(list(proj_popn = proj_popn))
  
}
