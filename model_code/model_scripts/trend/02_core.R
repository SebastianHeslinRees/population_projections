trend_core <- function(popn_mye, n_proj_yr) {
  library(dplyr)
  source("model_code/helper_functions/assign_function.R")
  
  # Load core functions
  age_on <- assign_function("model_code/method_modules/age_on/age_on_sya.R")
  
  # set up projection
  first_proj_yr <- max(popn_mye$year) + 1
  last_proj_yr <-  max(popn_mye$year) + n_proj_yr
  
  proj_popn <- popn_mye
  curr_yr_popn <- popn_mye %>% filter(year == max(popn_mye$year))
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    aged_popn <- curr_yr_popn %>%
      age_on() 
    
    
    next_yr_popn <- aged_popn
    proj_popn <- rbind(proj_popn, next_yr_popn)
    
    curr_yr_popn <- next_yr_popn
    
  }
  
  return(list(proj_popn = proj_popn))
  
}
