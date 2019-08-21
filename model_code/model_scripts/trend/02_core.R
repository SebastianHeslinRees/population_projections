# TODO make this take a list? 
trend_core <- function(popn_mye, mortality, n_proj_yr) {
  library(dplyr)

  # Load core functions
  #age_on <- popmodules::age_on_sya
  age_on <- popmodules::popn_age_on
  calc_deaths <- popmodules::deaths_from_popn_mort
  
  # set up projection
  # TODO pass first_proj_yr in via funtion
  first_proj_yr <- max(popn_mye$year) + 1
  last_proj_yr <-  max(popn_mye$year) + n_proj_yr
  
  proj_popn <- list(popn_mye)
  proj_deaths <- list()
  curr_yr_popn <- popn_mye %>% filter(year == max(popn_mye$year))
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    aged_popn <- curr_yr_popn %>%
      age_on() 

    # aged on population is used due to definitions of MYE to ensure the correct denominator
    # population in population at 30th June
    # change rates are for changes that occured in the 12 months up to 30th June
    # age is the age the cohort is at 30th June
    deaths <- calc_deaths(popn = aged_popn,
                          mortality = filter(mortality, year == my_year),
                          col_count = "value",
                          col_rate = "rate")
    
    # TODO validate joins
    next_yr_popn <- aged_popn %>% 
      left_join(deaths, by = names(deaths)[names(deaths)!= "deaths"]) %>%
      mutate(value = value - deaths) %>%
      select(-deaths)
      
    proj_popn[[length(proj_popn)+1]] <- next_yr_popn
    proj_deaths[[length(proj_deaths)+1]] <- deaths

    curr_yr_popn <- next_yr_popn
    
  }
  
  proj_popn   <- data.frame(data.table::rbindlist(proj_popn))
  proj_deaths <- data.frame(data.table::rbindlist(proj_deaths))
  
  return(list(population = proj_popn, deaths = proj_deaths))
  
}
