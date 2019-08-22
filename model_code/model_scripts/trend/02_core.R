# TODO make this take a list? 
trend_core <- function(popn_mye, fertility, mortality, first_proj_yr, n_proj_yr) {
  library(dplyr)
  
  
  # do checks on the input data
  validate_inputs <- function(popn_mye, fertility, mortality, first_proj_yr, n_proj_yr) {
    
    popmodules::validate_population(popn_mye)
    popmodules::validate_population(fertility, test_complete = FALSE)
    popmodules::validate_population(mortality)
    
    # TODO make the aggregations columns flexible. Make this more elegant.
    popmodules::validate_join_population(popn_mye %>% filter(year == first_proj_yr - 1) %>% select(gss_code, sex, age), 
                                         mortality %>% filter(year == first_proj_yr) %>% select(gss_code, sex, age))
    
    # TODO make the aggregations columns flexible. Make this more elegant.
    popmodules::validate_join_population(popn_mye %>% filter(year == first_proj_yr - 1, age %in% fertility$age) %>% select(gss_code, age), 
                                         fertility %>% filter(year == first_proj_yr) %>% select(gss_code, age))
    
    last_proj_yr <- first_proj_yr + n_proj_yr -1
    assertthat::assert_that((first_proj_yr - 1) %in% unique(popn_mye$year))
    assertthat::assert_that(all(first_proj_yr:last_proj_yr %in% fertility$year))
    assertthat::assert_that(all(first_proj_yr:last_proj_yr %in% mortality$year))
    
    
    
    invisible(TRUE)
  }
  
  validate_inputs(popn_mye, fertility, mortality, first_proj_yr, n_proj_yr)
  
  # Load core functions
  #age_on <- popmodules::age_on_sya
  age_on <- popmodules::popn_age_on
  calc_deaths <- popmodules::deaths_from_popn_mort
  calc_births <- popmodules::births_from_popn_fert
  
  # set up projection
  last_proj_yr <-  first_proj_yr + n_proj_yr -1
  proj_popn <- popn_mye %>% filter(year < first_proj_yr)
  curr_yr_popn <- popn_mye %>% filter(year == first_proj_yr - 1)
  proj_deaths <- NULL
  
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
    
    births <- calc_births(popn = aged_popn,
                          fertility = filter(fertility, year == my_year))
   
    # TODO validate joins
    next_yr_popn <- aged_popn %>% 
      left_join(deaths, by = names(deaths)[names(deaths)!= "deaths"]) %>%
      mutate(value = value - deaths) %>%
      select(-deaths)
    
    proj_popn <- rbind(proj_popn, next_yr_popn)
    proj_deaths <- rbind(proj_deaths, deaths)
    
    curr_yr_popn <- next_yr_popn
    
  }
  
  return(list(population = proj_popn, deaths = proj_deaths))
  
}
