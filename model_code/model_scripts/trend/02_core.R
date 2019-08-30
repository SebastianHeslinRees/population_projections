# TODO make this take a list? 
trend_core <- function(population, births, deaths, fertility, mortality, first_proj_yr, n_proj_yr) {
  library(dplyr)
  library(assertthat)
  library(popmodules)
  
  # do checks on the input data
  validate_inputs <- function(population, fertility, mortality, first_proj_yr, n_proj_yr) {
    
    popmodules::validate_population(population, col_data = "popn")
    popmodules::validate_population(fertility, col_data = "rate")
    popmodules::validate_population(mortality, col_data = "rate")
    
    # check that the rates join onto the population
    ## TODO make the aggregations columns flexible. Make this more elegant.
    popmodules::validate_join_population(population %>% filter(year == first_proj_yr - 1) %>% select(gss_code, sex, age), 
                                         mortality %>% filter(year == first_proj_yr) %>% select(gss_code, sex, age))
    
    ## TODO make the aggregations columns flexible. Make this more elegant.
    popmodules::validate_join_population(population %>% filter(year == first_proj_yr - 1) %>% select(gss_code, age), 
                                         fertility %>% filter(year == first_proj_yr) %>% select(gss_code, age))
    
    # check that the coverage of years is correct
    last_proj_yr <- first_proj_yr + n_proj_yr -1
    assert_that((first_proj_yr - 1) %in% unique(population$year), msg = paste0("the population backseries doesn't contain the projection jump-off year (", first_proj_yr-1,")"))
    assert_that(all(first_proj_yr:last_proj_yr %in% fertility$year), msg = "the projected fertility data doesn't contain all the projection years")
    assert_that(all(first_proj_yr:last_proj_yr %in% mortality$year), msg = "the projected mortality data doesn't contain all the projection years")
    
    # check that the rates values are always between 0 and 1 
    assert_that(max(fertility$rate) <= 1 & min(fertility$rate >= 0), msg = "projected fertility contains rates outside the rage 0-1")
    assert_that(max(mortality$rate) <= 1 & min(mortality$rate >= 0), msg = "projected mortality contains rates outside the rage 0-1")
    
    invisible(TRUE)
  }
  
  validate_inputs(population, fertility, mortality, first_proj_yr, n_proj_yr)
  
  # Load core functions
  #age_on <- popmodules::age_on_sya
  age_on <- popmodules::popn_age_on
  calc_deaths <- popmodules::deaths_from_popn_mort
  calc_births <- popmodules::births_from_popn_fert
  
  # set up projection
  last_proj_yr <-  first_proj_yr + n_proj_yr -1
  proj_popn <- population %>% filter(year < first_proj_yr)
  curr_yr_popn <- population %>% filter(year == first_proj_yr - 1)
  proj_deaths <- deaths
  proj_births <- births
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    # aged on population is used due to definitions of MYE to ensure the correct denominator
    # population in population at 30th June
    # change rates are for changes that occured in the 12 months up to 30th June
    # age is the age the cohort is at 30th June
    aged_popn <- curr_yr_popn %>%
      age_on() 
    
    births <- calc_births(popn = aged_popn,
                          fertility = filter(fertility, year == my_year),
                          col_popn = "popn")
    aged_popn_w_births <- rbind(aged_popn, rename(births, popn = births))
    validate_population(aged_popn_w_births, col_data = "popn")
    
    deaths <- calc_deaths(popn = aged_popn_w_births,
                          mortality = filter(mortality, year == my_year),
                          col_popn = "popn",
                          col_rate = "rate")
    validate_population(deaths, col_data = "deaths")
    
    validate_join_population(aged_popn_w_births, deaths, many2one = FALSE, one2many = FALSE) 
    next_yr_popn <- aged_popn_w_births %>% 
      left_join(deaths, by = intersect(names(deaths), names(aged_popn_w_births))) %>%
      mutate(popn = popn - deaths) %>%
      select(-deaths)
    
    proj_popn <- rbind(proj_popn, next_yr_popn)
    proj_births <- rbind(proj_births, births)
    proj_deaths <- rbind(proj_deaths, deaths)
    
    curr_yr_popn <- next_yr_popn
    
  }
  
  return(list(population = proj_popn, deaths = proj_deaths, births = proj_births))
  
}
