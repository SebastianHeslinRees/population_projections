# TODO make this take a list? 
trend_core <- function(population, births, deaths, int_out, int_in,
                       fertility, mortality, int_out_rate, int_in_proj,
                       first_proj_yr, n_proj_yr) {
  library(dplyr)
  library(assertthat)
  library(popmodules)
  
  validate_trend_core_inputs(population, births, deaths, int_out, int_in,
                             fertility, mortality, int_out_rate, int_in_proj,
                             first_proj_yr, n_proj_yr)

  
  # Load core functions
  #age_on <- popmodules::age_on_sya
  age_on <- popmodules::popn_age_on
  calc_deaths <- popmodules::component_from_popn_rate
  calc_births <- popmodules::births_from_popn_fert
  calc_int_out <- popmodules::component_from_popn_rate
  
  # set up projection
  last_proj_yr <-  first_proj_yr + n_proj_yr -1
  proj_popn <- population %>% filter(year < first_proj_yr)
  curr_yr_popn <- population %>% filter(year == first_proj_yr - 1)
  proj_deaths <- deaths
  proj_births <- births
  proj_int_out <- int_out
  proj_int_in <- int_in
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    # TODO pass births, deaths, migration function in via list along with their arguments to make the core more flexible.
    # Would remove need for hard coded internation out migration method switch
    
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
    validate_population(aged_popn_w_births, col_data = "popn", comparison_pop = mutate(curr_yr_popn, year=year+1))
    
    deaths <- calc_deaths(popn = aged_popn_w_births,
                          component_rate = filter(mortality, year == my_year),
                          col_popn = "popn",
                          col_rate = "rate",
                          col_component = "deaths")
    validate_population(deaths, col_data = "deaths", comparison_pop = mutate(curr_yr_popn, year=year+1))
    validate_join_population(aged_popn_w_births, deaths, many2one = FALSE, one2many = FALSE) 
    
    
    # TODO add switch for changing whether international out is rates based, or just numbers to match international in
    
    int_out <- calc_int_out(popn = aged_popn_w_births,
                            component_rate = filter(int_out_rate, year == my_year),
                            col_popn = "popn",
                            col_rate = "rate",
                            col_component = "int_out")
    validate_population(int_out, col_data = "int_out")
    validate_join_population(aged_popn_w_births, int_out, many2one = FALSE, one2many = FALSE)
    
    int_in <- int_in_proj %>% filter(year == my_year)
    validate_population(int_out, col_data = "int_in")
    validate_join_population(aged_popn_w_births, int_in, many2one = FALSE, one2many = FALSE)
    
    next_yr_popn <- aged_popn_w_births %>% 
      left_join(deaths, by = c("year", "gss_code", "age", "sex")) %>%
      left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
      left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
      mutate(popn = popn - deaths - int_out + int_in) %>%
      select(-c(deaths, int_in, int_out))

    
    proj_popn <- rbind(proj_popn, next_yr_popn)
    proj_births <- rbind(proj_births, births)
    proj_deaths <- rbind(proj_deaths, deaths)
    proj_int_out <- rbind(proj_int_out, int_out)
    proj_int_in <- rbind(proj_int_in, int_in)
    
    curr_yr_popn <- next_yr_popn
    
  }
  
  return(list(population = proj_popn, deaths = proj_deaths, births = proj_births, int_out = proj_int_out, int_in = proj_int_in))
  
}


# do checks on the input data
validate_trend_core_inputs <- function(population, births, deaths, int_out, int_in,
                                       fertility, mortality, int_out_rate, int_in_proj,
                                       first_proj_yr, n_proj_yr) {
  
  popmodules::validate_population(population, col_data = "popn")
  
  popmodules::validate_population(fertility, col_data = "rate")
  popmodules::validate_population(mortality, col_data = "rate")
  popmodules::validate_population(int_out_rate, col_data = "rate")
  popmodules::validate_population(int_in_proj, col_data = "int_in")
  
  # check that the rates join onto the population
  ## TODO make the aggregations columns flexible. Make this more elegant.
  popmodules::validate_join_population(population, mortality, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, fertility, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, int_out_rate, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  
  # check that the coverage of years is correct
  last_proj_yr <- first_proj_yr + n_proj_yr -1
  assert_that((first_proj_yr - 1) %in% unique(population$year), msg = paste0("the population backseries doesn't contain the projection jump-off year (", first_proj_yr-1,")"))
  assert_that(all(first_proj_yr:last_proj_yr %in% fertility$year), msg = "the projected fertility data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% mortality$year), msg = "the projected mortality data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% int_out_rate$year), msg = "the projected int_out_rate data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% int_in_proj$year), msg = "the projected int_in data doesn't contain all the projection years")
  
  # check that the rates values are always between 0 and 1 
  assert_that(max(fertility$rate) <= 1 & min(fertility$rate) >= 0, msg = "projected fertility contains rates outside the range 0-1")
  assert_that(max(mortality$rate) <= 1 & min(mortality$rate) >= 0, msg = "projected mortality contains rates outside the range 0-1")
  assert_that(max(int_out_rate$rate) <= 1 & min(int_out_rate$rate) >= 0, msg = "projected international out migration rate contains rates outside the range 0-1")
  
  invisible(TRUE)
}