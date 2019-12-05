housing_led_core <- function(start_population,
                             fertility_rates,
                             mortality_rates,
                             int_out_flows_rates,
                             int_in_flows,
                             domestic_rates,
                             int_out_method,
                             npp_constraints = NULL, upc = NULL,
                             constraints,
                             communal_establishment_population,
                             average_household_size,
                             development_trajectory,
                             dwelling2household_ratio,
                             projection_year = projection_year){
  
  #1. Run the trend model for one year####
  
  trend_population <- trend_core(start_population,
                                 fertility_rates, mortality_rates,
                                 int_out_flows_rates, int_in_flows,
                                 domestic_rates,
                                 int_out_method,
                                 constraints = npp_constraints,
                                 upc = upc,
                                 projection_year)
  
  
  #2. Calculate household population
  #There is something in the ons household model functions that does this
  #Is it worth making this a functions - its so simple
  #group this here or when it gets read in
  ce <- dtplyr::lazy_dt(communal_establishment_population) %>%
    group_by(gss_code, year) %>%
    summarise(communal_est_popn = sum(communal_establishment_population)) %>%
    data.frame()
  
  household_population <- dtplyr::lazy_dt(start_population) %>%
    group_by(gss_code, year) %>%
    summarise(popn = sum(popn)) %>%
    left_join(ce, by=c("gss_code","year")) %>%
    mutate(household_popn = popn - communal_est_popn) %>%
    as.data.frame()
  .check_negative_populations("houshold_popn") %>%
    select(-popn, -communal_est_popn)
  
  #3. Calculate trend AHS
  #Population divided by households
  curr_yr_trend_ahs <- left_join(household_population, households, by=c("year","gss_code")) %>%
    mutate(trend_ahs = household_popn/households) %>%
    selecy(-household_popn, -households)
  
  #Somethings that sets cap here
  
  #4. AHS Decision Tree
  #This will be new
  
  #5. Target population
  #Probably apply_rate_to_population
  target_adults <- apply_rate_to_population(households, ahs,
                                            col_popn = "households",
                                            col_rate = "ahs",
                                            col_out = "target_popn",
                                            col_aggregation = c("year","gss_code"))
  
  #6. Constrain births, deaths & international
  #Constrain functions should work here - might need some additional functionality
  births <- trend_projection["briths"] %>%
    filter(gss_code %in% hma$gss_code) %>%
    left_join(hma, by="gss_code") %>%
    group_by(hma) %>%
    summarise(births = sum(births)) %>%
    ungroup() %>%
    constrain_component(constraints,
                        col_aggregation = c("year","hma"),
                        col_popn = "births",
                        col_constraint = "births")
  
  constrain_to_hma <- function(population, constrain, hma_list,
                               col_popn, col_constraint,
                               col_aggregation = c("hma","year")){
    
    
    trend_projection["briths"] %>%
      filter(gss_code %in% hma$gss_code) %>%
      left_join(hma, by="gss_code") %>%
      group_by(hma) %>%
      summarise(births = sum(births)) %>%
      ungroup() %>%
      constrain_component(constraints,
                          col_aggregation = c("year","hma"),
                          col_popn = "births",
                          col_constraint = "births")
    
  }
  
  
  #7. Add components from step 6 to domestic from step 1 & start population
  #Perhaps there's a need for a build_population_from_components function - we do something similar
  #at the end of the trend core at in step 10 here as well
  
  #8. Compare population from step 7 to target from step 5. Difference = domestic adjustment
  #9. Adjust domestic
  #Is this a function to do both steps in one - ie identify adjustment and apply it
  
  #10. Add components from step 6 to domestic from step 9 & start population
  #see 7
  
  #11. Constrain total population
  
  #12. Compare population from step 11 to population from step 10. Difference = domestic adjustment
  #13. Adjust domestic
  #see 8 & 9
  
  return()
  
  #Things we want to return:
  # final pop and components
  # ahs used
  # report on the ahs choice made
  
}
