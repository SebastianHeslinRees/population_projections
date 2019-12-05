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
                             hma_list,
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
  target <- apply_rate_to_population(households, ahs,
                                     col_popn = "households",
                                     col_rate = "ahs",
                                     col_out = "target_popn",
                                     col_aggregation = c("year","gss_code"))
  
  #6. Constrain births, deaths & international
  #Constrain functions should work here - might need some additional functionality
  births <- x
  
  deaths <- x
  
  int_out <- x
  
  #7. Add components from step 6 to domestic from step 1 & start population
  #Perhaps there's a need for a build_population_from_components function - we do something similar
  #at the end of the trend core at in step 10 here as well
  step_7 <- construct_popn_from_components(start_population,
                                           births,
                                           deaths,
                                           int_in,
                                           int_out,
                                           dom_in,
                                           dom_out,
                                           upc = NULL) %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  #8. Compare population from step 7 to target from step 5. Difference = domestic adjustment
  
  #10. Add components from step 6 to domestic from step 9 & start population
  #see 7
  step_10 <- construct_popn_from_components(start_population,
                                            births,
                                            deaths,
                                            int_in,
                                            int_out,
                                            dom_in,
                                            dom_out,
                                            upc = NULL) %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(reconstructed_popn)) %>%
    as.data.frame()
  
  #11. Constrain total population
  popn <- constrain_to_hma(popn = trend_projection["population"],
                           constraint = constraints['population_constraint'],
                           hma_list = hma_list,
                           col_aggregation = c("year","hma"),
                           col_popn = "births")
  
  #12. Compare population from step 11 to population from step 10. Difference = domestic adjustment
  #13. Adjust domestic
  #see 8 & 9
  
  return()
  
  #Things we want to return:
  # final pop and components
  # ahs used
  # report on the ahs choice made
  
}
