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
    as.data.frame() %>%
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
  #So that total match at the borough level
  
  #TODO This is a messy way to acheive this
  borough_constraint_gss <- constraints['population']$gss_code
  
  births <- trend_projection['births'] %>%
    filter(gss_code %in% borough_constraint_gss) %>%
    constrain_component(
      constraint = constraints['births_constraint'],
      col_aggregation = c("year","gss_code"),
      col_popn = "births",
      col_constraint = "births") %>%
    rbind(filter(trend_projection['births'], !gss_code %in% borough_constraint_gss))
  
  deaths <- trend_projection['deaths'] %>%
    filter(gss_code %in% borough_constraint_gss) %>%
    constrain_component(constraint = constraints['death_constraint'],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "deaths",
                        col_constraint = "deaths") %>%
    rbind(filter(trend_projection['deaths'], !gss_code %in% borough_constraint_gss))
  
  int_out <- rend_projection['int_out'] %>%
    filter(gss_code %in% borough_constraint_gss) %>%
    constrain_component(popn = trend_projection['int_out'],
                        constraint = constraints['int_out_constraint'],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "int_out",
                        col_constraint = "int_out") %>%
    rbind(filter(trend_projection['int_out'], !gss_code %in% borough_constraint_gss))
  
  
  #7. Add components from step 6 to domestic from step 1 & start population
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
  
  #8. Compare population from step 7 to target from step 5.
  #   Difference = domestic adjustment
  #   Adjust domestic
  adjusted_domestic <- adjust_domestic_migration(popn = step_7, target = target,
                                                 dom_in = trend_projection['dom_in'],
                                                 dom_out = trend_projection['dom_out'])
  
  #9. Add components from step 6 to domestic from step 8 & start population
  step_9 <- construct_popn_from_components(start_population,
                                           births,
                                           deaths,
                                           int_in,
                                           int_out,
                                           adujusted_domestic['dom_in'],
                                           adjusteed_domestic['dom_out'],
                                           upc = NULL) %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  #10. Constrain total population
  constrained_popn <- constrain_to_hma(popn = step_9,
                                       constraint = constraints['population_constraint'],
                                       hma_list = hma_list,
                                       col_aggregation = c("year","hma"),
                                       col_popn = "popn")
  
  #11. Compare population from step 10 to population from step 9.
  #    Difference = domestic adjustment
  #    Adjust domestic
  final_domestic <- adjust_domestic_migration(popn = step_9, target = constrained_popn,
                                              dom_in = trend_projection['dom_in'],
                                              dom_out = trend_projection['dom_out'])
  
  
  return(list(population = constrained_popn,
              births = births,
              death = deaths,
              int_in = int_in,
              int_out = int_out,
              dom_in = final_domestic$dom_in,
              dom_out = final_domestic$dom_out,
              ahs = ahs,
              ahs_choice))
}
