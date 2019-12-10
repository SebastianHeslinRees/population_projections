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
  browser()
  #1. Run the trend model for one year
  trend_projection <- trend_core(start_population,
                                 fertility_rates, mortality_rates,
                                 int_out_flows_rates, int_in_flows,
                                 domestic_rates,
                                 int_out_method = int_out_method,
                                 constraints = npp_constraints,
                                 upc = upc,
                                 projection_year)
  
  #2. Constrain births, deaths & international
  #So that totals match at the borough level
  
  #TODO This is a messy way to acheive this
  borough_constraint_gss <- unique(constraints[['population_constraint']]$gss_code)
  
  births <- trend_projection[['births']] %>%
    filter(gss_code %in% borough_constraint_gss) %>%
    constrain_component(
      constraint = constraints[['birth_constraint']],
      col_aggregation = c("year","gss_code"),
      col_popn = "births",
      col_constraint = "births") %>%
    rbind(filter(trend_projection[['births']], !gss_code %in% borough_constraint_gss))
  
  deaths <- trend_projection[['deaths']] %>%
    filter(gss_code %in% borough_constraint_gss) %>%
    constrain_component(constraint = constraints[['death_constraint']],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "deaths",
                        col_constraint = "deaths") %>%
    rbind(filter(trend_projection[['deaths']], !gss_code %in% borough_constraint_gss))
  
  int_out <- trend_projection[['int_out']] %>%
    filter(gss_code %in% borough_constraint_gss) %>%
    constrain_component(constraint = constraints[['international_out_constraint']],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "int_out",
                        col_constraint = "int_out") %>%
    rbind(filter(trend_projection[['int_out']], !gss_code %in% borough_constraint_gss))
  
  #3. Calculate household population
  #There is something in the ons household model functions that does this
  #Is it worth making this a functions - its so simple
  #group this here or when it gets read in?
  ce <- dtplyr::lazy_dt(communal_establishment_population) %>%
    group_by(gss_code, year) %>%
    summarise(communal_est_popn = sum(communal_establishment_population)) %>%
    data.frame()
  
  household_population <- dtplyr::lazy_dt(trend_projection[['population']]) %>%
    group_by(gss_code, year) %>%
    summarise(popn = sum(popn)) %>%
    left_join(ce, by=c("gss_code","year")) %>%
    mutate(household_popn = popn - communal_est_popn) %>%
    as.data.frame() %>%
    #check_negative_populations("houshold_popn") %>%
    select(-popn, -communal_est_popn)
  
  #4. Calculate trend AHS
  #Population divided by households
  # curr_yr_trend_ahs <- left_join(household_population, households, by=c("year","gss_code")) %>%
  #   mutate(trend_ahs = household_popn/households) %>%
  #   selecy(-household_popn, -households)
  
  #Somethings that sets cap here
  
  #5. AHS Decision Tree
  #This will be new
  
  #6. Target population
  #Probably apply_rate_to_population
  # target <- apply_rate_to_population(households, ahs,
  #                                    col_popn = "households",
  #                                    col_rate = "ahs",
  #                                    col_out = "target_popn",
  #                                    col_aggregation = c("year","gss_code"))
  
  target <- mutate(household_population, target_popn = household_popn *1.1) %>%
    select(-household_popn) %>%
    filter(stringr::str_detect(gss_code, "E09"))
  
  #7. Add components from step 2 to domestic from step 1 & start population
  step_7 <- construct_popn_from_components(start_population,
                                           births,
                                           deaths,
                                           trend_projection[['int_in']],
                                           int_out,
                                           trend_projection[['dom_in']],
                                           trend_projection[['dom_out']],
                                           upc = NULL) %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  #8. Compare population from step 7 to target from step 5.
  #   Difference = domestic adjustment
  #   Adjust domestic
  adjusted_domestic <- step_7 %>%
    filter(gss_code %in% target$gss_code) %>%
    adjust_domestic_migration(target = target,
                              dom_in = trend_projection[['dom_in']],
                              dom_out = trend_projection[['dom_out']],
                              col_aggregation = c("year","gss_code"),
                              col_popn = "popn",
                              col_target = "target_popn")
  rbind(filter(step_7, !gss_code %in% target$gss_code))
  
  #9. Add components from step 6 to domestic from step 8 & start population
  step_9 <- construct_popn_from_components(start_population,
                                           births,
                                           deaths,
                                           trend_projection[['int_in']],
                                           int_out,
                                           adjusted_domestic[['dom_in']],
                                           adjusted_domestic[['dom_out']],
                                           upc = NULL)
  
  #10. Constrain total population
  constrained_popn <- constrain_to_hma(popn = step_9,
                                       constraint = constraints[['population_constraint']],
                                       hma_list = hma_list,
                                       col_aggregation = c("year","hma","sex","age"),
                                       col_popn = "popn",
                                       col_constraint = "popn")
  
  #11. Compare population from step 10 to population from step 9.
  #    Difference = domestic adjustment
  #    Adjust domestic
  final_domestic <- adjust_domestic_migration(popn = step_9, target = constrained_popn,
                                              dom_in = adjusted_domestic[['dom_in']],
                                              dom_out = adjusted_domestic[['dom_out']],
                                              col_aggregation = c("year","gss_code","sex","age"),
                                              col_popn = "popn", col_target = "popn")
  
  
  return(list(population = constrained_popn,
              births = births,
              death = deaths,
              int_in = trend_projection[['int_in']],
              int_out = int_out,
              dom_in = final_domestic[['dom_in']],
              dom_out = final_domestic[['dom_out']],
              ahs = "ahs",
              ahs_choice= "ahs_choice"))
}
