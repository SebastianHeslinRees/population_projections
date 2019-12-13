housing_led_core <- function(start_population,
                             fertility_rates,
                             mortality_rates,
                             int_out_flows_rates,
                             int_in_flows,
                             domestic_rates,
                             int_out_method,
                             npp_constraints = NULL,
                             upc = NULL,
                             component_constraints,
                             hma_constraint,
                             communal_establishment_population,
                             average_household_size,
                             household_data,
                             ldd_households,
                             hma_list,
                             projection_year,
                             ahs_cap_year,
                             ahs_cap,
                             ldd_max_yr){
  
  #GSS codes present in housing trajectory
  constrain_gss <- unique(household_data$gss_code)
  
  #1. Run the trend model for one year
  trend_projection <- trend_core(start_population,
                                 fertility_rates, mortality_rates,
                                 int_out_flows_rates, int_in_flows,
                                 domestic_rates,
                                 int_out_method = int_out_method,
                                 constraints = npp_constraints,
                                 upc = upc,
                                 projection_year)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code %in% constrain_gss)) 
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code %in% constrain_gss))
  aged_on_population <- filter(start_population, gss_code %in% constrain_gss) %>%
    popn_age_on()
  
  #2. Constrain births, deaths & international
  #So that totals match at the borough level
  
  births <- trend_projection[['births']] %>%
    constrain_component(
      constraint = component_constraints[['birth_constraint']],
      col_aggregation = c("year","gss_code"),
      col_popn = "births",
      col_constraint = "births",
      rows_to_constrain = trend_projection$births$gss_code %in% borough_constraint_gss)

  
  deaths <- trend_projection[['deaths']] %>%
    constrain_component(constraint = constraints[['death_constraint']],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "deaths",
                        col_constraint = "deaths",
                        rows_to_constrain = trend_projection$deaths$gss_code %in% borough_constraint_gss)
  
  int_out <- trend_projection[['int_out']] %>%
    constrain_component(constraint = constraints[['international_out_constraint']],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "int_out",
                        col_constraint = "int_out",
                        rows_to_constrain = trend_projection$int_out$gss_code %in% borough_constraint_gss)

  #3. Calculate household population
  household_population <- dtplyr::lazy_dt(trend_projection[['population']]) %>%
    group_by(gss_code, year) %>%
    summarise(popn = sum(popn)) %>%
    left_join(communal_establishment_population, by=c("gss_code","year")) %>%
    mutate(household_popn = popn - communal_est_popn) %>%
    as.data.frame() %>%
    check_negative_values("household_popn") %>%
    select(-popn, -communal_est_popn)
  
  #4. Calculate trend AHS
  #Population divided by household_data
  curr_yr_trend_ahs <- left_join(household_data, household_population, by=c("year","gss_code")) %>%
    mutate(trend = household_popn/households) %>%
    select(-household_popn, -households)
  
  #Initially the ahs_cap is passed as NULL
  #In the year where the cap is set the variable is changed to be a dataframe of ahs values
  #The core outputs the ahs_cap variable and the next year's ahs_cap is set as that output
  #in the control. So it will be NULL before the cap year and then the same dataframe following
  #the cap year
  
  if(ahs_cap_year == projection_year){
    ahs_cap <- curr_yr_trend_ahs %>% rename(cap = trend) %>% select(-year)
  }
  
  if(projection_year <= ldd_max_yr){
    if(is.null(ahs_cap)){
      #before the max LDD data year always select the trend
      average_household_size <- select(curr_yr_trend_ahs, year, gss_code, ahs = trend) %>%
        mutate(ahs, ahs_choice = "trend")
    } else {
      average_household_size <- ahs_cap %>%
        left_join(curr_yr_trend_ahs, by = c("year","gss_code")) %>%
        mutate(ahs_choice = case_when(trend > cap ~ "cap",
                                      trend <= cap ~ "trend"),
               ahs = case_when(trend > cap ~ cap,
                               trend <= cap ~ trend))
    }
  } else {
    if(is.null(ahs_cap)){
      #after LDD but before cap is set select the higher of the trend or input ahs
      average_household_size <- average_household_size %>%
        filter(gss_code %in% constrain_gss) %>%
        rename(input = ahs) %>%
        left_join(curr_yr_trend_ahs, by = c("year","gss_code")) %>%
        mutate(ahs_choice = case_when(trend > input ~ "trend",
                                      input > trend ~ "input"),
               ahs = case_when(trend > input ~ trend,
                               input > trend ~ input))
    } else {
      average_household_size <- average_household_size %>%
        filter(gss_code %in% constrain_gss) %>%
        rename(input = ahs) %>%
        left_join(curr_yr_trend_ahs, by = c("year","gss_code")) %>%
        left_join(ahs_cap, by = "gss_code") %>%
        mutate(ahs_choice = case_when(cap < trend ~ "cap",
                                      cap < input ~ "cap",
                                      trend > input ~ "trend",
                                      input > trend ~ "input",
                                      TRUE ~ "trend"),
               ahs = case_when(cap < trend ~ cap,
                               cap < input ~ cap,
                               trend > input ~ trend,
                               input > trend ~ input,
                               TRUE ~ trend))
    }
  }
  
  ahs_choice <- select(average_household_size, year, gss_code, ahs_choice)
  ahs <- select(average_household_size, year, gss_code, ahs)
  
  #6. Target population
  #Probably apply_rate_to_population
  target <- apply_rate_to_population(household_data, average_household_size,
                                     col_popn = "households",
                                     col_rate = "ahs",
                                     col_out = "target_popn",
                                     col_aggregation = c("year","gss_code"),
                                     pop1_is_subset = TRUE)
  
  #7. Add components from step 2 to domestic from step 1 & start population
  step_7 <- construct_popn_from_components(start_population = aged_on_population,
                                           addition_data = list(births,
                                                                trend_projection[['int_in']],
                                                                trend_projection[['dom_in']]),
                                           subtraction_data = list(deaths,
                                                                   int_out,
                                                                   trend_projection[['dom_out']])) %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  #8. Compare population from step 7 to target from step 5.
  #   Difference = domestic adjustment
  #   Adjust domestic
  adjusted_domestic <- adjust_domestic_migration(popn = step_7,
                                                 target = target,
                                                 dom_in = trend_projection[['dom_in']],
                                                 dom_out = trend_projection[['dom_out']],
                                                 col_aggregation = c("year","gss_code"),
                                                 col_popn = "popn",
                                                 col_target = "target_popn")
  
  #9. Add components from step 6 to domestic from step 8 & start population
  #Join the non-adjusted population data back to the adjusted
  step_9 <- construct_popn_from_components(start_population = aged_on_population,
                                           addition_data = list(births,
                                                                trend_projection[['int_in']],
                                                                adjusted_domestic[['dom_in']]),
                                           subtraction_data = list(deaths,
                                                                   int_out,
                                                                   adjusted_domestic[['dom_out']])) %>%
    rbind(areas_with_no_housing_data[['population']]) 
  
  #10. Constrain total population
  constrained_popn <- constrain_to_hma(popn = step_9,
                                       constraint = hma_constraint,
                                       hma_list = hma_list,
                                       col_aggregation = c("year","hma","sex","age"),
                                       col_popn = "popn",
                                       col_constraint = "popn")
  
  #Join the non-adjusted components data back to the adjusted
  births <- rbind(births, areas_with_no_housing_data[['births']])
  deaths <- rbind(deaths, areas_with_no_housing_data[['deaths']])
  int_out <- rbind(int_out, areas_with_no_housing_data[['int_out']])
  dom_in <- rbind(adjusted_domestic[['dom_in']], areas_with_no_housing_data[['dom_in']])
  dom_out <- rbind(adjusted_domestic[['dom_out']], areas_with_no_housing_data[['dom_out']])
  
  #11. Compare population from step 10 to population from step 9.
  #    Difference = domestic adjustment
  #    Adjust domestic
  final_domestic <- adjust_domestic_migration(popn = step_9,
                                              target = constrained_popn,
                                              dom_in = dom_in,
                                              dom_out = dom_out,
                                              col_aggregation = c("year","gss_code","sex","age"),
                                              col_popn = "popn",
                                              col_target = "popn")
  
  #TODO Look into this more
  #More validation needed here?
  constrained_popn <- check_negative_values(constrained_popn, "popn")
  
  return(list(population = constrained_popn,
              births = births,
              death = deaths,
              int_in = trend_projection[['int_in']],
              int_out = int_out,
              dom_in = final_domestic[['dom_in']],
              dom_out = final_domestic[['dom_out']],
              ahs = ahs,
              ahs_choice = ahs_choice,
              ahs_cap = ahs_cap))
}
