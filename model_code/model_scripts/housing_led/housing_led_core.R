housing_led_core <- function(start_population,
                             trend_projection,
                             component_constraints,
                             hma_constraint,
                             communal_establishment_population,
                             external_ahs,
                             households_1,
                             households_2,
                             hma_list,
                             projection_year,
                             ahs_cap_year,
                             ahs_cap,
                             ldd_max_yr,
                             constrain_projection){
  
  #1. GSS codes present in housing trajectory
  constrain_gss <- unique(households_1$gss_code)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code %in% constrain_gss)) 
  trend_projection_national <- trend_projection
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code %in% constrain_gss))
  aged_on_population <- filter(start_population, gss_code %in% constrain_gss) %>%
    popn_age_on()
  
  #2. Constrain births, deaths & international
  #So that totals match at the borough level
  births <- component_constraints[['birth_constraint']] %>%
    filter(gss_code %in% constrain_gss, year == projection_year) %>%
    mutate(female = births*(100/205),
           male = births*(105/205),
           age = 0) %>%
    select(-births) %>%
    tidyr::pivot_longer(c("female","male"), names_to = "sex", values_to="births")
  
  deaths <- trend_projection[['deaths']] %>%
    constrain_component(constraint = component_constraints[['death_constraint']],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "deaths",
                        col_constraint = "deaths",
                        rows_to_constrain = trend_projection$deaths$gss_code %in% constrain_gss)
  
  int_out <- trend_projection[['int_out']] %>%
    constrain_component(constraint = component_constraints[['international_out_constraint']],
                        col_aggregation = c("year","gss_code"),
                        col_popn = "int_out",
                        col_constraint = "int_out",
                        rows_to_constrain = trend_projection$int_out$gss_code %in% constrain_gss)
  
  initial_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(births,
                                                        trend_projection[['int_in']],
                                                        trend_projection[['dom_in']]),
                                   subtraction_data = list(deaths,
                                                           int_out,
                                                           trend_projection[['dom_out']])) %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  #3. Calculate household population
  household_population <- dtplyr::lazy_dt(trend_projection[['population']]) %>%
    group_by(gss_code, year) %>%
    summarise(popn = sum(popn)) %>%
    left_join(communal_establishment_population, by=c("gss_code","year")) %>%
    mutate(household_popn = popn - communal_est_popn) %>%
    as.data.frame() %>%
    validate_population(col_data = "household_popn", col_aggregation = c("year","gss_code")) %>%
    select(-popn, -communal_est_popn)
  
  #4. Calculate trend AHS
  #Population divided by households_1
  #2011 dw2hh ratio
  curr_yr_trend_ahs <- left_join(households_1, household_population, by=c("year","gss_code")) %>%
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
  
  average_household_size <- external_ahs %>%
    filter(gss_code %in% constrain_gss) %>%
    rename(external = ahs) %>%
    left_join(curr_yr_trend_ahs, by = c("year","gss_code"))
  
  if(projection_year <= ldd_max_yr){
    if(is.null(ahs_cap)){
      #before the max LDD data year always select the trend
      average_household_size <- average_household_size %>%
        mutate(cap = NA, ahs = trend, ahs_choice = "trend")
    } else {
      average_household_size <- average_household_size %>%
        left_join(ahs_cap, by = "gss_code") %>%
        mutate(ahs_choice = case_when(trend > cap ~ "cap",
                                      trend <= cap ~ "trend"),
               ahs = case_when(trend > cap ~ cap,
                               trend <= cap ~ trend))
    }
  } else {
    if(is.null(ahs_cap)){
      #after LDD but before cap is set select the higher of the trend or external ahs
      average_household_size <- average_household_size %>%
        mutate(cap = NA,
               ahs_choice = case_when(trend > external ~ "trend",
                                      external > trend ~ "external"),
               ahs = case_when(trend > external ~ trend,
                               external > trend ~ external))
    } else {
      average_household_size <- external_ahs %>%
        filter(gss_code %in% constrain_gss) %>%
        rename(external = ahs) %>%
        left_join(curr_yr_trend_ahs, by = c("year","gss_code")) %>%
        left_join(ahs_cap, by = "gss_code") %>%
        mutate(ahs_choice = case_when(cap < trend ~ "cap",
                                      cap < external ~ "cap",
                                      trend > external ~ "trend",
                                      external > trend ~ "external",
                                      TRUE ~ "trend"),
               ahs = case_when(cap < trend ~ cap,
                               cap < external ~ cap,
                               trend > external ~ trend,
                               external > trend ~ external,
                               TRUE ~ trend))
    }
  }
  
  ahs_choice <- select(average_household_size, year, gss_code, external, cap, trend, ahs_choice)
  ahs <- select(average_household_size, year, gss_code, ahs)
  
  #6. Target population
  #Probably apply_rate_to_population
  #dw2hh 2018
  target_population <- apply_rate_to_population(households_2, average_household_size,
                                                col_popn = "households",
                                                col_rate = "ahs",
                                                col_out = "target_popn",
                                                col_aggregation = c("year","gss_code"),
                                                pop1_is_subset = TRUE)
  
  #7. This point is intentionaly left blank
  
  
  #8. Compare population from step 3 to target from step 5.
  #   Difference = domestic adjustment
  #   Adjust domestic
  adjusted_domestic_migration <- adjust_domestic_migration(popn = household_population,
                                                           target = target_population,
                                                           dom_in = trend_projection[['dom_in']],
                                                           dom_out = trend_projection[['dom_out']],
                                                           col_aggregation = c("year","gss_code"),
                                                           col_popn = "household_popn",
                                                           col_target = "target_popn")
  
  out_adjusted_dom <- left_join(adjusted_domestic_migration[["dom_in"]],
                                adjusted_domestic_migration[["dom_out"]],
                                by=c("year","gss_code","age","sex")) %>%
    transmute(year, gss_code, age, sex, dom_net = dom_in-dom_out) %>%
    left_join(trend_projection[["dom_in"]], by = c("year","gss_code","age","sex")) %>%
    left_join(trend_projection[["dom_out"]], by = c("year","gss_code","age","sex")) %>%
    transmute(year, gss_code, age, sex, adjustment = dom_net - dom_in + dom_out)
  
  #9. Add components from step 6 to domestic from step 8 & start population
  #Join the non-adjusted population data back to the adjusted
  unconstrained_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(births,
                                                        trend_projection[['int_in']],
                                                        adjusted_domestic_migration[['dom_in']]),
                                   subtraction_data = list(deaths,
                                                           int_out,
                                                           adjusted_domestic_migration[['dom_out']])) %>%
    rbind(areas_with_no_housing_data[['population']]) 
  
  #Join the non-adjusted components data back to the adjusted
  births <- rbind(births, areas_with_no_housing_data[['births']])
  deaths <- rbind(deaths, areas_with_no_housing_data[['deaths']])
  int_out <- rbind(int_out, areas_with_no_housing_data[['int_out']])
  dom_in <- rbind(adjusted_domestic_migration[['dom_in']], areas_with_no_housing_data[['dom_in']])
  dom_out <- rbind(adjusted_domestic_migration[['dom_out']], areas_with_no_housing_data[['dom_out']])
  
  if(constrain_projection){
    #10. Constrain total population
    constrained_population <- constrain_to_hma(popn =  unconstrained_population,
                                               constraint = hma_constraint,
                                               hma_list = hma_list,
                                               col_aggregation = c("year","hma","sex","age"),
                                               col_popn = "popn",
                                               col_constraint = "popn")
    
    #11. Compare population from step 10 to population from step 9.
    #    Difference = domestic adjustment
    #    Adjust domestic
    final_domestic_migration <- adjust_domestic_migration(popn =  unconstrained_population,
                                                          target = constrained_population,
                                                          dom_in = dom_in,
                                                          dom_out = dom_out,
                                                          col_aggregation = c("year","gss_code","sex","age"),
                                                          col_popn = "popn",
                                                          col_target = "popn",
                                                          rows_to_constrain =  unconstrained_population$gss_code %in% hma_list$gss_code)
    dom_in <- final_domestic_migration[['dom_in']]
    dom_out <- final_domestic_migration[['dom_out']]
    
    #TODO More validation needed here?
    output_population <- check_negative_values(constrained_population, "popn")
    
  } else {
    
    output_population <- check_negative_values(unconstrained_population, "popn")
    
  }
  
  return(list(population = output_population,
              births = births,
              deaths = deaths,
              int_in = trend_projection_national[['int_in']],
              int_out = int_out,
              dom_in = dom_in,
              dom_out = dom_out,
              ahs = ahs,
              ahs_choice = ahs_choice,
              ahs_cap = ahs_cap,
              household_population = household_population,
              adjusted_domestic_migration = out_adjusted_dom,
              unconstrained_population =  unconstrained_population))
}
