#TODO: Description

small_area_core <- function(start_population, births, deaths, communal_est_popn,
                            out_migration_rates, in_migration_characteristics,
                            popn_constraint, birth_constraint, death_constraint,
                            fertiltiy_rates, mortality_rates, projection_year){
  
  ####Age on####
  #TODO Where does the communal population get removed?
  aged_on_popn <- popn_age_on(start_population)
  
  ####Fertility####
  if(projection_year <= max(births$year)){
    births <- filter(births, year == projection_year)
  } else {
    births <- apply_rate_to_population(popn = aged_on_popn,
                                       popn_rate = fertility_rates)
  }
  births <- constrain_births()
  
  popn_w_births <- popn_age_on(start_population, births)
  
  ####Mortality####
  if(projection_year <= max(deaths$year)){
    deaths <- filter(deaths, year == projection_year)
  } else {
    deaths <- apply_rate_to_population(popn = popn_w_births,
                                       popn_rate = mortality_rates)
  }
  
  deaths <- constrain_component()
  
  #Age on population
  natural_change_popn <- start_populationn %>%
    construct_popn_from_components(addition_data = list(births),
                                   subtraction_data = list(deaths),
                                   col_aggregation = c("year","gss_code_small_area","sex","age"))
  
  #Apply outmigration rates
  out_migration <- apply_rate_to_population(popn = natural_change_popn,
                                            popn_rate = out_migration_rates)
  
  popn_post_out_migration <- left_join(natural_change_popn, out_migration) %>%
    mutate(popn = popn - out_mig)
  
  #Compare the adult popn at this point to the target adults
  #target adults = adults_per_dwelling * dwellings
  #The difference is migration inflow
  target_adults <- left_join(adults_per_dwelling, dwellings, by=c("year","gss_code_small_area")) %>%
    mutate(target = dwellings*adults_per_dwelling)
  
  inflow_total <- dtplyr::lazydt(popn_post_out_migration) %>%
    filter(age >= 18) %>%
    group_by(year, gss_code_small_area) %>%
    summarise(adults = sum(popn)) %>%
    as.data.frame() %>%
    left_join(target_adults, by=c("year","gss_code_small_area")) %>%
    mutate(inflow = adults - target) %>%
    select(year, gss_code_small_area, inflow)
  
  #apply in-migration characteristics rates to inflow
  in_migration <- apply_rate_to_population(popn = inflow_total,
                                           popn_rate = in_migration_characteristics)
  
  #Add in-migration
  #Add communal establishment popn back in
  unconstrined_popn <- left_join(popn_post_out_migration, in_migration) %>%
    mutate(household_popn = popn + in_mig) %>%
    select(-popn) %>%
    left_join(communal_est_popn) %>%
    mutate(popn = household_popn + ce_popn)
  
  #constrain borough populations to match constraint
  constrained_popn <- constrain_component(popn = unconstrined_popn,
                                          constraint = borough_popn_constraint)
  
  final_popn <- select(popn, year, gss_code_small_area, sex, age, popn)
  
  final_migration <- rename(natural_change_popn, nat_chng = popn) %>%
    left_join(final_popn, by=c("year","gss_code_small_area","sex","age")) %>%
    mutate(migration = popn - nat_chng) %>%
    select(year, gss_code_area, sex, age, migration)
  
  return(list(population = final_popn,
              births = births,
              deaths = deaths,
              migration = final_migration))
}

