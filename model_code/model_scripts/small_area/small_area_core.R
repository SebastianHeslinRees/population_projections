#TODO: Description

start_population = curr_yr_popn
births = births
deaths = deaths
popn_constraint = curr_yr_popn_constraint
birth_constraint = curr_yr_birth_constraint
death_constraint = curr_yr_death_constraint
fertility_rates = curr_yr_fertility
mortality_rates = curr_yr_mortality
adults_per_dwelling = curr_yr_adults_per_dwelling
dwellings = curr_yr_dwellings


small_area_core <- function(start_population, births, deaths, communal_est_popn,
                            out_migration_rates, in_migration_characteristics,
                            popn_constraint, birth_constraint, death_constraint,
                            fertiltiy_rates, mortality_rates, projection_year){
  
  
  ####Age on####
  #TODO Where does the communal population get removed?
  aged_on_popn <- popn_age_on(start_population,
                              col_aggregation = c("year", "gss_code_small_area", "age", "sex"))
  
  ####Fertility####
  if(projection_year <= max(births$year)){
    curr_yr_births <- filter(births, year == projection_year) %>%
      group_by(year, gss_code_small_area) %>%
      summarise(births = sum(births)) %>%
      as.data.frame()
    
  } else {
    curr_yr_births <- apply_rate_to_population(popn = aged_on_popn,
                                               popn_rate = fertility_rates)
  }
  
  birth_ratio_m2f <- 105/205
  
  curr_yr_births <- left_join(curr_yr_births, ward_to_district, by="gss_code_small_area") %>%
    constrain_component(constraint=curr_yr_birth_constraint,
                        col_aggregation = "gss_code",
                        col_popn = "births") %>%
    mutate(male = births * birth_ratio_m2f,
           female = births * (1-birth_ratio_m2f)) %>%
    select(-births) %>%
    tidyr::pivot_longer(cols = c(male, female), names_to = "sex", values_to = "popn") %>%
    mutate(age = 0) %>%
    select(names(start_population)) %>%
    as.data.frame()
  
  popn_w_births <- rbind(aged_on_popn, curr_yr_births) %>%
    arrange(gss_code_small_area, sex, age)
  
  ####Mortality####
  if(projection_year <= max(deaths$year)){
    curr_yr_deaths <- filter(deaths, year == projection_year)
    
    age_groups <- unique(deaths$age_group)
    for(i in 1:length(age_groups)){
      mn <- substr(age_groups[i],1,2)
      mn <- case_when(mn == "1_" ~ 1,
                      mn == "5_" ~ 5,
                      TRUE ~ as.numeric(mn))
      
      mx <- case_when(mn == 0 ~ 0,
                      TRUE ~ mn+4)
      
      x[[i]] <- filter(curr_yr_deaths, age_group == age_groups[i]) %>%
        distribute_within_age_band(popn_2 = death_constraint,
                                   popn_1_col = "deaths",
                                   popn_2_col = "deaths",
                                   min_age = mn, max_age = mx,
                                   col_aggregation=c("gss_code","sex"))
    }
    
    curr_yr_deaths <- data.table::rbindlits(x)
    
    
  } else {
    curr_yr_deaths <- popn_w_births %>%
      # TODO apply_rate_to_population() instead
      left_join(curr_yr_mortality, by=c("gss_code","sex","age")) %>%
      mutate(deaths = rate*popn) %>%
      group_by(year, gss_code, gss_code_small_area) %>%
      summarise(deaths_from_rate = sum(deaths)) %>%
      as.data.frame()%>%
      calculate_scaling_factors(constraint = deaths,
                                col_aggregation = c("year","gss_code_small_area"),
                                col_popn = "deaths_from_rate",
                                col_constraint = "deaths",
                                rows_to_constrain = TRUE) %>%
      dtplyr::lazy_dt() %>%
      group_by(gss_code_small_area) %>%
      summarise(scaling = EnvStats::geoMean(scaling)) %>%
      as.data.frame() %>%
      left_join(ward_to_district, by="gss_code_small_area") %>%
      left_join(curr_yr_mortality, by="gss_code") %>%
      mutate(scaled_rate = rate*scaling) %>%
      left_join(popn_w_births, by = c("gss_code","gss_code_small_area","sex","age")) %>%
      mutate(scaled_deaths = popn * scaled_rate) %>%
      select(gss_code_small_area, sex, age, deaths = scaled_deaths)
  }
  
  #TODO Add age
  curr_yr_deaths <- left_join(curr_yr_deaths, ward_to_district, by="gss_code_small_area") %>%
    constrain_component(constraint = curr_yr_death_constraint,
                        col_aggregation = c("gss_code","sex"),
                        col_popn = "deaths") %>%
    select(gss_code_small_area, sex, deaths) %>%
    as.data.frame()
  
  #Age on population
  natural_change_popn <- left_join(popn_w_births, curr_yr_deaths, by=c("gss_code_small_area","sex","age")) %>%
    mutate(popn = popn - deaths) %>%
    select(-deaths)
  
  #Apply outmigration rates
  # out_migration <- apply_rate_to_population(popn = natural_change_popn,
  #                                           popn_rate = out_migration_rates,
  #                                           col_popn = "popn",
  #                                           col_rate = "out_migration_rate",
  #                                           col_aggregation = c("gss_code_small_area","sex","age"))
  
  out_migration <- left_join(natural_change_popn, out_migration_rates, by=c("gss_code_small_area","sex","age")) %>%
    mutate(out_migrants = out_migration_rate*popn) 
  
  popn_post_out_migration <- out_migration%>%
    mutate(popn = popn - out_migrants) %>%
    select(gss_code_small_area, sex, age, popn)
  
  #Compare the adult popn at this point to the target adults
  #target adults = adults_per_dwelling * dwellings
  #The difference is migration inflow
  target_adults <- left_join(adults_per_dwelling, dwellings, by=c("gss_code_small_area")) %>%
    mutate(target = units*adults_per_dwelling)
  
  inflow_total <- dtplyr::lazy_dt(popn_post_out_migration) %>%
    filter(age >= 18) %>%
    group_by(gss_code_small_area) %>%
    summarise(adults = sum(popn)) %>%
    as.data.frame() %>%
    left_join(target_adults, by=c("gss_code_small_area")) %>%
    mutate(inflow = adults - target) %>%
    select(year, gss_code_small_area, inflow)
  
  #apply in-migration characteristics rates to inflow
  # apply_rate_to_population(popn = inflow_total, popn_rate = in_migration_characteristics)
  in_migration <- left_join(in_migration_characteristics, inflow_total, by="gss_code_small_area") %>%
    mutate(in_migrants = final_dist * inflow)
  
  #Add in-migration
  #Add communal establishment popn back in
  unconstrined_popn <- left_join(popn_post_out_migration, in_migration, by = c("gss_code_small_area", "sex", "age")) %>%
    mutate(household_popn = popn + in_migrants) %>%
    select(-popn) %>%
    left_join(communal_est_popn, by = c("gss_code_small_area", "sex", "age")) %>%
    mutate(popn = household_popn + ce_popn)
  
  #constrain borough populations to match constraint
  constrained_popn <- left_join(unconstrined_popn, ward_to_district, by="gss_code_small_area") %>%
    as.data.frame() %>%
    constrain_component(constraint = curr_yr_popn_constraint,
                        col_popn = "popn",
                        col_aggregation = c("gss_code","sex","age"))
  
  final_popn <- select(constrained_popn, year, gss_code_small_area, sex, age, popn)
  
  final_migration <- rename(natural_change_popn, nat_chng = popn) %>%
    left_join(final_popn, by=c("gss_code_small_area","sex","age")) %>%
    mutate(migration = popn - nat_chng) %>%
    select(gss_code_area, sex, age, migration)
  
  return(list(population = final_popn,
              births = births,
              deaths = deaths,
              migration = final_migration))
}

