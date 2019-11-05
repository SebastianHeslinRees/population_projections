apply_household_rates <- function(household_population, household_rates, rates_col, hh_pop_col) {
  
  household_population <- filter(household_population, year %in% household_rates$year)
  
  households <- left_join(household_population, household_rates, by = c("gss_code", "sex", "year", "age_group")) %>%
    rename(rate = rates_col,
           popn = hh_pop_col) %>%
    mutate(households = popn*rate) %>%
    select(-popn, -rate)
  
  return(households)
  
}

#-----------------------------------------------------

population_into_age_groups <- function(population, age_groups, labels, popn_col){
  
  population <- population %>%
    mutate(age_group = cut(age,
                           breaks = age_groups,
                           include.lowest = T,
                           labels = labels)) %>%
    mutate(age_group = as.character(age_group)) %>%
    select(-age)
  
  cols <- names(population)[!names(population) %in% popn_col]
  
  population <- population %>%
    group_by_at(cols) %>%
    summarise_all(.funs=list(sum)) %>%
    ungroup()
  
  return(population)
  
}

extend_data <- function(data, max_projection_year){
  
  max_data_year <- max(data$year)
  
  if(max_data_year < max_projection_year){
    
    final_rates <- filter(data, year==max_data_year)
    extra_years <- vector("list", max_projection_year)
    
    for(y in (max_data_year+1):max_projection_year){
      
      extra_years[[y]] <- final_rates %>%
        mutate(year = y)
      
    }
    
    extra_years <- data.table::rbindlist(extra_years) %>%
      as.data.frame()
    
    return(rbind(data, extra_years))
    
  }
  
  return(data)
}

#-------------------------------------------------

household_summary <- function(households, household_population, communal_establishment, col_aggregation){
  
  # households <- stage_2[[5]]
  # household_population <- stage_2[[3]]
  # communal_establishment <- stage_2[[4]]
  # col_aggregation <- c("gss_code","year","age_group")
  
  summary_tbl <- left_join(households, household_population,
                           by=col_aggregation) %>%
    left_join(communal_establishment,
              by=col_aggregation) %>%
    mutate(total_popn = household_popn + ce_pop) %>%
    select(gss_code, year, households, household_popn, ce_pop, total_popn) %>%
    data.frame() %>%
    group_by(gss_code, year) %>%
    summarise_all(list(sum)) %>%
    ungroup() %>%
    mutate(ahs = household_popn / households) %>%
    mutate(ahs = round(ahs, 3),
           total_popn = round(total_popn, 0),
           household_popn = round(household_popn, 0),
           ce_popn = round(ce_pop, 0),
           households = round(households, 0)) %>%
    select(gss_code, year, total_popn, ce_popn = ce_pop, household_popn, households, ahs)
  
  return(summary_tbl)
  
}
