apply_household_rates <- function(household_population, household_rates, rates_col, hh_pop_col) {
  
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

