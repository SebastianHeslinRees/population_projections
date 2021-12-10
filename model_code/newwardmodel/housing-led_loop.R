housing_led_core <- function(start_population, 
                             trend_projection,
                             communal_establishment_population,
                             household_rep_rates,
                             households,
                             projection_year,
                             ahs_cap_year,
                             ahs_cap,
                             ahs_method,
                             ldd_final_yr,
                             constrain_projection){
  
  col_agg <- c("year", "gss_code", "gss_code_ward", "sex", "age")
  nested_geog <- c("gss_code", "gss_code_ward")
  
  #browser()
  #1. GSS codes present in housing trajectory
  #TODO
  constrain_gss <- unique(intersect(start_population$gss_code_ward, households$gss_code))
  
  households <- filter(households, gss_code_ward %in% constrain_gss)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code_ward %in% constrain_gss)) 
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code_ward %in% constrain_gss))
  
  aged_on_population <- filter(start_population, gss_code_ward %in% constrain_gss) %>%
    popn_age_on(  col_aggregation = col_agg,
                  col_geog = nested_geog)
  
  initial_population <- trend_projection$population
  
  #-----------------------------------------------------------------------------
  
  household_population_sya <- left_join(initial_population, communal_establishment_population,
                                        by = c("gss_code_ward", "sex", "age")) %>% 
    mutate(household_popn = popn - ce_popn) 
  
  household_population <- household_population_sya %>% 
    group_by(gss_code_ward, year) %>% 
    summarise(household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame()
  
  #-----------------------------------------------------------------------------
  #browser()
  curr_yr_hhr_ahs <- household_population_sya %>% 
    mutate(age_group = case_when(age <= 24 ~ "0_24",
                                 age %in% 25:34 ~ "25_34",
                                 age %in% 35:49 ~ "35_49",
                                 age %in% 50:64 ~ "50_64",
                                 TRUE ~ "65_plus")) %>% 
    group_by(gss_code_ward, sex, age_group) %>% 
    summarise(household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    left_join(household_rep_rates, by = c("gss_code_ward", "sex", "age_group")) %>% 
    mutate(households = HRR * household_popn) %>% 
    group_by(gss_code_ward, year) %>% 
    summarise(households = sum(households), .groups = 'drop_last') %>% 
    data.frame() %>% 
    left_join(household_population, by = c("gss_code_ward", "year")) %>% 
    mutate(hhr = household_popn/households) %>% 
    select(gss_code_ward, year, hhr)
  
  #-----------------------------------------------------------------------------
  
  curr_yr_trend_ahs <- left_join(households, household_population, by=c("year","gss_code_ward")) %>%
    mutate(trend = household_popn/households) %>%
    select(-household_popn, -households)
  
  if(ahs_cap_year == projection_year){
    ahs_cap <- curr_yr_trend_ahs %>% rename(cap = trend) %>% select(-year)
  }
  
  #-----------------------------------------------------------------------------
  
  #TODO Temporary
  
  # selected_ahs <- left_join(curr_yr_hhr_ahs, curr_yr_trend_ahs, by = "gss_code_ward") %>% 
  #   left_join(ahs_cap, by = "gss_code_ward")
  
  #New floating method
  if(is.numeric(ahs_method)){
    set_ahs <- curr_yr_hhr_ahs %>%
      filter(gss_code_ward %in% constrain_gss) %>%
      left_join(curr_yr_trend_ahs, by = c("year", "gss_code_ward")) 
    
    if(is.null(ahs_cap)){
      set_ahs <- set_ahs %>%
        mutate(cap = trend)
    } else {
      set_ahs <- set_ahs %>%
        left_join(ahs_cap, by="gss_code_ward")
    }    
    
    set_ahs <- set_ahs %>%
      mutate(diff = cap - hhr,
             float = hhr + (diff*0)) %>%
      select(-diff) %>%
      mutate(diff = trend - float,
             ahs = float + (diff*ahs_method))
    
    ahs_choice <- select(set_ahs, year, gss_code_ward, hhr, cap, trend, float, ahs)
    ahs <- select(set_ahs, year, gss_code_ward, ahs)
    
  }
  
  
  #-----------------------------------------------------------------------------
  
  
  #browser()
  # selected_ahs <- left_join(ahs_cap, curr_yr_trend_ahs,
  #                           by = c("gss_code_ward")) %>% 
  #   mutate(ahs = ifelse(cap > trend, trend, cap),
  #          selection = ifelse(cap > trend, "trend", "cap")) 
  # 
  # ahs <- selected_ahs %>% 
  #   select(-trend, -cap, -selection)
  
  #-----------------------------------------------------------------------------
  
  #browser()
  target_population <- apply_rate_to_population(popn = households,
                                                rates = ahs,
                                                col_popn = "households",
                                                col_rate = "ahs",
                                                col_out = "target_popn",
                                                col_aggregation = c("year", "gss_code_ward")) %>% 
    mutate(target_popn = ifelse(is.na(target_popn), 0, target_popn),
           target_popn = ifelse(is.infinite(target_popn), 0, target_popn))
  
  adjusted_migration <- adjust_domestic_migration(household_population,
                                                  target = target_population,
                                                  dom_in = trend_projection$in_migration,
                                                  dom_out = trend_projection$out_migration,
                                                  col_aggregation = c("year","gss_code_ward"),
                                                  col_popn = "household_popn",
                                                  col_target = "target_popn",
                                                  col_dom_in = "inflow",
                                                  col_dom_out = "outflow")
  
  net_migration <- adjusted_migration$dom_out %>% 
    mutate(inflow = outflow *-1) %>% 
    select(-outflow) %>% 
    rbind(adjusted_migration$dom_in) %>% 
    group_by_at(col_agg) %>% 
    summarise(net_migration = sum(inflow), .groups = 'drop_last') %>% 
    data.frame()
  
  #browser()
  unconstrained_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(trend_projection$births,
                                                        adjusted_migration$dom_in),
                                   subtraction_data = list(trend_projection$deaths,
                                                           adjusted_migration$dom_out),
                                   col_aggregation = col_agg) %>%
    rbind(areas_with_no_housing_data$population) 
  
  return(list(population = unconstrained_population,
              births = trend_projection$births,
              deaths = trend_projection$deaths,
              out_migration = adjusted_migration$dom_out,
              in_migration = adjusted_migration$dom_in,
              net_migration = net_migration,
              ahs_cap = ahs_cap,
              ahs = ahs_choice))
}
