housing_led_core <- function(start_population, 
                             trend_projection,
                             communal_establishment_population,
                             #external_ahs = curr_yr_ahs,
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
  
  household_population <- left_join(initial_population, communal_establishment_population,
                                    by = c("gss_code_ward", "sex", "age")) %>% 
    mutate(household_popn = popn - ce_popn) %>% 
    group_by(gss_code_ward, year) %>% 
    summarise(household_popn = sum(household_popn),
              .groups = 'drop_last') %>% 
    data.frame()
  
  curr_yr_trend_ahs <- left_join(households, household_population, by=c("year","gss_code_ward")) %>%
    mutate(trend = household_popn/households) %>%
    select(-household_popn, -households)
  
  if(ahs_cap_year == projection_year){
    ahs_cap <- curr_yr_trend_ahs %>% rename(cap = trend) %>% select(-year)
  }

  #-----------------------------------------------------------------------------
  #TODO Temporary
  
  #browser()
  selected_ahs <- left_join(ahs_cap, curr_yr_trend_ahs,
                   by = c("gss_code_ward")) %>% 
    mutate(ahs = ifelse(cap > trend, trend, cap),
           selection = ifelse(cap > trend, "trend", "cap")) 
  
  ahs <- selected_ahs %>% 
    select(-trend, -cap, -selection)
  
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
              selected_ahs = selected_ahs))
}
