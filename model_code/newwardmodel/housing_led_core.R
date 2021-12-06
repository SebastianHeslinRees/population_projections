housing_led_core <- function(start_population, 
                             trend_projection,
                             #component_constraints = component_constraints,
                             #hma_constraint = curr_yr_hma_constraint,
                             communal_establishment_population,
                             #external_ahs = curr_yr_ahs,
                             households_1,
                             #households_2 = curr_yr_households_adjusted ,
                             #hma_list = hma_list,
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
  constrain_gss <- unique(intersect(start_population$gss_code_ward, households_1$gss_code))
  
  #trend_projection <- lapply(trend_projection, filter_to_LAs)
  households_1 <- filter(households_1, gss_code_ward %in% constrain_gss)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code_ward %in% constrain_gss)) 
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code_ward %in% constrain_gss))
  
  aged_on_population <- filter(start_population, gss_code_ward %in% constrain_gss) %>%
    popn_age_on(  col_aggregation = col_agg,
                  col_geog = nested_geog)
  
  births <- trend_projection$births
  deaths <- trend_projection$deaths
  in_migration <- trend_projection$in_migration
  out_migration <- trend_projection$out_migration
  initial_population <- trend_projection$population
  
  household_population <- left_join(initial_population, communal_establishment_population,
                                    by = c("gss_code_ward", "sex", "age")) %>% 
    mutate(household_popn = popn - ce_popn) %>% 
    group_by(gss_code_ward, year) %>% 
    summarise(household_popn = sum(household_popn),
              .groups = 'drop_last') %>% 
    data.frame()
  
  curr_yr_trend_ahs <- left_join(households_1, household_population, by=c("year","gss_code_ward")) %>%
    mutate(trend = household_popn/households) %>%
    select(-household_popn, -households)
  
  if(ahs_cap_year == projection_year){
    ahs_cap <- curr_yr_trend_ahs %>% rename(cap = trend) %>% select(-year)
  }

  #TODO Temporary
  ahs <- left_join(ahs_cap, curr_yr_trend_ahs,
                   by = c("gss_code_ward")) %>% 
    mutate(ahs = ifelse(cap > trend, trend, cap)) %>% 
    select(-trend, -cap)
  
  #browser()
  target_population <- apply_rate_to_population(popn = households_1,
                                                rates = ahs,
                                                col_popn = "households",
                                                col_rate = "ahs",
                                                col_out = "target_popn",
                                                col_aggregation = c("year", "gss_code_ward")) %>% 
    mutate(target_popn = ifelse(is.na(target_popn),0,target_popn))
  
  adjusted_migration <- adjust_domestic_migration(household_population,
                                                  target = target_population,
                                                  dom_in = trend_projection[['in_migration']],
                                                  dom_out = trend_projection[['out_migration']],
                                                  col_aggregation = c("year","gss_code_ward"),
                                                  col_popn = "household_popn",
                                                  col_target = "target_popn",
                                                  col_dom_in = "inflow",
                                                  col_dom_out = "outflow")
  #browser()
  unconstrained_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(births,
                                                        adjusted_migration[['dom_in']]),
                                   subtraction_data = list(deaths,
                                                           adjusted_migration[['dom_out']]),
                                   col_aggregation = col_agg) %>%
  rbind(areas_with_no_housing_data[['population']]) 
  
  
  return(list(population = unconstrained_population,
              ahs_cap = ahs_cap))
}
