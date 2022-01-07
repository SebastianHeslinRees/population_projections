housing_led_core <- function(start_population, 
                             trend_projection,
                             communal_establishment_population,
                             household_rep_rates,
                             households,
                             projection_year,
                             ahs_mix,
                             ldd_final_yr,
                             constrain_projection){
  
  col_agg <- c("year", "gss_code", "gss_code_ward", "sex", "age")
  nested_geog <- c("gss_code", "gss_code_ward")
  
  #if(start_population$year >= 2038){browser()}
  #browser()
  #1. GSS codes present in housing trajectory
  #TODO
  constrain_gss <- unique(intersect(start_population$gss_code_ward, households$gss_code))
  
  households <- filter(households, gss_code_ward %in% constrain_gss)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code_ward %in% constrain_gss)) 
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code_ward %in% constrain_gss))
  
  aged_on_population <- filter(start_population, gss_code_ward %in% constrain_gss) %>%
    popn_age_on(col_aggregation = col_agg,
                col_geog = nested_geog)
  
  #TODO The check_negative_values here should, I think, be part of the trend loop
  initial_population <- trend_projection$population %>% 
    check_negative_values("popn")
  
  #-----------------------------------------------------------------------------
  #TODO
  #Need to make sure that the household population that is put into the internal hh model
  #doesn't have negative population. I'm doing this here. But arguable it could be done when the
  #sya is grouped into age groups. 
  
  #browser()
  
  household_population_sya <- left_join(initial_population, communal_establishment_population,
                                        by = c("gss_code_ward", "sex", "age")) %>% 
    mutate(household_popn = popn - ce_popn) %>% 
    check_negative_values("household_popn")
  
  households_detail <- household_population_sya %>% 
    group_by(gss_code_ward, year) %>% 
    summarise(trend_population = sum(popn),
              ce_population = sum(ce_popn),
              household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    left_join(households, by=c("year","gss_code_ward")) %>% 
    rename(input_households = households)
  
  household_population <- households_detail %>% 
    select(gss_code_ward, year, household_popn) 
  
  #-----------------------------------------------------------------------------
  #browser()
  curr_yr_hhr_households <- household_population_sya %>% 
    mutate(age_group = case_when(age <= 17 ~ "0_17",
                                 age %in% 18:24 ~ "18_24",
                                 age %in% 25:34 ~ "25_34",
                                 age %in% 35:49 ~ "35_49",
                                 age %in% 50:64 ~ "50_64",
                                 TRUE ~ "65_plus")) %>% 
    group_by(gss_code_ward, sex, age_group) %>% 
    summarise(household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    left_join(household_rep_rates, by = c("gss_code_ward", "sex", "age_group")) %>% 
    mutate(households = hh_rep_rate * household_popn) %>% 
    group_by(gss_code_ward, year) %>% 
    summarise(households = sum(households), .groups = 'drop_last') %>% 
    data.frame() 
  
  curr_yr_hhr_ahs <- curr_yr_hhr_households %>% 
    left_join(household_population, by = c("gss_code_ward", "year")) %>% 
    mutate(hhr_ahs = household_popn/households) %>% 
    select(gss_code_ward, year, hhr_ahs)
  
  #-----------------------------------------------------------------------------
  
  curr_yr_trend_ahs <- left_join(households, household_population, by=c("year","gss_code_ward")) %>%
    mutate(trend_ahs = household_popn/households)
  
  #-----------------------------------------------------------------------------
  
  # floating method
  ahs_choice <- curr_yr_hhr_ahs %>%
    filter(gss_code_ward %in% constrain_gss) %>%
    left_join(curr_yr_trend_ahs, by = c("year", "gss_code_ward")) %>%
    select(-household_popn, -households) %>% 
    mutate(diff = trend_ahs - hhr_ahs,
           ahs = hhr_ahs + (diff*ahs_mix)) %>%
    select(year, gss_code_ward, hhr_ahs, trend_ahs, ahs)
  
  ahs <- select(ahs_choice, year, gss_code_ward, ahs)
  
  #-----------------------------------------------------------------------------
  
  households_detail <- left_join(households_detail, curr_yr_hhr_households,
                                 by = c("gss_code_ward", "year")) %>% 
    rename(HHR_households = households) %>% 
    left_join(ahs_choice, by=c("year","gss_code_ward"))
  
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
  
  #-----------------------------------------------------------------------------
  #browser()
  
  unconstrained_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(trend_projection$births,
                                                        adjusted_migration$dom_in),
                                   subtraction_data = list(trend_projection$deaths,
                                                           adjusted_migration$dom_out),
                                   col_aggregation = col_agg) %>%
    rbind(areas_with_no_housing_data$population) 
  
  #-----------------------------------------------------------------------------
  
  #Guard against negative populations
  negatives <- filter(unconstrained_population, popn < 0)
  unconstrained_population <- unconstrained_population %>% check_negative_values("popn")
  
  #fix negatives by increasing inflow
  if(nrow(negatives)!=0){
    adjusted_migration$dom_in <- adjusted_migration$dom_in %>% 
      left_join(negatives, by = col_agg) %>% 
      mutate(inflow = ifelse(is.na(popn), inflow, inflow-popn)) %>% 
      select(-popn)
  }
  
  #-----------------------------------------------------------------------------
  
  net_migration <- adjusted_migration$dom_out %>% 
    mutate(net_migration = outflow *-1) %>% 
    select(-outflow) %>% 
    rbind(adjusted_migration$dom_in %>% 
            rename(net_migration = inflow)) %>% 
    group_by_at(col_agg) %>% 
    summarise(net_migration = sum(net_migration), .groups = 'drop_last') %>% 
    data.frame()
  
  #-----------------------------------------------------------------------------
  
  components <- bind_rows(mutate(unconstrained_population, comp = 'popn'),
                          rename(trend_projection$births, popn = births) %>%  mutate(comp = 'births'),
                          rename(trend_projection$deaths, popn = deaths) %>%  mutate(comp = 'deaths'),
                          rename(adjusted_migration$dom_in, popn = inflow) %>%  mutate(comp = 'inflow'),
                          rename(adjusted_migration$dom_out, popn = outflow) %>%  mutate(comp = 'outflow'),
                          rename(net_migration, popn = net_migration) %>%  mutate(comp = 'netflow')) %>%
    select(-age, -sex) %>% 
    group_by(gss_code, gss_code_ward, year, comp) %>% 
    summarise(popn = sum(popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    tidyr::pivot_wider(names_from = 'comp', values_from = 'popn')
  
  #-----------------------------------------------------------------------------
  
  return(list(population = unconstrained_population,
              births = trend_projection$births,
              deaths = trend_projection$deaths,
              out_migration = adjusted_migration$dom_out,
              in_migration = adjusted_migration$dom_in,
              net_migration = net_migration,
              household_population_sya = household_population_sya,
              ahs = ahs_choice,
              households_detail = households_detail,
              components = components
  ))
}
