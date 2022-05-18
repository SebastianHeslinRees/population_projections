#' Project from a starting population using a housing-led methodology to
#' produce an output population and components 
#'
#' @param start_population A dataframe. The starting population
#' @param trend_projection A list. The output from the small area trend model for
#'  the year \code{projection_year}
#' @param communal_establishment_population A dataframe. The communal establishment
#'  population
#' @param household_rep_rates A dataframe. Pre-calculated HHRs for model areas
#' @param households A dataframe. An input total household figure for model areas
#' @param projection_year Numeric. The year being projected
#' @param ahs_mix Numeric. A number between 0 and 1 defining the mix of AHS
#'  from the HHR household model and the implied trend AHS
#' @param hhr_ahs_uplift Numeric or NULL. The difference between the input AHS
#'  and the trend AHS in the first year of the projection.
#' @param constraint_list A list of constraints information and data for use
#'  in the model loop.
#' @param n_cores Numeric. Number of cores
#' @param lookup Dataframe. area_code to area_name (cols must be named like this)
#' 
#' @return A list of projected components
#' 
#' @import popmodules
#' @import dplyr
#' @import tidyr
#' @importFrom dtplyr lazy_dt
#' 
#' @export

housing_led_core <- function(start_population, 
                             trend_projection,
                             communal_establishment_population,
                             household_rep_rates,
                             households,
                             projection_year,
                             ahs_mix,
                             hhr_ahs_uplift,
                             constraint_list,
                             n_cores, lookup){
  
  col_agg <- c("year", "gss_code", "area_code", "sex", "age")
  nested_geog <- c("gss_code", "area_code")
  browser()
  # 1 --------------------------------------------------------------------------
  
  #Prep input data
  
  constrain_gss <- unique(intersect(start_population$area_code, households$gss_code))
  
  households <- filter(households, area_code %in% constrain_gss)
  start_population <- filter(start_population, area_code %in% constrain_gss)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$area_code %in% constrain_gss)) 
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$area_code %in% constrain_gss))
  
  aged_on_population <- start_population %>%
    popn_age_on(col_aggregation = col_agg,
                col_geog = nested_geog)
  
  # 2 --------------------------------------------------------------------------
  
  # The implied household population from the trend model
  
  trend_hh_population <- .remove_ce_popn(trend_projection$population,
                                         communal_establishment_population)
  
  sum_trend_hh_population <- trend_hh_population %>% 
    group_by(gss_code, area_code, year) %>% 
    summarise(household_popn = sum(household_popn)) %>% 
    data.frame()
  
  # 3 --------------------------------------------------------------------------
  
  # Run the ward implementation of the ONS HH model
  
  curr_yr_hhr_households <- trend_hh_population %>% 
    mutate(age_group = case_when(age <= 17 ~ "0_17",
                                 age %in% 18:24 ~ "18_24",
                                 age %in% 25:34 ~ "25_34",
                                 age %in% 35:49 ~ "35_49",
                                 age %in% 50:64 ~ "50_64",
                                 TRUE ~ "65_plus")) %>% 
    lazy_dt() %>% 
    group_by(gss_code, area_code, sex, age_group) %>% 
    summarise(household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    lazy_dt() %>% 
    left_join(household_rep_rates, by = c("area_code", "sex", "age_group")) %>% 
    mutate(households = hh_rep_rate * household_popn) %>% 
    group_by(gss_code, area_code, year) %>% 
    summarise(households = sum(households), .groups = 'drop_last') %>% 
    data.frame() 
  
  curr_yr_hhr_ahs <- curr_yr_hhr_households %>% 
    left_join(sum_trend_hh_population, by = c("gss_code", "area_code", "year")) %>% 
    mutate(hhr_ahs = household_popn/households) %>% 
    select(gss_code, area_code, year, hhr_ahs)
  
  # 4 --------------------------------------------------------------------------
  
  # Calculate the implied AHS from the trend input population and the input trajectory
  
  curr_yr_trend_ahs <- left_join(households, sum_trend_hh_population, by=c("year", "area_code")) %>%
    mutate(trend_ahs = household_popn/households)
  
  # 5 --------------------------------------------------------------------------
  
  # Shift the HHR AHS up/down to avoid jump-off issues
  
  if(is.null(hhr_ahs_uplift)){
    hhr_ahs_uplift <- left_join(curr_yr_hhr_ahs, curr_yr_trend_ahs,
                                by = c("gss_code", "area_code", "year")) %>% 
      mutate(hhr_diff = trend_ahs - hhr_ahs) %>%
      select(area_code, hhr_diff)
  }
  
  curr_yr_hhr_ahs <- curr_yr_hhr_ahs %>% 
    left_join(hhr_ahs_uplift, by = "area_code") %>% 
    mutate(hhr_ahs = hhr_ahs + hhr_diff) %>% 
    select(gss_code, area_code, year, hhr_ahs)
  
  # 6 --------------------------------------------------------------------------
  
  # Created a blended AHS using the 'floating' method
  
  ahs_choice <- curr_yr_hhr_ahs %>%
    filter(area_code %in% constrain_gss) %>%
    left_join(curr_yr_trend_ahs, by = c("year", "gss_code", "area_code")) %>%
    select(-household_popn, -households) %>% 
    mutate(diff = trend_ahs - hhr_ahs,
           blended_ahs = hhr_ahs + (diff*ahs_mix)) %>%
    select(year, gss_code, area_code, hhr_ahs, trend_ahs, blended_ahs)
  
  ahs <- select(ahs_choice, year, gss_code, area_code, ahs = blended_ahs)
  
  # 8 --------------------------------------------------------------------------
  
  # Apply the blended AHS to the input housing trajectory
  # Then add on the CE population
  
  target_population <- apply_rate_to_population(popn = households,
                                                rates = ahs,
                                                col_popn = "households",
                                                col_rate = "ahs",
                                                col_out = "target_popn",
                                                col_aggregation = c("year", "area_code")) %>% 
    mutate(target_popn = ifelse(is.na(target_popn), 0, target_popn),
           target_popn = ifelse(is.infinite(target_popn), 0, target_popn)) %>% 
    .add_ce_popn(communal_establishment_population, "target_popn",
                 col_aggregation = c("area_code"))
  
  # 9 --------------------------------------------------------------------------
  
  # Calculate the implied net migration components
  # Calculate consistent gross migration components
  
  sum_births <- trend_projection$births %>% 
    group_by(year, area_code) %>% 
    summarise(births = sum(births)) %>% 
    data.frame()
  
  sum_deaths <- trend_projection$deaths %>% 
    group_by(year, area_code) %>% 
    summarise(deaths = sum(deaths)) %>% 
    data.frame()
  
  sum_start_popn <- start_population %>% 
    group_by(year, area_code) %>% 
    summarise(start_popn = sum(popn)) %>% 
    data.frame() 
  
  target_net <- sum_start_popn %>% 
    select(-year) %>% 
    left_join(target_population, by = "area_code") %>% 
    left_join(sum_births, by = c("year", "area_code")) %>% 
    left_join(sum_deaths, by = c("year", "area_code")) %>% 
    mutate(net_target = target_popn - start_popn - births + deaths) %>% 
    select(-births, -deaths, -target_popn, -start_popn)
  
  optimised_flows <- regross_parallel(base_in = trend_projection$in_migration,
                                      base_out = trend_projection$out_migration,
                                      target_net = target_net, 
                                      col_inflow = "inflow",
                                      col_outflow = "outflow",
                                      col_target = "net_target",
                                      n_cores) %>% 
    select(year, area_code, inflow, outflow)
  
  migration_distribution <- left_join(trend_projection$in_migration,
                                      trend_projection$out_migration,
                                      by = col_agg) %>%
    group_by(year, area_code) %>%
    mutate(dist_in = inflow / sum(inflow),
           dist_out = outflow / sum(outflow)) %>%
    data.frame() %>%
    select(-inflow, -outflow)
  
  adjusted_migration <- migration_distribution %>% 
    left_join(optimised_flows, by = c("year", "area_code")) %>%
    mutate(inflow = inflow * dist_in,
           outflow = outflow * dist_out,
           netflow = inflow - outflow) %>%
    select(col_agg, inflow, outflow, netflow)
  
  adjusted_migration <- list(inflow = select(adjusted_migration, col_agg, inflow),
                             outflow = select(adjusted_migration, col_agg, outflow),
                             netflow = select(adjusted_migration, col_agg, netflow))
  
  rm(optimised_flows)
  
  # 10 -------------------------------------------------------------------------
  
  # Apply the calculated components to get an age structure for the target population
  # There may be some negatives in the hh population at the SYA level
  
  unconstrained_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(trend_projection$births,
                                                        adjusted_migration$inflow),
                                   subtraction_data = list(trend_projection$deaths,
                                                           adjusted_migration$outflow),
                                   col_aggregation = col_agg) 
  
  unconstrained_household_popn <- unconstrained_population %>% 
    .remove_ce_popn(communal_establishment_population) %>% 
    check_negative_values("household_popn") %>% 
    mutate(popn = ce_popn + household_popn) %>% 
    select(year, gss_code, area_code, sex, age, popn, ce_popn, household_popn)
  
  unconstrained_population <- unconstrained_household_popn %>% 
    select(year, gss_code, area_code, sex, age, popn)
  
  # 11 -------------------------------------------------------------------------
  
  # Constrain if required
  # Remove the CE pop and check that the household pop isn't negative
  
  if(constraint_list$components$population){
    
    constrained_population <- unconstrained_population %>%
      .apply_constraint(constraint_list$population_constraint,
                        areas = constraint_list$constraint_lookup,
                        mapping = constraint_list$mapping)
    
    constrained_household_popn <- constrained_population %>% 
      .remove_ce_popn(communal_establishment_population) %>% 
      check_negative_values("household_popn") %>% 
      mutate(popn = ce_popn + household_popn) %>% 
      select(year, gss_code, area_code, sex, age, popn, ce_popn, household_popn)
    
    constrained_population <- constrained_household_popn %>% 
      select(year, gss_code, area_code, sex, age, popn)
    
    final_population <- constrained_population
    
  } else {
    
    constrained_population <- NULL
    final_population <- unconstrained_population
    
  }
  
  # 12 -------------------------------------------------------------------------
  
  # The migration components need to be re-calculated to ensure they are consistent
  
  final_net <- final_population %>% 
    rename(final = popn) %>% 
    left_join(aged_on_population, by = col_agg) %>%
    rename(aged = popn) %>% 
    mutate(aged = ifelse(is.na(aged),0,aged)) %>% 
    left_join(trend_projection$births, by = col_agg) %>% 
    left_join(trend_projection$deaths, by = col_agg) %>% 
    mutate(births = ifelse(is.na(births), 0, births)) %>% 
    group_by(year, area_code, sex, age) %>% 
    summarise(net_target = sum(final - aged - births + deaths),
              .groups = 'drop_last') %>% 
    data.frame()
  
  optimised_flows <- regross_parallel(base_in = trend_projection$in_migration,
                                      base_out = trend_projection$out_migration,
                                      target_net = final_net, 
                                      col_inflow = "inflow",
                                      col_outflow = "outflow",
                                      col_target = "net_target",
                                      n_cores)

  lk <- select(lookup, gss_code, area_code)
  
  final_migration <- optimised_flows %>%
    left_join(lk, by = "area_code") %>% 
    mutate(netflow = inflow - outflow) %>% 
    select(col_agg, inflow, outflow, netflow)
  
  final_migration <- list(inflow = select(final_migration, col_agg, inflow),
                          outflow = select(final_migration, col_agg, outflow),
                          netflow = select(final_migration, col_agg, netflow))
  
  # 13  ------------------------------------------------------------------------
  
  # Re-compile the population using the finalised components
  # Confirm that HH population is > 0 (this can introduce inconsistencies)
 
  final_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(trend_projection$births,
                                                        final_migration$inflow),
                                   subtraction_data = list(trend_projection$deaths,
                                                           final_migration$outflow),
                                   col_aggregation = col_agg) 
  
  final_household_popn <- final_population %>% 
    .remove_ce_popn(communal_establishment_population) %>% 
    check_negative_values("household_popn") %>% 
    mutate(popn = ce_popn + household_popn) %>% 
    select(year, gss_code, area_code, sex, age, popn, ce_popn, household_popn)
  
  final_population <- final_household_popn %>% 
    select(year, gss_code, area_code, sex, age, popn)
  
  # 14 -------------------------------------------------------------------------
  
  # Reporting output - the final implied AHS
  
  actual_ahs <- final_household_popn %>% 
    lazy_dt() %>% 
    group_by(area_code) %>% 
    summarise(hh_popn = sum(household_popn), .groups = 'drop_last') %>% 
    left_join(households, by ="area_code") %>% 
    data.frame() %>% 
    mutate(actual_ahs = hh_popn/households) %>% 
    select(-year, -hh_popn, -households)
  
  ahs_choice <- ahs_choice %>% 
    left_join(actual_ahs, by = "area_code")
  
  # Reporting output - Components of Change dataframe
  
  components <- bind_rows(mutate(final_population, component = 'popn'),
                          rename(trend_projection$births, popn = births) %>%  mutate(component = 'births'),
                          rename(trend_projection$deaths, popn = deaths) %>%  mutate(component = 'deaths'),
                          rename(final_migration$inflow, popn = inflow) %>%  mutate(component = 'inflow'),
                          rename(final_migration$outflow, popn = outflow) %>%  mutate(component = 'outflow'),
                          rename(final_migration$netflow, popn = netflow) %>%  mutate(component = 'netflow')) %>%
    
    lazy_dt() %>% 
    group_by(gss_code, area_code, year, sex, age, component) %>% 
    summarise(popn = sum(popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    pivot_wider(names_from = 'component', values_from = 'popn') %>% 
    mutate(births = ifelse(age > 0, 0, births))
  
  # Reporting output - Households and population deatiled summary
  
  households_detail <- final_household_popn %>%
    lazy_dt() %>%
    group_by(gss_code, area_code, year) %>%
    summarise(population = sum(popn),
              ce_population = sum(ce_popn),
              household_popn = sum(household_popn), .groups = 'drop_last') %>%
    data.frame() %>%
    left_join(households, by=c("year","area_code")) %>%
    rename(input_households = households)
  
  # 15 -------------------------------------------------------------------------
  
  output_list <- list(population = final_population,
                      unconstrained_population = unconstrained_population,
                      constrained_population = constrained_population,
                      births = trend_projection$births,
                      deaths = trend_projection$deaths,
                      out_migration = final_migration$outflow,
                      in_migration = final_migration$inflow,
                      net_migration = final_migration$netflow,
                      household_population_sya = final_household_popn,
                      ahs = ahs_choice,
                      households_detail = households_detail,
                      detailed_components = components,
                      hhr_ahs_uplift = hhr_ahs_uplift)
  
  # 16 -------------------------------------------------------------------------
  
  #validate there are no NAs in any of the output dataframes
  lapply(output_list, FUN = function(x){
    a <- output_list
    if(sum(is.na(x))!=0){browser()}
  })
  
  return(output_list)
}
