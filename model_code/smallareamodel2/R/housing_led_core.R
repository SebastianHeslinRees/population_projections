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
#' 
#' @return A list of projected components
#' 
#' @import popmodules
#' @import dplyr
#' @import tidyr
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
                             constraint_list){
  
  col_agg <- c("year", "gss_code", "gss_code_ward", "sex", "age")
  nested_geog <- c("gss_code", "gss_code_ward")
  
  #1. GSS codes present in housing trajectory
  constrain_gss <- unique(intersect(start_population$gss_code_ward, households$gss_code))
  
  households <- filter(households, gss_code_ward %in% constrain_gss)
  
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code_ward %in% constrain_gss)) 
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code_ward %in% constrain_gss))
  
  aged_on_population <- filter(start_population, gss_code_ward %in% constrain_gss) %>%
    popn_age_on(col_aggregation = col_agg,
                col_geog = nested_geog)
  
  #-----------------------------------------------------------------------------
  #TODO
  #Need to make sure that the household population that is put into the internal hh model
  #doesn't have negative population. I'm doing this here. But arguably it could be done when the
  #sya is grouped into age groups. 
  #browser()
  household_population_sya <- trend_projection$population %>% 
    left_join(communal_establishment_population, by = c("gss_code_ward", "sex", "age")) %>% 
    mutate(household_popn = popn - ce_popn) %>% 
    check_negative_values("household_popn")
  
  households_detail <- household_population_sya %>% 
    group_by(gss_code, gss_code_ward, year) %>% 
    summarise(trend_population = sum(popn),
              ce_population = sum(ce_popn),
              household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    left_join(households, by=c("year","gss_code_ward")) %>% 
    rename(input_households = households)
  
  household_population <- households_detail %>% 
    select(gss_code, gss_code_ward, year, household_popn) 
  
  #-----------------------------------------------------------------------------
  
  curr_yr_hhr_households <- household_population_sya %>% 
    mutate(age_group = case_when(age <= 17 ~ "0_17",
                                 age %in% 18:24 ~ "18_24",
                                 age %in% 25:34 ~ "25_34",
                                 age %in% 35:49 ~ "35_49",
                                 age %in% 50:64 ~ "50_64",
                                 TRUE ~ "65_plus")) %>% 
    group_by(gss_code, gss_code_ward, sex, age_group) %>% 
    summarise(household_popn = sum(household_popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    left_join(household_rep_rates, by = c("gss_code_ward", "sex", "age_group")) %>% 
    mutate(households = hh_rep_rate * household_popn) %>% 
    group_by(gss_code, gss_code_ward, year) %>% 
    summarise(households = sum(households), .groups = 'drop_last') %>% 
    data.frame() 
  
  curr_yr_hhr_ahs <- curr_yr_hhr_households %>% 
    left_join(household_population, by = c("gss_code", "gss_code_ward", "year")) %>% 
    mutate(hhr_ahs = household_popn/households) %>% 
    select(gss_code, gss_code_ward, year, hhr_ahs)
  
  #-----------------------------------------------------------------------------
  
  curr_yr_trend_ahs <- left_join(households, household_population, by=c("year", "gss_code_ward")) %>%
    mutate(trend_ahs = household_popn/households)
  
  #-----------------------------------------------------------------------------
  
  if(is.null(hhr_ahs_uplift)){
    hhr_ahs_uplift <- left_join(curr_yr_hhr_ahs, curr_yr_trend_ahs,
                                by = c("gss_code", "gss_code_ward", "year")) %>% 
      mutate(hhr_diff = trend_ahs - hhr_ahs) %>%
      select(gss_code_ward, hhr_diff)
  }
  
  
  curr_yr_hhr_ahs <- curr_yr_hhr_ahs %>% 
    left_join(hhr_ahs_uplift, by = "gss_code_ward") %>% 
    mutate(hhr_ahs = hhr_ahs + hhr_diff) %>% 
    select(gss_code, gss_code_ward, year, hhr_ahs)
  
  #-----------------------------------------------------------------------------
  
  # floating method
  ahs_choice <- curr_yr_hhr_ahs %>%
    filter(gss_code_ward %in% constrain_gss) %>%
    left_join(curr_yr_trend_ahs, by = c("year", "gss_code", "gss_code_ward")) %>%
    select(-household_popn, -households) %>% 
    mutate(diff = trend_ahs - hhr_ahs,
           applied_ahs = hhr_ahs + (diff*ahs_mix)) %>%
    select(year, gss_code, gss_code_ward, hhr_ahs, trend_ahs, applied_ahs)
  
  ahs <- select(ahs_choice, year, gss_code, gss_code_ward, ahs = applied_ahs)
  
  #-----------------------------------------------------------------------------
  
  #TODO this dataframe ends up being nonesense becuase of post-constraining changes
  households_detail <- left_join(households_detail, curr_yr_hhr_households,
                                 by = c("gss_code", "gss_code_ward", "year")) %>% 
    rename(HHR_households = households) %>% 
    left_join(ahs_choice, by=c("year", "gss_code", "gss_code_ward"))
  
  #-----------------------------------------------------------------------------
  
  target_population <- apply_rate_to_population(popn = households,
                                                rates = ahs,
                                                col_popn = "households",
                                                col_rate = "ahs",
                                                col_out = "target_popn",
                                                col_aggregation = c("year", "gss_code_ward")) %>% 
    mutate(target_popn = ifelse(is.na(target_popn), 0, target_popn),
           target_popn = ifelse(is.infinite(target_popn), 0, target_popn))
  
  adjusted_migration <- adjust_gross_migration(household_population,
                                                  target = target_population,
                                                  inflow_df = trend_projection$in_migration,
                                                  outflow_df = trend_projection$out_migration,
                                                  col_aggregation = c("year","gss_code_ward"),
                                                  col_popn = "household_popn",
                                                  col_target = "target_popn",
                                                  col_inflow = "inflow",
                                                  col_outflow = "outflow")
  
  #-----------------------------------------------------------------------------
  
  unconstrained_population <- aged_on_population %>%
    #take out ce
    construct_popn_from_components(addition_data = list(trend_projection$births,
                                                        adjusted_migration$inflow),
                                   subtraction_data = list(trend_projection$deaths,
                                                           adjusted_migration$outflow),
                                   col_aggregation = col_agg) %>%
    rbind(areas_with_no_housing_data$population) # %>% 
  #check_negs %>% 
  #add back in CE
  
  #Is this problematic? If I explicitly add in CE pop after migration then
  #I can be sure the pop is always >= ce_pop
  
  #-----------------------------------------------------------------------------

   if(constraint_list$components$population){
    
    constrained_population <- unconstrained_population %>%
      check_negative_values("popn") %>% 
      .apply_constraint(constraint_list$population_constraint,
                        areas = constraint_list$constraint_lookup,
                        mapping = constraint_list$mapping)
    end_population <- constrained_population
    
  } else {
    
    constrained_population <- NULL
    end_population <- unconstrained_population %>%
      check_negative_values("popn")
  }
  
  #-----------------------------------------------------------------------------
  
  final_migration <- adjust_gross_migration(unconstrained_population,
                                                  target = end_population,
                                                  inflow_df = adjusted_migration$inflow,
                                                  outflow_df = adjusted_migration$outflow,
                                                  col_aggregation = col_agg,
                                                  col_popn = "popn",
                                                  col_target = "popn",
                                                  col_inflow = "inflow",
                                                  col_outflow = "outflow")
  
  #TODO the household population is inconsistent
  # because the end population doesn't really account for the ce pop
  # the check negs bit of this effectively over-rides the ce pop
  # should the remainder be added into the ce pop, should something
  # be done sooner so that this doesn't become an issue (some kind of rule
  # the means end pop is always >= ce pop), or don't we really care
  
  #browser()
  final_household_popn <- end_population %>% 
    left_join(communal_establishment_population, by = c("gss_code_ward", "sex", "age")) %>% 
    mutate(household_popn = popn - ce_popn) %>% 
    check_negative_values("household_popn")
  
  actual_ahs <- final_household_popn %>% 
    group_by(gss_code_ward) %>% 
    summarise(hh_popn = sum(household_popn), .groups = 'drop_last') %>% 
    left_join(households, by ="gss_code_ward") %>% 
    mutate(actual_ahs = hh_popn/households) %>% 
    select(-year, -hh_popn, -households)
  
  ahs_choice <- ahs_choice %>% 
    left_join(actual_ahs, by = "gss_code_ward")
  
  #-----------------------------------------------------------------------------
  
  net_migration <- final_migration$outflow %>% 
    mutate(net_migration = outflow *-1) %>% 
    select(-outflow) %>% 
    rbind(final_migration$inflow %>% 
            rename(net_migration = inflow)) %>% 
    group_by_at(col_agg) %>% 
    summarise(net_migration = sum(net_migration), .groups = 'drop_last') %>% 
    data.frame()
  
  #-----------------------------------------------------------------------------
  
  components <- bind_rows(mutate(end_population, component = 'popn'),
                          rename(trend_projection$births, popn = births) %>%  mutate(component = 'births'),
                          rename(trend_projection$deaths, popn = deaths) %>%  mutate(component = 'deaths'),
                          rename(final_migration$inflow, popn = inflow) %>%  mutate(component = 'inflow'),
                          rename(final_migration$outflow, popn = outflow) %>%  mutate(component = 'outflow'),
                          rename(net_migration, popn = net_migration) %>%  mutate(component = 'netflow')) %>%
    
    group_by(gss_code, gss_code_ward, year, sex, age, component) %>% 
    summarise(popn = sum(popn), .groups = 'drop_last') %>% 
    data.frame() %>% 
    pivot_wider(names_from = 'component', values_from = 'popn') %>% 
    mutate(births = ifelse(age != 0, 0, births))
  
  #-----------------------------------------------------------------------------
  
  output_list <- list(population = end_population,
                      unconstrained_population = unconstrained_population,
                      constrained_population = constrained_population,
                      births = trend_projection$births,
                      deaths = trend_projection$deaths,
                      out_migration = final_migration$outflow,
                      in_migration = final_migration$inflow,
                      net_migration = net_migration,
                      household_population_sya = final_household_popn,
                      ahs = ahs_choice,
                      households_detail = households_detail,
                      detailed_components = components,
                      hhr_ahs_uplift = hhr_ahs_uplift)
  
  #validate no NAs in outputs
  lapply(output_list, FUN = function(x){
    a <- output_list
    if(sum(is.na(x))!=0){browser()}
  })
  
  return(output_list)
}
