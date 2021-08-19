#' Run a housing-led population model and output population, components of change and rates
#'
#' Given a starting population and set of projected rates for fertility, mortality and migration
#' the function will produce a population for the next year using the GLA housing-led
#' method. The model outputs are the calculated populations and components of change.
#'
#' @param start_population A data frame. The population at the start of the time period
#' @param trend_projection A list. The output of the \code{trendmodel::trend_core}
#'   function for the \code{projection_year}
#' @param component_constraints A dataframe. The component constraints by borough
#' @param hma_constraint A dataframe. The total population constraint by housing
#'   market area (HMA)
#' @param communal_establishment_population A dataframe. The communal establishment
#'   population for the \code{projection_year}
#' @param external_ahs A set of average household size rates by geographic
#'   aggregation and year consistent with the \code{communal_establishment_population}
#' @param households_1 A dataframe. For each geographic aggregation a number of
#'   total households to be used in calculating the current year trend AHS
#' @param households_2 A dataframe. For each geographic aggregation a number of
#'   total households to be used in calculating the population by applying an AHS
#'   rate
#' @param hma_list A named list. Containing gss_codes which, when aggregated,
#'   form a housing market area (HMA). The name in the list is the name of the HMA.
#' @param projection_year Numeric. The year being projected
#' @param ahs_cap_year Numeric. The year in which to set the AHS cap
#' @param ahs_cap A dataframe or NULL. When no AHS cap has been set this
#'   parameter is set to \code{NULL}. Once a set of AHS caps have been set
#'   this becomes the dataframe containing those caps
#' @param ahs_method A string or Numeric. If the AHS tree method is to be used,
#'   set to \code{tree}, else the floating AHS method will be used. In this case
#'   the parameter is a numeric between 0 and 1. It acts to determine the mix
#'   between the external AHS and the trend AHS where 0 is the external and 1 is
#'   the trend.
#' @param ldd_final_yr Numeric. The final year for which LDD development data 
#'   is available.
#' @param constrain_projection Logical. If \code{TRUE} then the projection will
#'   be constrained (at the borough level for components and at hma level for
#'   population totals.) If \code{FALSE} the model is run unconstrained.
#' @param external_births A dataframe containing actual births data to be used in
#'   core in place of calculated births
#' @param external_deaths A dataframe containing actual deaths data to be used in
#'   core in place of calculated deaths
#'
#' @return A list where each element is a data frame containing either projected population or
#' projected components of change.
#'
#' @import popmodules
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom dtplyr lazy_dt

housing_led_core <- function(start_population,
                             trend_projection,
                             component_constraints,
                             hma_constraint,
                             communal_establishment_population,
                             external_ahs,
                             households_1,
                             households_2,
                             hma_list,
                             projection_year,
                             ahs_cap_year,
                             ahs_cap,
                             ahs_method,
                             ldd_final_yr,
                             constrain_projection,
                             external_births,
                             external_deaths){

  #1. GSS codes present in housing trajectory
  constrain_gss <- unique(households_1$gss_code)
  
  trend_projection <- lapply(trend_projection, filter_to_LAs)
  areas_with_no_housing_data <- lapply(trend_projection, function(x) filter(x, !x$gss_code %in% constrain_gss)) 
  trend_projection_national <- trend_projection
  trend_projection <- lapply(trend_projection, function(x) filter(x, x$gss_code %in% constrain_gss))
  aged_on_population <- filter(start_population, gss_code %in% constrain_gss) %>%
    popn_age_on()
  
  #2. Constrain births, deaths & international
  #So that totals match at the borough level
  if(constrain_projection){
    births <- component_constraints[['birth_constraint']] %>%
      filter(gss_code %in% constrain_gss, year == projection_year) %>%
      mutate(female = births*(100/205),
             male = births*(105/205),
             age = 0) %>%
      select(-births) %>%
      pivot_longer(c("female","male"), names_to = "sex", values_to="births")
    
    deaths <- trend_projection[['deaths']] %>%
      constrain_component(constraint = component_constraints[['death_constraint']],
                          col_aggregation = c("year","gss_code"),
                          col_popn = "deaths",
                          col_constraint = "deaths",
                          rows_to_constrain = trend_projection$deaths$gss_code %in% constrain_gss)
    
    int_out <- trend_projection[['int_out']] %>%
      constrain_component(constraint = component_constraints[['international_out_constraint']],
                          col_aggregation = c("year","gss_code"),
                          col_popn = "int_out",
                          col_constraint = "int_out",
                          rows_to_constrain = trend_projection$int_out$gss_code %in% constrain_gss)
  } else {
    
    births <- trend_projection[['births']] %>%
      select(year, gss_code, age, sex, births)
    deaths <- trend_projection[['deaths']]
    int_out <- trend_projection[['int_out']]
  }
  
  #Overwrite births if there are actuals for the projection year
  if(!is.null(external_births)){
    births <- trend_projection[['births']] %>%
      select(year, gss_code, age, sex, births) %>% 
      filter(!gss_code %in% external_births$gss_code) %>%
      rbind(external_births) %>% 
      filter(gss_code %in% constrain_gss)
  }
  
  if(!is.null(external_deaths)){
    deaths <- trend_projection[['deaths']] %>%
      select(year, gss_code, age, sex, deaths) %>% 
      filter(!gss_code %in% external_deaths$gss_code) %>%
      rbind(external_deaths) %>% 
      filter(gss_code %in% constrain_gss)
  }
  
  if(is.null(trend_projection[['upc']])) {
    
    initial_population <- aged_on_population %>%
      construct_popn_from_components(addition_data = list(births,
                                                          trend_projection[['int_in']],
                                                          trend_projection[['dom_in']]),
                                     subtraction_data = list(deaths,
                                                             int_out,
                                                             trend_projection[['dom_out']]))
  } else {
    initial_population <- aged_on_population %>%
      construct_popn_from_components(addition_data = list(births,
                                                          trend_projection[['int_in']],
                                                          trend_projection[['dom_in']],
                                                          trend_projection[['upc']]),
                                     subtraction_data = list(deaths,
                                                             int_out,
                                                             trend_projection[['dom_out']]),
                                     data_are_subsets = TRUE)
  }
  
  initial_population <- initial_population %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame()
  
  
  #3. Calculate household population
  household_population <- lazy_dt(initial_population) %>%
    group_by(gss_code, year) %>%
    summarise(popn = sum(popn)) %>%
    left_join(communal_establishment_population, by=c("gss_code","year")) %>%
    mutate(household_popn = popn - communal_est_popn) %>%
    as.data.frame() %>%
    validate_population(col_data = "household_popn", col_aggregation = c("year","gss_code"),
                        test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE) %>%
    select(-popn, -communal_est_popn)
  
  #4. Calculate trend AHS
  #Population divided by households_1
  #2011 dw2hh ratio
  curr_yr_trend_ahs <- left_join(households_1, household_population, by=c("year","gss_code")) %>%
    mutate(trend = household_popn/households) %>%
    select(-household_popn, -households)
  
  #Initially the ahs_cap is passed as NULL
  #In the year where the cap is set the variable is changed to be a dataframe of AHS values
  #The core outputs the ahs_cap variable and the next year's ahs_cap is set as that output
  #in the control. So it will be NULL before the cap year and then the same dataframe following
  #the cap year
  
  if(ahs_cap_year == projection_year){
    ahs_cap <- curr_yr_trend_ahs %>% rename(cap = trend) %>% select(-year)
  }
  
  #New floating method
  if(is.numeric(ahs_method)){
    set_ahs <- external_ahs %>%
      filter(gss_code %in% constrain_gss) %>%
      rename(external = ahs) %>%
      left_join(curr_yr_trend_ahs, by = c("year", "gss_code")) 
    
    if(is.null(ahs_cap)){
      set_ahs <- set_ahs %>%
        mutate(cap = trend)
    } else {
      set_ahs <- set_ahs %>%
        left_join(ahs_cap, by="gss_code")
    }    
    
    set_ahs <- set_ahs %>%
      mutate(diff = cap - external,
             float = external + (diff*0)) %>%
      select(-diff) %>%
      mutate(diff = trend - float,
             ahs = float + (diff*ahs_method))
    
    ahs_choice <- select(set_ahs, year, gss_code, external, cap, trend, float, ahs)
    ahs <- select(set_ahs, year, gss_code, ahs)
    
  }
  
  if(ahs_method == "tree"){
    
    #AHS decision tree
    average_household_size <- external_ahs %>%
      filter(gss_code %in% constrain_gss) %>%
      rename(external = ahs) %>%
      left_join(curr_yr_trend_ahs, by = c("year","gss_code"))
    
    if(projection_year <= ldd_final_yr){
      if(is.null(ahs_cap)){
        #before the max LDD data year always select the trend
        average_household_size <- average_household_size %>%
          mutate(cap = NA, ahs = trend, ahs_choice = "trend")
      } else {
        average_household_size <- average_household_size %>%
          left_join(ahs_cap, by = "gss_code") %>%
          mutate(ahs_choice = case_when(trend > cap ~ "cap",
                                        trend <= cap ~ "trend"),
                 ahs = case_when(trend > cap ~ cap,
                                 trend <= cap ~ trend))
      }
    } else {
      if(is.null(ahs_cap)){
        #after LDD but before cap is set select the higher of the trend or external AHS
        average_household_size <- average_household_size %>%
          mutate(cap = NA,
                 ahs_choice = case_when(trend > external ~ "trend",
                                        external > trend ~ "external"),
                 ahs = case_when(trend > external ~ trend,
                                 external > trend ~ external))
      } else {
        average_household_size <- external_ahs %>%
          filter(gss_code %in% constrain_gss) %>%
          rename(external = ahs) %>%
          left_join(curr_yr_trend_ahs, by = c("year","gss_code")) %>%
          left_join(ahs_cap, by = "gss_code") %>%
          mutate(ahs_choice = case_when(cap < trend ~ "cap",
                                        cap < external ~ "cap",
                                        trend > external ~ "trend",
                                        external > trend ~ "external",
                                        TRUE ~ "trend"),
                 ahs = case_when(cap < trend ~ cap,
                                 cap < external ~ cap,
                                 trend > external ~ trend,
                                 external > trend ~ external,
                                 TRUE ~ trend))
      }
    }
    
    ahs_choice <- select(average_household_size, year, gss_code, external, cap, trend, ahs_choice)
    ahs <- select(average_household_size, year, gss_code, ahs)
    
  }
  
  #6. Target population
  #dw2hh 2018
  target_population <- apply_rate_to_population(households_2, ahs,
                                                col_popn = "households",
                                                col_rate = "ahs",
                                                col_out = "target_popn",
                                                col_aggregation = c("year","gss_code"),
                                                one2many = TRUE)
  
  #7. This point is intentionally left blank
  
  
  #8. Compare population from step 3 to target from step 5.
  #   Difference = domestic adjustment
  #   Adjust domestic
  adjusted_domestic_migration <- adjust_domestic_migration(popn = household_population,
                                                           target = target_population,
                                                           dom_in = trend_projection[['dom_in']],
                                                           dom_out = trend_projection[['dom_out']],
                                                           col_aggregation = c("year","gss_code"),
                                                           col_popn = "household_popn",
                                                           col_target = "target_popn")
  
  #This is a QA output only
  out_adjusted_dom <- left_join(adjusted_domestic_migration[["dom_in"]],
                                adjusted_domestic_migration[["dom_out"]],
                                by=c("year","gss_code","age","sex")) %>%
    transmute(year, gss_code, age, sex, dom_net = dom_in-dom_out) %>%
    left_join(trend_projection[["dom_in"]], by = c("year","gss_code","age","sex")) %>%
    left_join(trend_projection[["dom_out"]], by = c("year","gss_code","age","sex")) %>%
    transmute(year, gss_code, age, sex, adjustment = dom_net - dom_in + dom_out)
  
  #9. Add components from step 6 to domestic from step 8 & start population
  #Join the non-adjusted population data back to the adjusted
  #This implicitly adds back in the communal establishment pop because we're
  #rolling back to a point before it was removed
  unconstrained_population <- aged_on_population %>%
    construct_popn_from_components(addition_data = list(births,
                                                        trend_projection[['int_in']],
                                                        adjusted_domestic_migration[['dom_in']]),
                                   subtraction_data = list(deaths,
                                                           int_out,
                                                           adjusted_domestic_migration[['dom_out']])) %>%
    rbind(areas_with_no_housing_data[['population']]) 

  #Join the non-adjusted components data back to the adjusted

  births <- rbind(births, areas_with_no_housing_data[['births']])
  deaths <- rbind(deaths, areas_with_no_housing_data[['deaths']])
  int_out <- rbind(int_out, areas_with_no_housing_data[['int_out']])
  dom_in <- rbind(adjusted_domestic_migration[['dom_in']], areas_with_no_housing_data[['dom_in']])
  dom_out <- rbind(adjusted_domestic_migration[['dom_out']], areas_with_no_housing_data[['dom_out']])
  
  if(constrain_projection & !is.null(hma_list)){
    #10. Constrain total population
    constrained_population <- unconstrained_population %>%
      check_negative_values("popn") %>%
      constrain_to_hma(constraint = hma_constraint,
                       hma_list = hma_list,
                       col_aggregation = c("year","hma","sex","age"),
                       col_popn = "popn",
                       col_constraint = "popn")
    
    #11. Compare population from step 10 to population from step 9.
    #    Difference = domestic adjustment
    #    Adjust domestic
    final_domestic_migration <- adjust_domestic_migration(popn =  unconstrained_population,
                                                          target = constrained_population,
                                                          dom_in = dom_in,
                                                          dom_out = dom_out,
                                                          col_aggregation = c("year","gss_code","sex","age"),
                                                          col_popn = "popn",
                                                          col_target = "popn",
                                                          rows_to_constrain =  unconstrained_population$gss_code %in% hma_list$gss_code)
    dom_in <- final_domestic_migration[['dom_in']]
    dom_out <- final_domestic_migration[['dom_out']]
    
    #TODO More validation needed here?
    output_population <- check_negative_values(constrained_population, "popn")
    
  } else {
    
    output_population <- check_negative_values(unconstrained_population, "popn")
    
  }
  
  #Recalculate the household population
  household_population <- lazy_dt(output_population) %>%
    filter(gss_code %in% communal_establishment_population$gss_code) %>% 
    group_by(gss_code, year) %>%
    summarise(popn = sum(popn)) %>%
    left_join(communal_establishment_population, by=c("gss_code","year")) %>%
    mutate(household_popn = popn - communal_est_popn) %>%
    as.data.frame() %>%
    validate_population(col_data = "household_popn", col_aggregation = c("year","gss_code"),
                        test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)

  summary <- left_join(households_2, household_population, by = c("gss_code","year")) %>% 
    mutate(actual_ahs = household_popn/households) %>% 
    left_join(ahs, by = c("gss_code", "year")) %>% 
    rename(applied_ahs = ahs) %>% 
    select(gss_code, year, popn, communal_est_popn,
           household_popn, households, applied_ahs, actual_ahs)
  
  household_population <- household_population %>%
    filter(gss_code %in% constrain_gss) %>%
    select(-popn, -communal_est_popn)
  
  ahs <- select(summary, gss_code, year, ahs = actual_ahs)
  
  return(list(population = output_population,
              births = births,
              deaths = deaths,
              int_in = trend_projection_national[['int_in']],
              int_out = int_out,
              dom_in = dom_in,
              dom_out = dom_out,
              ahs = ahs,
              ahs_choice = ahs_choice,
              ahs_cap = ahs_cap,
              household_population = household_population,
              adjusted_domestic_migration = out_adjusted_dom,
              unconstrained_population =  unconstrained_population,
              summary = summary))
}
