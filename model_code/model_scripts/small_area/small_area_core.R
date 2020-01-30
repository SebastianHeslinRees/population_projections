#' Small area model core: calculate components of change and next year's
#' population
#'
#' @param start_population Data frame. Initial population at small area
#'   resolution.
#' @param births Data frame. Historical births backseries at small area
#'   resolution. Used when \code{projection_year} is \code{last_data_year} or
#'   earlier.
#' @param deaths Data frame. Historical deaths backseries at small area
#'   resolution. Used when \code{projection_year} is \code{last_data_year} or
#'   earlier.
#' @param communal_est_popn Data frame. Static communal establishment population
#'   at small area resolution.
#' @param out_migration_rates Data frame. Static outmigration rates at small
#'   area resolution.
#' @param in_migration_characteristics Data frame. Static inmigration rates at
#'   small area resolution. Rates should give the number of people by age and
#'   sex that migrate to an area for each adult (over 18) that migrates to an
#'   area (that is, the rates will sum to a bit more than 1 for each geography).
#' @param popn_constraint Data frame. Population by borough to constrain the
#'   output to.
#' @param birth_constraint Data frame. Births by borough to constrain the output
#'   to.
#' @param death_constraint Data frame. Deaths by borough to constrain the output
#'   to.
#' @param fertility_rates NULL or data frame. Fertility rates at small area
#'   resolution. Used when \code{projection_year} is after
#'   \code{last_data_year}.
#' @param mortality_rates NULL or data frame. Mortality rates at small area
#'   resolution. Used when \code{projection_year} is after
#'   \code{last_data_year}.
#' @param last_data_year Integer. Year at which the model switches from using
#'   historical births and deaths from \code{births} and \code{deaths} to
#'   projected rates from \code{fertility_rates} and \code{mortality_rates}.
#' @param dwellings Data frame. Number of dwellings at small area resolution.
#' @param adults_per_dwelling Data frame. Ratio of adults to dwellings at small
#'   area resolution.
#' @param projection_year Integer. Uh, the projection year.
#' @param small_area_to_district Data frame. A lookup from ward to borough codes.


small_area_core <- function(start_population, births, deaths, communal_est_popn,
                            out_migration_rates, in_migration_characteristics,
                            popn_constraint, birth_constraint, death_constraint,
                            fertility_rates, mortality_rates, last_data_year,
                            dwellings, adults_per_dwelling,
                            projection_year, small_area_to_district){

  ####Age on####
  aged_on_popn <- popn_age_on(start_population,
                              col_aggregation = c("year", "gss_code_small_area", "age", "sex"))
  
  ####Fertility####
  if(projection_year <= last_data_year){
   
    curr_yr_births <- filter(births, year == projection_year) %>%
      group_by(year, gss_code_small_area) %>%
      summarise(births = sum(births)) %>%
      as.data.frame()
    
  } else {
 
    curr_yr_births <-left_join(aged_on_popn, fertility_rates,
                               by=c("year","gss_code_small_area","age","sex")) %>%
      mutate(births = fert_rate * popn) %>%
      group_by(year, gss_code_small_area) %>%
      summarise(births = sum(births)) %>%
      as.data.frame()
  }
browser()
  #Constrain births
  curr_yr_births <- left_join(curr_yr_births, small_area_to_district, by="gss_code_small_area") %>%
    constrain_component(constraint = birth_constraint,
                        col_aggregation = c("year","gss_code"),
                        col_popn = "births") %>%
    sum_births_and_split_by_sex_ratio(geog_cols = c("gss_code","gss_code_small_area")) %>%
    rename(popn = births)
  
  popn_w_births <- rbind(aged_on_popn, curr_yr_births) %>%
    arrange(gss_code_small_area, sex, age)
  
  ####Mortality####
  if(projection_year <= last_data_year){
    
    curr_yr_deaths <- filter(deaths, year == projection_year) %>%
      left_join(small_area_to_district, by = "gss_code_small_area")
    
    age_groups <- unique(deaths$age_group)
    
    x <- list()
    for(i in 1:length(age_groups)){
      
      mn <- substr(age_groups[i],1,2)
      mn <- case_when(mn == "1_" ~ 1,
                      mn == "5_" ~ 5,
                      TRUE ~ as.numeric(mn))
      
      mx <- case_when(mn == 0 ~ 0,
                      mn == 1 ~ 4,
                      mn == 85 ~ 90,
                      TRUE ~ mn+4)
      
      x[[i]] <- filter(curr_yr_deaths, age_group == age_groups[i]) %>%
        distribute_within_age_band(popn_2 = death_constraint,
                                   popn_1_col = "deaths",
                                   popn_2_col = "deaths",
                                   min_age = mn, max_age = mx,
                                   col_aggregation=c("gss_code","sex")) %>%
        as.data.frame()
    }
    
    curr_yr_deaths <- data.table::rbindlist(x) %>%
      as.data.frame() 
    
  } else {
    
    # TODO apply_rate_to_population() instead
    curr_yr_deaths <- popn_w_births %>%
      left_join(mortality_rates, by = c("year","gss_code_small_area","sex","age")) %>%
      mutate(deaths = mort_rate*popn)
    
  }
 
  #Constrain deaths
  curr_yr_deaths <- curr_yr_deaths %>%
    constrain_component(constraint = death_constraint,
                        col_aggregation = c("year","gss_code","sex","age"),
                        col_popn = "deaths") %>%
    select(year, gss_code, gss_code_small_area, sex, age, deaths) %>%
    as.data.frame()

  natural_change_popn <- left_join(popn_w_births, curr_yr_deaths, by=c("year","gss_code","gss_code_small_area","sex","age")) %>%
    mutate(popn = popn - deaths) %>%
    select(-deaths)
  
  #Apply outmigration rates
  out_migration <- left_join(natural_change_popn, out_migration_rates, by=c("gss_code_small_area","sex","age")) %>%
    mutate(out_migrants = out_migration_rate*popn) 
  
  popn_post_out_migration <- out_migration %>%
    mutate(popn = popn - out_migrants) %>%
    select(gss_code_small_area, sex, age, popn)

  #Compare the adult household popn to the target adults
  #target adults = adults_per_dwelling * dwellings
  #Difference is migration inflow
  target_adults <- left_join(adults_per_dwelling, dwellings, by=c("gss_code_small_area")) %>%
    mutate(target = units*adults_per_dwelling)

  household_popn <- popn_post_out_migration %>%
    left_join(communal_est_popn, by = c("gss_code_small_area", "sex", "age")) %>%
    mutate(household_popn = popn - ce_popn) %>%
    select(-popn)
  
  inflow_total <- dtplyr::lazy_dt(household_popn) %>%
    filter(age >= 18) %>%
    group_by(gss_code_small_area) %>%
    summarise(adults = sum(household_popn)) %>%
    as.data.frame() %>%
    left_join(target_adults, by=c("gss_code_small_area")) %>%
    mutate(inflow = target - adults) %>%
    select(year, gss_code_small_area, inflow)
  
  #apply in-migration characteristics rates to inflow
  in_migration <- left_join(in_migration_characteristics, inflow_total,
                            by="gss_code_small_area") %>%
    mutate(in_migrants = in_migration_rate * inflow)
  
  #Add in-migration
  #Add communal establishment popn back in
  unconstrained_popn <- left_join(popn_post_out_migration, in_migration, by = c("gss_code_small_area", "sex", "age")) %>%
    mutate(popn = popn + in_migrants) %>% 
    select(year, gss_code_small_area, sex, age, popn)
  
  #constrain borough populations to match constraint
  constrained_popn <- left_join(unconstrained_popn, small_area_to_district, by="gss_code_small_area") %>%
    as.data.frame() %>%
    constrain_component(constraint = popn_constraint,
                        col_popn = "popn",
                        col_aggregation = c("year","gss_code","sex","age"))
  
  final_popn <- constrained_popn %>%
    select(year, gss_code, gss_code_small_area, sex, age, popn)
  
  final_migration <- rename(natural_change_popn, nat_chng = popn) %>%
    left_join(final_popn, by=c("year","gss_code","gss_code_small_area","sex","age")) %>%
    mutate(migration = popn - nat_chng) %>%
    select(year, gss_code, gss_code_small_area, sex, age, migration)
  
  return(list(population = final_popn,
              births = rename(curr_yr_births, births = popn),
              deaths = curr_yr_deaths,
              migration = final_migration))
}