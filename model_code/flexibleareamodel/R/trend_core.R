#' Project from a starting population using a CCM methodology to
#' produce an output population and components 
#'
#' @param start_population A dataframe. The starting population
#' @param fertility_rates A dataframe. Fertility rates for the \code{projection_year}
#' @param mortality_rates A dataframe. Mortality rates for the \code{projection_year}
#' @param out_rates A dataframe. Out migration rates for the \code{projection_year}
#' @param in_flows A dataframe. In migration flows for the \code{projection_year}
#' @param projection_year Numeric. The year being projected
#' @param constraint_list A list of constraints.
#' @param excess_deaths A dataframe of additional deaths for the projection year
#' 
#' @return A list of projected components
#' 
#' @import popmodules
#' @import dplyr
#' @import tidyr
#' 
#' @export

trend_core <- function(start_population,
                       fertility_rates,
                       mortality_rates,
                       out_rates,
                       in_flows,
                       projection_year,
                       constraint_list,
                       excess_deaths){
  
  if("gss_code" %in% names(start_population)){
    col_agg <- c("year", "gss_code", "area_code", "sex", "age")
    nested_geog <- c("gss_code", "area_code")
  } else {
    col_agg <- c("year", "area_code", "sex", "age")
    nested_geog <- "area_code"
  }
  
  #-----------------------------------------------------------------------------
  
  start_population <- start_population %>% 
    select(!!col_agg, "popn")
  
  #age on
  aged_popn <- popn_age_on2(start_population,
                            col_aggregation = col_agg,
                            col_geog = nested_geog)
  
  assert_that(sum(is.na(aged_popn))==0, msg=paste("aged_popn", projection_year))
  
  #-----------------------------------------------------------------------------
  
  #fertility
  birthratio_m2f <- 1.05
  
  births_by_mother <- apply_rate_to_population(aged_popn,
                                               filter(fertility_rates, age != 0),
                                               col_out = "births",
                                               col_aggregation = col_agg,
                                               validate_geog = TRUE,
                                               col_geog = nested_geog)
  
  births <- sum_births_and_split_by_sex_ratio(births_by_mother, birthratio_m2f,
                                              col_aggregation = nested_geog)
  
  if(constraint_list$components$births){
    
    births <- apply_constraint(births, constraint_list$births_constraint,
                               constraint_lookup = constraint_list$apply_constraint_lookup,
                               mapping = constraint_list$mapping)
  }
  
  assert_that(sum(is.na(births))==0, msg=paste("births", projection_year))
  
  aged_popn_w_births <- rbind(aged_popn, rename(births, popn = births))
  
  #-----------------------------------------------------------------------------
  
  #mortality
  deaths <- apply_rate_to_population(popn = aged_popn_w_births,
                                     rates = mortality_rates,
                                     col_popn = "popn",
                                     col_rate = "rate",
                                     col_out = "deaths",
                                     col_aggregation = col_agg,
                                     validate_geog = TRUE,
                                     col_geog = nested_geog)
  
  #Covid deaths
  if(!is.null(excess_deaths)){
    deaths <- left_join(deaths, excess_deaths, by = col_agg) %>% 
      mutate(deaths = deaths + excess_deaths) %>% 
      select(names(deaths)) %>% 
      check_negative_values("deaths")
  }
  
  if(constraint_list$components$deaths){
    
    deaths <- apply_constraint(deaths, constraint_list$deaths_constraint,
                               constraint_lookup = constraint_list$apply_constraint_lookup,
                               mapping = constraint_list$mapping)
  }
  
  assert_that(sum(is.na(deaths))==0, msg=paste("deaths", projection_year))
  
  #-----------------------------------------------------------------------------
  
  #natural change
  natural_change_popn <- left_join(aged_popn_w_births, deaths, by = col_agg) %>%
    mutate(popn = popn - deaths) %>%
    select(-deaths) %>% 
    check_negative_values(data_col = "popn")
  
  #-----------------------------------------------------------------------------
  
  #out migration
  outflow <- apply_rate_to_population(popn = aged_popn_w_births,
                                      rates = out_rates,
                                      col_popn = "popn",
                                      col_rate = "out_rate",
                                      col_out = "outflow",
                                      col_aggregation = col_agg,
                                      validate_geog = TRUE,
                                      col_geog = nested_geog)
  
  if(constraint_list$components$out_migration){
    
    outflow <- apply_constraint(outflow, constraint_list$out_migration_constraint,
                                constraint_lookup = constraint_list$apply_constraint_lookup,
                                mapping = constraint_list$mapping)
  }
  
  assert_that(sum(is.na(outflow))==0, msg=paste("outflow", projection_year))
  
  #-----------------------------------------------------------------------------
  
  #in migration
  inflow <- filter(in_flows, year == projection_year) %>% 
    select(!!col_agg, "in_flow") %>% 
    rename(inflow = in_flow)
  
  if(constraint_list$components$in_migration){
    
    inflow <- apply_constraint(inflow, constraint_list$in_migration_constraint,
                               constraint_lookup = constraint_list$constraint_lookup,
                               mapping = constraint_list$mapping)
  }
  
  assert_that(sum(is.na(inflow))==0, msg=paste("inflow", projection_year))
  
  #-----------------------------------------------------------------------------
  
  #final population
  next_yr_popn <- construct_popn_from_components(start_population = natural_change_popn,
                                                 addition_data = list(inflow),
                                                 subtraction_data = list(outflow),
                                                 col_aggregation = col_agg) %>% 
    select(!!col_agg, "popn")
  
  assert_that(sum(is.na(next_yr_popn))==0, msg=paste("next_yr_popn", projection_year))
  
  negatives <- filter(next_yr_popn, popn < 0)
  next_yr_popn <- next_yr_popn %>% check_negative_values("popn")
  
  if(constraint_list$components$population){
    # TODO
    # This constraining step means the components do not sum to the population
    next_yr_popn <- apply_constraint(next_yr_popn, constraint_list$population_constraint,
                                     constraint_lookup = constraint_list$apply_constraint_lookup,
                                     mapping = constraint_list$mapping)
  }
  
  #-----------------------------------------------------------------------------
  
  #fix negatives by increasing inflow
  inflow <- inflow %>% 
    left_join(negatives,
              by = col_agg) %>% 
    mutate(inflow = ifelse(is.na(popn), inflow, inflow-popn)) %>% 
    select(-popn)
  
  total_net <- left_join(inflow, outflow, by = col_agg) %>% 
    mutate(net_migration = inflow - outflow) %>% 
    select(-inflow, -outflow)
  
  #-----------------------------------------------------------------------------
  
  #make a nice output dataframe
  components <- aged_popn_w_births %>% 
    arrange(across(col_agg)) %>% 
    mutate(births = ifelse(age == 0, popn, 0)) %>% 
    mutate(popn = ifelse(age == 0, 0, popn)) %>% 
    left_join(deaths, by = col_agg) %>% 
    left_join(outflow, by = col_agg) %>% 
    left_join(inflow, by = col_agg) %>% 
    rename(start_popn = popn) %>% 
    left_join(next_yr_popn, by = col_agg) %>% 
    mutate(change = births - deaths + inflow - outflow,
           popn_from_components = start_popn + change,
           diff = popn - popn_from_components) %>% 
    select(!!col_agg,
           start_popn, births, deaths,
           inflow, outflow, change,
           popn_from_components, diff, popn)
  
  if(sum(is.na(components))!=0){browser()}
  
  assert_that(sum(is.na(components))==0, msg=paste("components", projection_year))
  
  #-----------------------------------------------------------------------------
  
  return(list(population = next_yr_popn,
              births = births,
              deaths = deaths,
              in_migration = inflow,
              out_migration = outflow,
              net_migration = total_net,
              natural_change = natural_change_popn,
              births_by_mothers_age = births_by_mother,
              detailed_components = components))
  
}

