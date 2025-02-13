#' Run a cohort component population model and output population, components
#' of change and rates
#'
#' Given a starting population and set of projected rates for fertility,
#' mortality and migration the function will produce a population for the next
#' year using a cohort component method. The model outputs are the calculated
#' populations and components of change.
#'
#' @param start_population A data frame. The population at the start of the
#'   time period
#' @param fertility_rates A data frame. A set of age-specific fertility rates
#'   to be applied to the population
#' @param mortality_rates A data frame. A set of age/sex-specific mortality
#'   probabilities to be applied to the population
#' @param external_births A dataframe containing additional births data beyond
#'   the last data year available in the mid-estimates. Default NULL
#' @param external_deaths A dataframe containing additional deaths data beyond
#'   the last data year available in the mid-estimates Default NULL
#' @param int_out_flows_rates A data frame. A set of international out migration
#'   rates to be applied to the population, or a set of international out flow
#'   totals to be subtracted from the population
#' @param int_in_flows A data frame. A set of international in migration flows
#'   to be added to the population
#' @param domestic_rates A data frame. A set of origin-destination migration rates
#'   by age and sex to be applied to the population
#' @param int_out_method A string. A switch to define whether international out
#'   migration is a rate or a flow. Either \code{rate} or \code{flow}
#' @param constraints A list. A set of national-level constraints for each
#'   component. If the projection is to be run unconstrained this is set to NULL.
#'   Default \code{NULL}
#' @param popn_adjustment A data frame. Population adjustment component (UPC). If no
#'   adjustment is being applied this is set to NULL. Default \code{NULL}
#' @param projection_year Numeric. The year being projected
#' @param region_lookup A data frame. A lookup between LAD gss codes and region
#'   gss codes
#'
#' @return A list where each element is a data frame containing either projected
#'  population or projected components of change.
#'
#' @import popmodules
#' @import dplyr
#' @import assertthat
#' @importFrom dtplyr lazy_dt
#' @importFrom tidyr complete
#' @importFrom utils flush.console
#' 
#' @export

trend_core <- function(start_population,
                       fertility_rates,
                       mortality_rates,
                       external_births = NULL,
                       external_deaths = NULL,
                       int_out_flows_rates,
                       int_in_flows,
                       domestic_rates,
                       int_out_method,
                       constraints = NULL,
                       popn_adjustment = NULL,
                       projection_year,
                       region_lookup) {
  
  
  # aged on population is used due to definitions of MYE to ensure the correct denominator
  # population in population at 30th June
  # change rates are for changes that occurred in the 12 months up to 30th June
  # age is the age the cohort is at 30th June

  # Calculate the aged population based on the start population
  aged_popn <- start_population %>%
    popn_age_on2() 

  # Set the birth ratio between males and females
  birthratio_m2f <- 1.05 

  births_by_mother <- apply_rate_to_population(popn = aged_popn,
                                               rates = filter(fertility_rates, age != 0), # remove age 0 rates
                                               col_out = "births")

  # Check if there are external births data
  if(!is.null(external_births)){
    
  
    external_constraint <- external_births %>% 
      group_by(gss_code) %>%
      summarise(constraint = sum(births), .groups = 'drop_last')
    
    # Adjust births by mother based on the external constraint
    births_by_mother <- births_by_mother %>% 
      filter(gss_code %in% external_constraint$gss_code) %>% 
      group_by(gss_code) %>% 
      mutate(births_total = sum(births)) %>% 
      data.frame() %>% 
      left_join(external_constraint, by = "gss_code") %>% 
      mutate(births = births * (constraint/births_total)) %>% 
      select(names(births_by_mother)) %>% 
      bind_rows(filter(births_by_mother, !gss_code %in% external_constraint$gss_code))
  }

  # Check if there are birth constraints
  if(!is.null(constraints)){
    
    # Apply birth constraints
    births_by_mother <- constrain_births(births=births_by_mother, constraint=constraints$births_constraint)
  }

  # Calculate total births and split by sex ratio
  if(is.null(external_births)){
    
    births <- sum_births_and_split_by_sex_ratio(births_by_mother, birthratio_m2f)
    
  } else {
    
    # Use external births data and calculate additional births
    births <- external_births
    other_births <- filter(births_by_mother, !gss_code %in% external_births$gss_code) %>% 
      sum_births_and_split_by_sex_ratio(birthratio_m2f)
    births <- bind_rows(births, other_births)
  }


  aged_popn_w_births <- rbind(aged_popn, rename(births, popn = births))


  validate_population(aged_popn_w_births, col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = mutate(as.data.frame(start_population), year=year+1))

  # Check if there are external deaths data
  if(!is.null(external_deaths)){
    deaths <- external_deaths
  } else {
    deaths <- apply_rate_to_population(popn = aged_popn_w_births,
                                       rates = mortality_rates,
                                       col_popn = "popn",
                                       col_rate = "rate",
                                       col_out = "deaths")
  }

 
  validate_population(deaths, col_data = "deaths",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = mutate(as.data.frame(start_population), year=year+1))

  # Validate the join between aged population and deaths
  validate_join_population(aged_popn_w_births, deaths, many2one = FALSE, one2many = FALSE)

  # Apply death constraints if available
  if(!is.null(constraints)){
    deaths$country <- substr(deaths$gss_code, 1, 1)
    deaths <- constrain_component(popn=deaths,
                                  constraint = constraints$deaths_constraint,
                                  col_aggregation = c("year", "sex", "age", "country"),
                                  col_popn = "deaths",
                                  rows_to_constrain = deaths$country %in% constraints$deaths_constraint$country)
    deaths$country <- NULL
  }

  # Calculate natural change in population
  natural_change_popn <- left_join(aged_popn_w_births, deaths, by=c("year","gss_code","sex","age")) %>%
    mutate(popn = popn - deaths) %>%
    select(-deaths) %>% 
    check_negative_values(data_col = "popn")

  # Determine the method for international out
  if(int_out_method=="flow"){
    int_out <- int_out_flows_rates
  } else {
    int_out <- apply_rate_to_population(popn = natural_change_popn,
                                        rates = int_out_flows_rates,
                                        col_popn = "popn",
                                        col_rate = "int_out",
                                        col_out = "int_out")
  }

  
  validate_population(int_out, col_data = "int_out",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

  # Validate the join between aged population and international outflows
  validate_join_population(aged_popn_w_births, int_out, many2one = FALSE, one2many = FALSE)

  # Apply international outflow constraints if available
  if(!is.null(constraints)){
    int_out$country <- substr(int_out$gss_code, 1, 1)
    int_out <- constrain_component(popn=int_out,
                                   constraint = constraints$international_out_constraint,
                                   col_aggregation = c("year", "sex", "age", "country"),
                                   col_popn = "int_out",
                                   rows_to_constrain = int_out$country %in% constraints$international_out_constraint$country)
    int_out$country <- NULL
  }

  # Set international inflows
  int_in <- int_in_flows


  validate_population(int_in, col_data = "int_in",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

  # Validate the join between aged population and international inflows
  validate_join_population(aged_popn_w_births, int_in, many2one = FALSE, one2many = FALSE)

  # Apply international inflow constraints if available
  if(!is.null(constraints)){
    int_in$country <- substr(int_in$gss_code, 1, 1)
    int_in <- constrain_component(popn=int_in,
                                  constraint = constraints$international_in_constraint,
                                  col_aggregation = c("year", "sex", "age", "country"),
                                  col_popn = "int_in",
                                  rows_to_constrain = int_in$country %in% constraints$international_in_constraint$country)
    int_in$country <- NULL
  }

  # Calculate domestic flows
  domestic_flow <- natural_change_popn %>%
    apply_domestic_migration_rates(mign_rate = domestic_rates,
                                   col_aggregation = c("gss_code"="gss_out", "sex", "age"),
                                   col_gss_destination = "gss_in",
                                   col_popn = "popn",
                                   col_rate = "rate",
                                   col_flow = "flow",
                                   one2many = TRUE,
                                   many2one = FALSE) %>%
    mutate(year = projection_year) %>%
    select(year, gss_in, gss_out, age, sex, flow)

  # Apply cross-border constraints if available
  if(!is.null(constraints)){
    domestic_flow <- constrain_cross_border(domestic_flow = domestic_flow,
                                            in_constraint = constraints$cross_border_in_constraint,
                                            out_constraint = constraints$cross_border_out_constraint,
                                            col_flow = "flow")
  }

  regional_flows <- aggregate_regional_flows(domestic_flow, region_lookup)
  
  #District in E & W, national S, NI gross flows
  dom_out <- sum_domestic_flows(domestic_flow, "out")
  dom_in <- sum_domestic_flows(domestic_flow, "in")
  
  #Region in E, national W, S, NI gross flows
  reg_dom_out <- sum_domestic_flows(regional_flows[['regional_flow']], "out")
  reg_dom_in <- sum_domestic_flows(regional_flows[['regional_flow']], "in")
  
  #National E, W, S, NI gross flows
  nat_dom_out <- sum_domestic_flows(regional_flows[['national_flow']], "out")
  nat_dom_in <- sum_domestic_flows(regional_flows[['national_flow']], "in")
  
  #inner and outer London
  sub_reg_dom_out <- sum_domestic_flows(regional_flows[['sub_regional_flow']], "out") 
  sub_reg_dom_in <- sum_domestic_flows(regional_flows[['sub_regional_flow']], "in")
  
  # Combine domestic outflows from different geographies
  dom_out_all_geog <- rbind(
    filter(dom_out, substr(gss_code,1,2) %in% c("E0","W0")),
    filter(reg_dom_out, substr(gss_code,1,3) == "E12"),
    filter(sub_reg_dom_out, substr(gss_code,1,3)=="E13"),
    nat_dom_out)

  # Combine domestic inflows from different geographies
  dom_in_all_geog <- rbind(
    filter(dom_in, substr(gss_code,1,2) %in% c("E0","W0")),
    filter(reg_dom_in, substr(gss_code,1,3) == "E12"),
    filter(sub_reg_dom_in, substr(gss_code,1,3)=="E13"),
    nat_dom_in)

  # Construct next year's population based on components
  if(is.null(popn_adjustment)){
    next_yr_popn <- construct_popn_from_components(start_population = natural_change_popn,
                                                   addition_data = list(int_in, dom_in),
                                                   subtraction_data = list(int_out, dom_out),
                                                   col_aggregation = c("year", "gss_code", "age", "sex"))
  } else {
    next_yr_popn <- construct_popn_from_components(start_population = natural_change_popn,
                                                   addition_data = list(int_in, dom_in, popn_adjustment),
                                                   subtraction_data = list(int_out, dom_out),
                                                   col_aggregation = c("year", "gss_code", "age", "sex"),
                                                   data_are_subsets = TRUE) 
  }
  
  # FIXME
  # TODO This setup creates negative populations
  # For now just setting -ve pops to zero
  next_yr_popn <- check_negative_values(next_yr_popn, "popn")
  
  next_yr_popn <- select(next_yr_popn, year, gss_code, age, sex, popn)
  
  validate_population(next_yr_popn, col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE,
                      comparison_pop = start_population,
                      col_comparison = c("gss_code","sex","age"))
  
  return(list(population = next_yr_popn,
              deaths = deaths,
              births = births,
              int_out = int_out,
              int_in = int_in,
              dom_out = dom_out_all_geog,
              dom_in = dom_in_all_geog,
              births_by_mothers_age = births_by_mother,
              natural_change = natural_change_popn))
  
}
