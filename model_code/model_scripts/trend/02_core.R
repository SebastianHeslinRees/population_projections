#' Run a cohort component population model and output population, components of change and rates
#'
#' Given a starting population and set of projected rates for fertility, mortality and migration
#' the function will produce a population for the next year using a cohort component method.
#' The model outputs are the calculated populations and components of change.
#'
#' @param start_population A data frame. The population at the start of the time period
#' @param fertility_rates A data frame. A set of age-secific fertility rates to be applied to the population
#' @param mortality_rates A data frame. A set of age/sex-specific mortality probabilities to be
#'   applied to the population
#' @param int_out_flows_rates A data frame. A set of international out migration rates to be applied to
#'   the population, or a set of international out flow totals to be subtracted from the population
#' @param int_in_flows A data frame. A set of international in migration flows to be added to
#'   the population
#' @param domestic_rates. A data frame. A set of orgin-destination migration rates by age and sex
#'   to be applied to the population
#' @param int_out_method. A string. A switch to define whether international out migration is
#'   a rate or a flow. Either \code{rate} or \code{flow}
#' @param constraints A list. A set of national-level constraints for each component. If the projection
#'   is to be run unconstrained this is set to NULL. Default \code{NULL}
#' @param upc A data frame. Unattributable population change component. If no UPC is being applied this
#'   is set to NULL. Default \code{NULL}
#' @param projection_year Numeric. The year being projected
#'
#' @return A list where each element is a data frame containing either projected population or
#' projected components of change.
#'
#' @import popmodules
#' @import dplyr
#' @import assertthat
#'
#' @export

trend_core <- function(start_population, 
                       fertility_rates, mortality_rates,
                       int_out_flows_rates, int_in_flows,
                       domestic_rates,
                       int_out_method,
                       constraints = NULL, upc = NULL,
                       projection_year) {
 
  # run projection
  cat('\r',paste("  Projecting year",projection_year))
  flush.console()

  
  # aged on population is used due to definitions of MYE to ensure the correct denominator
  # population in population at 30th June
  # change rates are for changes that occured in the 12 months up to 30th June
  # age is the age the cohort is at 30th June
  aged_popn <- start_population %>%
    popn_age_on() 
  

  # at_risk <- start_population %>%
  #   mutate(age = age+1) %>%
  #   left_join(start_population, by=c("gss_code","year","sex","age")) %>%
  #   mutate(popn = (popn.x + popn.y)/2)       %>%
  #   mutate(popn = ifelse(is.na(popn),0,popn),
  #          year = year +1) %>%
  #   select(gss_code, year, sex, age, popn) %>%
  #   arrange(gss_code, year, sex, age) %>%
  #   filter(age <=90)
  
  births_by_mother <- apply_rate_to_population(aged_popn,
                                               filter(fertility_rates, age != 0),
                                               col_out = "births",
                                               many2one = FALSE)

  if(!is.null(constraints)){
    births_by_mother <- constrain_births(births=births_by_mother, constraint=constraints$births_constraint)
  }
  
  birthratio_m2f <- 1.05 
  births <- sum_births_and_split_by_sex_ratio(births_by_mother, birthratio_m2f)

  aged_popn_w_births <- rbind(aged_popn, rename(births, popn = births))
  validate_population(aged_popn_w_births, col_data = "popn", comparison_pop = mutate(as.data.frame(start_population), year=year+1))
  
  deaths <- component_from_popn_rate(popn = aged_popn_w_births,
                                     component_rate = mortality_rates,
                                     col_popn = "popn",
                                     col_rate = "rate",
                                     col_component = "deaths")
  validate_population(deaths, col_data = "deaths", comparison_pop = mutate(as.data.frame(start_population), year=year+1))
  validate_join_population(aged_popn_w_births, deaths, many2one = FALSE, one2many = FALSE) 
  
  if(!is.null(constraints)){
    deaths <- constrain_component(popn=deaths, constraint = constraints$deaths_constraint, col_popn = "deaths")
  }
  
  natural_change_popn <- left_join(aged_popn_w_births, deaths, by=c("year","gss_code","sex","age")) %>%
    mutate(popn = popn - deaths) %>%
    select(-deaths)
  
  if(int_out_method=="flow"){
    int_out <- int_out_flows_rates
  } else {
    int_out <- component_from_popn_rate(popn = natural_change_popn,
                                        component_rate = int_out_flows_rates,
                                        col_popn = "popn",
                                        col_rate = "int_out",
                                        col_component = "int_out")
  } 
  
  validate_population(int_out, col_data = "int_out")
  validate_join_population(aged_popn_w_births, int_out, many2one = FALSE, one2many = FALSE)
  
  
  if(!is.null(constraints)){
    int_out <- constrain_component(popn=int_out, constraint = constraints$international_out_constraint,
                              col_popn = "int_out")
  }
  
  int_in <- int_in_flows
  validate_population(int_in, col_data = "int_in")
  validate_join_population(aged_popn_w_births, int_in, many2one = FALSE, one2many = FALSE)
  
  if(!is.null(constraints)){
    int_in <- constrain_component(popn=int_in, constraint = constraints$international_in_constraint,
                             col_popn = "int_in")
  }
  
  domestic_flow <- natural_change_popn %>%
    apply_domestic_migration_rates(mign_rate = domestic_rates,
                                   col_aggregation = c("gss_code"="gss_out", "sex", "age"),
                                   col_gss_destination = "gss_in",
                                   col_popn = "popn",
                                   col_rate = "rate", 
                                   col_flow = "flow", 
                                   pop1_is_subset = FALSE, 
                                   many2one = FALSE) %>%
    mutate(year = projection_year)
  
  if(!is.null(constraints)){
    domestic_flow <- constrain_cross_border(domestic_flow = domestic_flow,
                                            in_constraint = constraints$cross_border_in_constraint,
                                            out_constraint = constraints$cross_border_out_constraint,
                                            col_flow = "flow")
  }
  
  dom_out <- dtplyr::lazy_dt(domestic_flow) %>%
    group_by(year, gss_out, sex, age) %>%
    summarise(dom_out = sum(flow)) %>%
    as.data.frame() %>%
    rename(gss_code = gss_out)%>%
    tidyr::complete(year, gss_code, age=0:90, sex, fill=list(dom_out=0))
  
  dom_in <- dtplyr::lazy_dt(domestic_flow) %>%
    group_by(year, gss_in, sex, age) %>%
    summarise(dom_in = sum(flow)) %>%
    as.data.frame()%>%
    rename(gss_code = gss_in)%>%
    tidyr::complete(year, gss_code, age=0:90, sex, fill=list(dom_in=0))
  
  if(is.null(upc)){
    
    next_yr_popn <- natural_change_popn %>% 
      arrange(year, gss_code, sex, age) %>%
      left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
      left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
      left_join(dom_out, by = c("year", "gss_code", "age", "sex")) %>% 
      left_join(dom_in, by = c("year", "gss_code", "age", "sex")) %>% 
      mutate(next_popn = popn - int_out + int_in - dom_out + dom_in)
    
  } else {
    
    next_yr_popn <- natural_change_popn %>% 
      arrange(year, gss_code, sex, age) %>%
      left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
      left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
      left_join(dom_out, by = c("year", "gss_code", "age", "sex")) %>% 
      left_join(dom_in, by = c("year", "gss_code", "age", "sex")) %>% 
      left_join(upc, by = c("gss_code", "age", "sex")) %>%
      tidyr::replace_na(list(upc = 0)) %>%
      mutate(next_popn = popn - int_out + int_in - dom_out + dom_in + upc)
  }
  
  # FIXME / TODO This setup creates negative populations - For now
  # I'm just setting -ve pops to zero and noting this in the pull request
  if(any(next_yr_popn$next_popn < 0)) {
    ix <- next_yr_popn$next_popn < 0
    sum_negative <- sum(next_yr_popn$next_popn[ix])
    
    warning(paste0(capture.output({
      print(paste("Negative populations were created in the", projection_year, "loop, summing to", sum_negative))
      
      if(sum(ix) < 20) {
        print("Values:")
        print(next_yr_popn[ix,])
      } else {
        print("First 20 values:")
        print(next_yr_popn[ix,][1:20,])
        print("Levels affected:")
        sapply(c("gss_code", "age", "sex"), function(col) {
          print("col:")
          print(unique(next_yr_popn[ix, col]))
        })
      }
    }), collapse = "\n"))
    
    next_yr_popn <- mutate(next_yr_popn, next_popn = ifelse(next_popn < 0, 0, next_popn))
  }
  
  next_yr_popn <- select(next_yr_popn, year, gss_code, age, sex, popn = next_popn)
  
  validate_population(next_yr_popn, col_data = "popn",
                      comparison_pop = start_population,
                      col_comparison = c("gss_code","sex","age"))
  
  return(list(population = next_yr_popn,
              deaths = deaths,
              births = births,
              int_out = int_out,
              int_in = int_in,
              dom_out = dom_out,
              dom_in = dom_in,
              births_by_mothers_age = births_by_mother,
              natural_change = natural_change_popn))
 
}
