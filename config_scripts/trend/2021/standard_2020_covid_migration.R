#-------------------------------------------------------------------------------
# Attach standard covid migration rates
# If a year exists in the input list then it is not replaced by the standard data
# i.e. user input supersedes this function

covid_migration <- function(int_out_flows_rates,
                            int_in,
                            domestic_rates){
  
  
  covid_int_out<- list(
    '2021' = list(path =  "input_data/scenario_data/2020_int_out_scenario_1_yr_2021.rds",
                  transition = F),
    '2022' = list(path = "input_data/scenario_data/2020_int_out_scenario_1_yr_2022.rds",
                  transition = T))
  
  #-------------------------------------------------------------------------------
  
  covid_int_in  <- list(
    '2021' = list(path =  "input_data/scenario_data/2020_int_in_scenario_1_yr_2021.rds",
                  transition = F),
    '2022' = list(path = "input_data/scenario_data/2020_int_in_scenario_1_yr_2022.rds",
                  transition = T))
  #-------------------------------------------------------------------------------
  
  covid_dom <- list(
    '2021' = list(path =  "input_data/scenario_data/2020_dom_scenario_1_yr_2021.rds",
                  transition = F),
    '2022' = list(path =  "input_data/scenario_data/2020_dom_scenario_1_yr_2022.rds",
                  transition = T))
  
  #-------------------------------------------------------------------------------
  
  f <- function(a,b){
    
    x <- a[!names(a) %in% names(b)]
    y <- c(x, b)
    y <- y[order(names(y))]
    return(y)
    
  }
  
  int_out <- f(covid_int_out, int_out_flows_rates)
  int_in  <- f(covid_int_in, int_in)
  dom <- f(covid_dom, domestic_rates)
  
  return(list(int_out, int_in, dom))
  
}