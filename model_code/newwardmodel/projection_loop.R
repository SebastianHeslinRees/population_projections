library(dplyr)
library(tidyr)
devtools::load_all('model_code/popmodules')

projection_loop <- function(start_population,
                            fertility_rates,
                            mortality_rates,
                            out_rates,
                            in_flows,
                            proj_yr){

  cat('\r',proj_yr)
  utils::flush.console()

  start_population <- start_population %>% 
    select(year, gss_code, gss_code_ward, sex, age, popn)

  col_agg <- c("year", "gss_code", "gss_code_ward", "sex", "age")
  
  #-----------------------------------------------------------------------------

  #age on
  aged_popn <- popn_age_on(start_population,
                      col_aggregation = col_agg,
                      col_geog = "gss_code_ward")
  
  assert_that(sum(is.na(aged_popn))==0, msg=paste("aged_popn", yr))
  
  #-----------------------------------------------------------------------------
  
  #fertility
  birthratio_m2f <- 1.05
  
  births_by_mother <- apply_rate_to_population(aged_popn,
                                               filter(fertility_rates, age != 0),
                                               col_out = "births",
                                               col_aggregation = col_agg,
                                               many2one = FALSE,
                                               col_geog_popn = "gss_code_ward",
                                               missing_levels_popn = TRUE,
                                               missing_levels_rate = TRUE,
                                               validate_geog = TRUE)

  births <- sum_births_and_split_by_sex_ratio(births_by_mother, birthratio_m2f,
                                              col_aggregation = c("gss_code","gss_code_ward"))
  

  assert_that(sum(is.na(births))==0, msg=paste("births", yr))
  
  aged_popn_w_births <- rbind(aged_popn, rename(births, popn = births))

  #-----------------------------------------------------------------------------
    
  #mortality
  deaths <- apply_rate_to_population(popn = aged_popn_w_births,
                                     popn_rate = mortality_rates,
                                     col_popn = "popn",
                                     col_rate = "rate",
                                     col_out = "deaths",
                                     col_aggregation = col_agg,
                                     col_geog_popn = "gss_code_ward",
                                     missing_levels_popn = TRUE,
                                     missing_levels_rate = TRUE,
                                     validate_geog = TRUE)
  
  assert_that(sum(is.na(deaths))==0, msg=paste("deaths", yr))
  
  #-----------------------------------------------------------------------------
  
  #natural change
  natural_change_popn <- left_join(aged_popn_w_births, deaths, by = col_agg) %>%
    mutate(popn = popn - deaths) %>%
    select(-deaths) %>% 
    check_negative_values(data_col = "popn")
  
  #-----------------------------------------------------------------------------
  
  #out migration
  outflow <- apply_rate_to_population(popn = aged_popn_w_births,
                                      popn_rate = out_rates,
                                      col_popn = "popn",
                                      col_rate = "out_rate",
                                      col_out = "outflow",
                                      col_aggregation = col_agg,
                                      col_geog_popn = "gss_code_ward",
                                      missing_levels_popn = TRUE,
                                      missing_levels_rate = TRUE,
                                      validate_geog = TRUE)
  
  assert_that(sum(is.na(outflow))==0, msg=paste("outflow", yr))
  
  #-----------------------------------------------------------------------------
  
  #in migration
  inflow <- filter(in_flows, year == proj_yr) %>% 
    rename(inflow = in_flow)
  
  assert_that(sum(is.na(inflow))==0, msg=paste("inflow", yr))
  
  #-----------------------------------------------------------------------------
  
  #final population
  next_yr_popn <- construct_popn_from_components(start_population = natural_change_popn,
                                                 addition_data = list(inflow),
                                                 subtraction_data = list(outflow),
                                                 col_aggregation = col_agg) %>% 
    select(year, gss_code, gss_code_ward, sex, age, popn)
  
  negatives <- filter(next_yr_popn, popn < 0)
  next_yr_popn <- next_yr_popn %>% check_negative_values("popn")
  
  assert_that(sum(is.na(components))==0, msg=paste("next_yr_popn", yr))
  
  #-----------------------------------------------------------------------------
 
  #fix negatives by increasing inflow
  inflow <- inflow %>% 
    left_join(negatives,
            by = col_agg) %>% 
    mutate(inflow = ifelse(is.na(popn), inflow, inflow-popn)) %>% 
    select(-popn)
  
  #-----------------------------------------------------------------------------
  
  #make a nice output dataframe
  components <- aged_popn_w_births %>% 
    mutate(popn = ifelse(age==0,0,popn)) %>% 
    left_join(births, by = col_agg) %>% 
    mutate(births = ifelse(is.na(births),0,births)) %>% 
    left_join(deaths, by = col_agg) %>% 
    left_join(outflow, by = col_agg) %>% 
    left_join(inflow, by = col_agg) %>% 
    rename(start_popn = popn) %>% 
    mutate(change = births - deaths + inflow - outflow,
           popn = start_popn + change)
  
  assert_that(sum(is.na(components))==0, msg=paste("components", yr))

  #-----------------------------------------------------------------------------
  
  return(list(next_yr_popn = next_yr_popn,
              components = components))
  
}