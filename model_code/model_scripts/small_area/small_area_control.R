####YEARS WITH BIRTH DATA####
#Borough rates are applied to all wards in the borough
#Resulting births are compared to actuals
#Scaling factors are produced for fertility rates

####YEARS WITH NO DATA####
#Calculate the geometric mean of all the scaling factors
#Apply it to the borough projected fertility rates

read_small_area_inputs <- funtion(path){
  df <- readRDS(path)
  if("gss_code_ward" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_ward)}
  if("gss_code_msoa" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_msoa)}
  return(df)
}

#Read Data
popn_estimates <- read_small_area_inputs()
communal_est_popn  <- read_small_area_inputs()
out_migration_rates <- read_small_area_inputs()
in_migration_characteristics <- read_small_area_inputs()
births <- read_small_area_inputs()
deaths <- read_small_area_inputs()
ldd_data
dwelling_trajectory
birth_constraint
death_constraint
popn_constraint
last_data_year
fertility_rates
mortality_rates
first_proj_yr
final_proj_year

#Jump off year borough totals should match constraint - check?

#Projection loop
curr_yr_popn <- filter(popn_estimates, year == projection_year-1)
projection <- list()

for(projection_year in first_proj_yr:final_proj_year){
  
  curr_yr_popn_constraint <- filter(popn_constraint, year == projection_year)
  curr_yr_birth_constraint <- filter(birth_constraint, year == projection_year)
  curr_yr_death_constraint <- filter(death_constraint, year == projection_year)
  
  if(projection_year == last_data_year+1){
    
    data_years <- unique(births$year)
    
    fert_scaling <- popn_age_on(popn_estimates) %>%
      filter(year %in% data_years) %>%
      apply_rate_to_population(popn_rate = fertility_rates,
                               col_aggregation = c("year","gss_code"),
                               col_popn = "popn",
                               col_rate = "fert_rate",
                               col_out = "births") %>%
      calculate_scaling_factors( constraint = births_constraint,
                                 col_aggregation = c("year","gss_code"),
                                 col_popn = "births",
                                 col_constraint = "births",
                                 rows_to_constrain = TRUE) %>%
      dtplyr::lazy_dt() %>%
      group_by(gss_code_small_area) %>%
      summarise(scaling = EnvStats::geoMean(scaling)) %>%
      as.data.frame()
    
    small_area_fertility_rates <- apply_rate_to_population(popn = fertility_rates,
                                                           popn_rate = fert_scaling)
  }
  
  if(projection_year > last_data_year){
    curr_yr_fertiity <- filter(small_area_fertility_rates, year == projection_year)
    curr_yr_mortality <- filter(small_area_mortality_rates, year == projection_year)
  } else {
    curr_yr_fertility <- NULL
    curr_yr_mortality <- NULL
  }
  
  projection[[projection_year]] <- small_area_core(start_population = curr_yr_popn,
                                                   births = births,
                                                   deaths = deaths,
                                                   communal_est_popn,
                                                   out_migration_rates,
                                                   in_migration_characteristics,
                                                   popn_constraint = curr_yr_popn_constraint,
                                                   birth_constraint = curr_yr_birth_constraint,
                                                   death_constraint = curr_yr_death_constraint,
                                                   fertility_rates = curr_yr_fertility,
                                                   mortality_rates = curr_yr_mortality,
                                                   projection_year) 
  
  curr_yr_popn <- projection[[projection_year]][['population']]
  
  if(projection_year <= last_data_year){
    popn_estimates <- filter(popn_estimates, year != projection_year) %>%
      rbind(curr_yr_popn)
  }
  
}