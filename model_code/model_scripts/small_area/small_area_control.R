#devtools::load_all('model_code/popmodules')
source('model_code/model_scripts/small_area/small_area_core.R')
source('model_code/model_scripts/small_area/arrange_small_area_core_outputs.R')
source('model_code/model_scripts/small_area/output_small_area_projection.R')

run_small_area_model <- function(config_list){
  
  message(paste("Running",config_list$projection_type,"model"))
  
  expected_config <- c("small_area_popn_estimates_path",
                       "small_area_communal_est_popn_path",
                       "small_area_births_backseries_path",
                       "small_area_deaths_backseries_path",
                       "small_area_births_sya_path",
                       "small_area_deaths_sya_path",
                       "small_area_ldd_data_path",
                       "small_area_dev_trajectory_path",
                       "adults_per_dwelling_path",
                       "small_area_to_district_path",
                       "out_migration_rates_path",
                       "in_migration_characteristics_path",
                       "housing_led_model_path",
                       "borough_fertility_rates_path",
                       "borough_mortality_rates_path",
                       "last_data_year",
                       "first_proj_yr",
                       "last_proj_yr",
                       "birth_rate_n_years_to_avg",
                       "death_rate_n_years_to_avg",
                       "ldd_final_yr",
                       "projection_type")
  
  validate_config_list(config_list, expected_config)
  
  read_small_area_inputs <- function(path){
    df <- readRDS(path)
    if("gss_code_ward" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_ward)}
    if("gss_code_msoa" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_msoa)}
    return(df)
  }
  
  #Read Data
  adults_per_dwelling <- read_small_area_inputs(config_list$adults_per_dwelling_path) %>%
    project_forward_flat(config_list$last_proj_yr)
  
  small_area_to_district <- read_small_area_inputs(config_list$small_area_to_district_path)
  out_migration_rates <- read_small_area_inputs(config_list$out_migration_rates_path)
  in_migration_characteristics <- read_small_area_inputs(config_list$in_migration_characteristics_path)
  
  popn_estimates <- read_small_area_inputs(config_list$small_area_popn_estimates_path)
  communal_est_popn  <- read_small_area_inputs(config_list$small_area_communal_est_popn_path)
  births <- read_small_area_inputs(config_list$small_area_births_backseries_path)
  deaths <- read_small_area_inputs(config_list$small_area_deaths_backseries_path)
  ldd_data <- read_small_area_inputs(config_list$small_area_ldd_data_path)
  dwelling_trajectory <- read_small_area_inputs(config_list$small_area_dev_trajectory_path)
  
  small_area_births_sya <- read_small_area_inputs(config_list$small_area_births_sya_path)
  small_area_deaths_sya <- read_small_area_inputs(config_list$small_area_deaths_sya_path)
  
  #----------
  
  birth_constraint <- readRDS(paste0(config_list$housing_led_model_path, "births.rds")) %>%
    filter(substr(gss_code,1,3)=="E09")
  death_constraint <- readRDS(paste0(config_list$housing_led_model_path, "deaths.rds")) %>%
    filter(substr(gss_code,1,3)=="E09")
  popn_constraint <- readRDS(paste0(config_list$housing_led_model_path,"population.rds")) %>%
    filter(substr(gss_code,1,3)=="E09")
  
  #------------
  
  fertility_rates <- readRDS(config_list$borough_fertility_rates_path) %>%
    filter(substr(gss_code,1,3)=="E09")
  mortality_rates <- readRDS(config_list$borough_mortality_rates_path) %>%
    filter(substr(gss_code,1,3)=="E09")
  
  #-------------------------
  
  #Create the cumulative development trajectory
  
  ldd_data <- filter(ldd_data, year <= config_list$ldd_final_yr) 
  assertthat::assert_that(min(dwelling_trajectory$year) <= config_list$ldd_final_yr + 1)
  
  dwelling_trajectory <- dwelling_trajectory %>%
    filter(year > config_list$ldd_final_yr) %>%
    rbind(ldd_data) %>%
    arrange(gss_code_small_area, year) %>%
    group_by(gss_code_small_area) %>%
    mutate(cum_units = cumsum(units)) %>%
    as.data.frame() %>%
    select(year, gss_code_small_area, units = cum_units) %>%
    validate_population(col_aggregation = c("year","gss_code_small_area"), col_data = "units")
  
  #-------------------------
  
  # Validate all these inputs
  validate_small_area_input_components(popn_estimates, adults_per_dwelling, out_migration_rates, in_migration_characteristics,
                                       birth_constraint, death_constraint, popn_constraint, fertility_rates,
                                       mortality_rates, dwelling_trajectory, config_list)
  
  #Projection loop
  curr_yr_popn <- filter(popn_estimates, year == config_list$first_proj_yr-1)
  projection <- list()
  
  for(projection_year in config_list$first_proj_yr:config_list$last_proj_yr){
    
    cat('\r',paste("  Projecting year", projection_year))
    flush.console()
    
    curr_yr_popn_constraint <- filter(popn_constraint, year == projection_year)
    curr_yr_birth_constraint <- filter(birth_constraint, year == projection_year)
    curr_yr_death_constraint <- filter(death_constraint, year == projection_year)
    curr_yr_dwellings <- filter(dwelling_trajectory, year == projection_year)
    curr_yr_adults_per_dwelling <- filter(adults_per_dwelling, year == projection_year) %>%
      select(gss_code_small_area, adults_per_dwelling)
    
    if(projection_year == config_list$last_data_year+1){
      
      #TODO make it work with age groups
      #Scaling factors for the 2019 rates and then applied to the fertility trajectory
      
      curr_yr_births <- dtplyr::lazy_dt(births) %>%
        group_by(year, gss_code_small_area) %>%
        summarise(births = sum(births)) %>%
        as.data.frame() %>%
        mutate(year = as.numeric(year))
      
      future_fertility_rates <- filter(fertility_rates, year == config_list$last_data_year+1) %>%
        select(-year)
      
      births_data_years <- (config_list$last_data_year-config_list$birth_rate_n_years_to_avg+1):config_list$last_data_year
      
      fertility_scaling <- calculate_geomean_scaling_factors(popn = popn_estimates,
                                                             future_rates = future_fertility_rates,
                                                             data_years = births_data_years,
                                                             constraint = curr_yr_births,
                                                             constraint_data_col = "births")
      
      #TODO apply_rate_to_population()
      small_area_fertility_rates <- left_join(fertility_scaling, small_area_to_district,
                                              by="gss_code_small_area") %>%
        left_join(fertility_rates, by=c("gss_code")) %>%
        mutate(fert_rate = scaling*rate) %>%
        select(year, gss_code_small_area, sex, age, fert_rate)
      
      #------------------
      
      curr_yr_deaths <- dtplyr::lazy_dt(deaths) %>%
        group_by(year, gss_code_small_area) %>%
        summarise(deaths = sum(deaths)) %>%
        as.data.frame() %>%
        mutate(year = as.numeric(year))
      
      future_mortality_rates <- filter(mortality_rates, year == config_list$last_data_year+1) %>%
        select(-year)
      
      deaths_data_years <- (config_list$last_data_year-config_list$death_rate_n_years_to_avg+1):config_list$last_data_year
      
      mortality_scaling <- calculate_geomean_scaling_factors(popn = popn_estimates,
                                                             future_rates = future_mortality_rates,
                                                             data_years = deaths_data_years,
                                                             constraint = curr_yr_deaths,
                                                             constraint_data_col = "deaths")
      
      #TODO apply_rate_to_population()
      small_area_mortality_rates <- left_join(mortality_scaling, small_area_to_district,
                                              by="gss_code_small_area") %>%
        left_join(mortality_rates, by=c("gss_code")) %>%
        mutate(mort_rate = scaling*rate)%>%
        select(year, gss_code_small_area, sex, age, mort_rate)
      
      
      # Validate these new data frames
      validate_small_area_fert_mort_components(popn_estimates, small_area_fertility_rates, small_area_mortality_rates,
                                               fertility_rates, mortality_rates, config_list)
    }
    
    #-----------------
    
    #Set rates for current year
    
    if(projection_year > config_list$last_data_year){
      curr_yr_fertility <- filter(small_area_fertility_rates, year == projection_year)
      curr_yr_mortality <- filter(small_area_mortality_rates, year == projection_year)
    } else {
      curr_yr_fertility <- NULL
      curr_yr_mortality <- NULL
    }
    
    #---------------
    
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
                                                     last_data_year = config_list$last_data_year,
                                                     dwellings = curr_yr_dwellings,
                                                     adults_per_dwelling = curr_yr_adults_per_dwelling,
                                                     projection_year = projection_year,
                                                     small_area_to_district = small_area_to_district)
    
    curr_yr_popn <- projection[[projection_year]][['population']]
    
    if(projection_year <= config_list$last_data_year){
      popn_estimates <- filter(popn_estimates, year != projection_year) %>%
        rbind(curr_yr_popn)
    }
    
  }
  
  message(" ")
  message("Running outputs")
  small_area_output_dir <- paste0(config_list$housing_led_model_path, config_list$projection_type,"/")
  
  projection <- arrange_small_area_core_outputs(projection, popn_estimates, dwelling_trajectory,
                                                config_list$first_proj_yr, config_list$last_proj_yr,
                                                small_area_births_sya, small_area_deaths_sya)
  
  projection <- output_small_area_projection(projection = projection,
                                             output_dir = small_area_output_dir,
                                             projection_type = config_list$projection_type,
                                             lookup = small_area_to_district)
  
  return(projection)
}


#===============================================================================

validate_small_area_input_components <- function(popn_estimates,
                                                 adults_per_dwelling,
                                                 out_migration_rates,
                                                 in_migration_characteristics,
                                                 birth_constraint,
                                                 death_constraint,
                                                 popn_constraint,
                                                 fertility_rates,
                                                 mortality_rates,
                                                 dwelling_trajectory,
                                                 config_list) {
  # Validate inputs
  validate_population(popn_estimates, col_aggregation = c("gss_code_small_area", "age", "sex", "year"), col_data = "popn")
  # TODO add this back in when the rates are fixed
  #validate_population(adults_per_dwelling, col_aggregation = c("gss_code_small_area", "year"), col_data = "adults_per_dwelling")
  validate_population(out_migration_rates, col_aggregation = c("gss_code_small_area", "age", "sex"), col_data = "out_migration_rate")
  validate_population(in_migration_characteristics, col_aggregation = c("gss_code_small_area", "age", "sex"), col_data = "in_migration_rate")
  validate_population(birth_constraint, col_aggregation = c("gss_code", "age", "year", "sex"), col_data = "births")
  validate_population(death_constraint, col_aggregation = c("gss_code", "age", "sex", "year"), col_data = "deaths")
  validate_population(popn_constraint, col_aggregation = c("gss_code", "age", "sex", "year"), col_data = "popn")
  validate_population(fertility_rates, col_aggregation = c("gss_code", "age", "sex", "year"), col_data = "rate")
  validate_population(mortality_rates, col_aggregation = c("gss_code", "age", "sex", "year"), col_data = "rate")
  validate_population(dwelling_trajectory, col_aggregation = c("gss_code_small_area", "year"), col_data = "units")
  
  # Check geographies are all correct
  domain_small_area <- unique(popn_estimates$gss_code_small_area)
  domain <- unique(popn_estimates$gss_code)
  assert_that(all(domain_small_area %in% adults_per_dwelling$gss_code_small_area))
  assert_that(all(domain_small_area %in% out_migration_rates$gss_code_small_area))
  assert_that(all(domain_small_area %in% in_migration_characteristics$gss_code_small_area))
  assert_that(all(domain %in% birth_constraint$gss_code))
  assert_that(all(domain %in% death_constraint$gss_code))
  assert_that(all(domain %in% fertility_rates$gss_code))
  assert_that(all(domain %in% mortality_rates$gss_code))
  assert_that(all(domain_small_area %in% dwelling_trajectory$gss_code_small_area))
  
  # Check years are all correct
  past_years <- (config_list$first_proj_yr - 1):config_list$last_data_year
  proj_years <- (config_list$last_data_year + 1):config_list$last_proj_yr
  if(config_list$last_proj_yr > config_list$last_data_year) {
    all_years <- c(past_years, proj_years)
  } else {
    all_years <- past_years
  }
  assert_that(all(all_years %in% birth_constraint$year))
  assert_that(all(all_years %in% death_constraint$year))
  assert_that(all(proj_years %in% fertility_rates$year))
  assert_that(all(proj_years %in% mortality_rates$year))
  assert_that(all(all_years %in% dwelling_trajectory$year))
  
}

# -------------------------------------------------------

validate_small_area_fert_mort_components <- function(popn_estimates,
                                                     small_area_fertility_rates,
                                                     small_area_mortality_rates,
                                                     fertility_rates,
                                                     mortality_rates,
                                                     config_list) {
  domain <- unique(popn_estimates$gss_code)
  proj_years <- (config_list$last_data_year + 1):config_list$last_proj_yr
  
  validate_population(small_area_fertility_rates, col_aggregation = c("gss_code_small_area", "age", "sex", "year"), col_data = "fert_rate")
  validate_population(small_area_mortality_rates, col_aggregation = c("gss_code_small_area", "age", "sex", "year"), col_data = "mort_rate")
  assert_that(all(domain %in% fertility_rates$gss_code))
  assert_that(all(domain %in% mortality_rates$gss_code))
  assert_that(all(proj_years %in% fertility_rates$year))
  assert_that(all(proj_years %in% mortality_rates$year)) 
}
