devtools::load_all('model_code/popmodules')
source('model_code/model_scripts/small_area/small_area_core.R')
source('model_code/model_scripts/small_area/arrange_small_area_core_outputs.R')

#TODO Remove once the branch with this function is merged in
source('M:/Temp/distribute_within_age_band.R')


run_small_area_model <- function(config_list){
 
  expected_config <- c("small_area_popn_estimates_path",
                       "small_area_communal_est_popn_path",
                       "small_area_births_backseries_path",
                       "small_area_deaths_backseries_path",
                       "small_area_ldd_data_path",
                       "small_area_dev_trajectory_path",
                       "housing_led_model_path",
                       "housing_led_model_timestamp",
                       "borough_fertility_rates_path",
                       "borough_mortality_rates_path",
                       "last_data_year",
                       "first_proj_yr",
                       "final_proj_yr",
                       "birth_rate_n_years_to_avg",
                       "death_rate_n_years_to_avg",
                       "projection_type",
                       "projection_name",
                       "small_area_output_dir")
  
  if(!identical(sort(names(config_list)),  sort(expected_config))) stop("configuration list is not as expected")
  
  read_small_area_inputs <- function(path){
    df <- readRDS(path)
    if("gss_code_ward" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_ward)}
    if("gss_code_msoa" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_msoa)}
    return(df)
  }
  
  #Read Data
  adults_per_dwelling <- read_small_area_inputs("input_data/small_area_model/ward_adults_per_dwelling.rds") 
  ward_to_district <- read_small_area_inputs("input_data/lookup/2011_ward_to_district.rds")
  out_migration_rates <- read_small_area_inputs('input_data/small_area_model/ward_out_migration_rates.rds')
  in_migration_characteristics <- read_small_area_inputs('input_data/small_area_model/ward_in_migration_characteristics.rds')
  
  popn_estimates <- read_small_area_inputs(config_list$small_area_popn_estimates_path)
  communal_est_popn  <- read_small_area_inputs(config_list$small_area_communal_est_popn_path)
  births <- read_small_area_inputs(config_list$small_area_births_backseries_path)
  deaths <- read_small_area_inputs(config_list$small_area_deaths_backseries_path)
  ldd_data <- read_small_area_inputs(config_list$small_area_ldd_data_path)
  dwelling_trajectory <- read_small_area_inputs(config_list$small_area_dev_trajectory_path)
  
  #----------
  
  #TODO should the housing led model output include the backseries for the components?
  #Would save having to read 2 file here and would probably be useful in other ways
  births_past <- readRDS("input_data/mye/2018/births_ons.rds") %>%
    select(year, gss_code, age, sex, births) %>% filter(age==0)
  deaths_past <- readRDS("input_data/mye/2018/deaths_ons.rds") %>%
    select(year, gss_code, age, sex, deaths)
  popn_past <- readRDS("input_data/mye/2018/population_gla_2019-11-13.rds") %>%
    select(year, gss_code, age, sex, popn)
  
  #TODO Change the housing-led model output names to make this easier (ie remove timestamp)
  birth_constraint <- readRDS(paste0(config_list$housing_led_model_path, "births_", config_list$housing_led_model_timestamp, ".rds")) %>%
    filter(substr(gss_code,1,3)=="E09") %>%
    rbind(births_past)
  death_constraint <- readRDS(paste0(config_list$housing_led_model_path, "deaths_", config_list$housing_led_model_timestamp, ".rds")) %>%
    filter(substr(gss_code,1,3)=="E09") %>%
    rbind(deaths_past)
  popn_constraint <- readRDS(paste0(config_list$housing_led_model_path,"population_", config_list$housing_led_model_timestamp, ".rds")) %>%
    filter(substr(gss_code,1,3)=="E09") %>%
    rbind(popn_past)
  
  rm(births_past, deaths_past, popn_past)
  #------------
  
  #TODO THink about a variable that sets the trend projection rather than setting specific paths
  #This is also linked to how the borough-level and small area models interact
  #i.e. how 'stand alone' should this model be or is it always run with the borough level model
  fertility_rates <- readRDS(config_list$borough_fertility_rates_path) %>%
    filter(substr(gss_code,1,3)=="E09")
  mortality_rates <- readRDS(config_list$borough_mortality_rates_path) %>%
    filter(substr(gss_code,1,3)=="E09")
  
  #-------------------------
  
  #Create the cumulative development trajectory
  final_ldd_year <- max(ldd_data$year)
  
  last_ldd_year_data <- filter(ldd_data, year == final_ldd_year) %>%
    select(-year)
  
  cumulative_future_dev <- dwelling_trajectory %>%
    filter(year > final_ldd_year) %>%
    group_by(gss_code_small_area) %>%
    mutate(cum_units = cumsum(units)) %>%
    as.data.frame() %>%
    select(-units) %>%
    left_join(last_ldd_year_data, by="gss_code_small_area") %>%
    mutate(total_units = units + cum_units) %>%
    select(year, gss_code_small_area, units = total_units)
  
  dwelling_trajectory <- rbind(ldd_data, cumulative_future_dev) %>%
    arrange(gss_code_small_area, year)
  
  rm(cumulative_future_dev)
  
  #-------------------------
  
  #Projection loop
  curr_yr_popn <- filter(popn_estimates, year == config_list$first_proj_yr-1)
  projection <- list()
  
  for(projection_year in config_list$first_proj_yr:config_list$final_proj_yr){
    
    cat('\r',paste("  Projecting year", projection_year))
    flush.console()
    
    curr_yr_popn_constraint <- filter(popn_constraint, year == projection_year)
    curr_yr_birth_constraint <- filter(birth_constraint, year == projection_year)
    curr_yr_death_constraint <- filter(death_constraint, year == projection_year)
    curr_yr_dwellings <- filter(dwelling_trajectory, year == projection_year)
    
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
      small_area_fertility_rates <- left_join(fertility_scaling, ward_to_district,
                                              by="gss_code_small_area") %>%
        left_join(fertility_rates, by=c("gss_code")) %>%
        mutate(fert_rate = scaling*rate)
      
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
      small_area_mortality_rates <- left_join(mortality_scaling, ward_to_district,
                                              by="gss_code_small_area") %>%
        left_join(mortality_rates, by=c("gss_code")) %>%
        mutate(mort_rate = scaling*rate)%>%
        select(year, gss_code_small_area, sex, age, mort_rate)
      
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
    
    #Adults per dwelling
    
    if(projection_year > max(adults_per_dwelling$year)){
      curr_yr_adults_per_dwelling <- filter(adults_per_dwelling, year == max(year)) %>%
        select(gss_code_small_area, adults_per_dwelling)
    } else {
      curr_yr_adults_per_dwelling <- filter(adults_per_dwelling, year == projection_year) %>%
        select(gss_code_small_area, adults_per_dwelling)
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
                                                     projection_year,
                                                     ward_to_district)
    
    curr_yr_popn <- projection[[projection_year]][['population']]
    
    if(projection_year <= config_list$last_data_year){
      popn_estimates <- filter(popn_estimates, year != projection_year) %>%
        rbind(curr_yr_popn)
    }
    
  }
  message(" ")
  
  projection <- arrange_small_area_core_outputs(projection, config_list$first_proj_yr, config_list$final_proj_yr)
  saveRDS(projection, paste0(config_list$small_area_output_dir, config_list$projection_name, ".rds"))
  
}