####YEARS WITH BIRTH DATA####
#Borough rates are applied to all wards in the borough
#Resulting births are compared to actuals
#Scaling factors are produced for fertility rates

####YEARS WITH NO DATA####
#Calculate the geometric mean of all the scaling factors
#Apply it to the borough projected fertility rates
devtools::load_all('model_code/popmodules')
source('model_code/model_scripts/small_area/small_area_core.R')
source('model_code/model_scripts/small_area/arrange_small_area_core_outputs.R')

#TODO Remove once the branch with this function is merged in
source('M:/Temp/distribute_within_age_band.R')
#source('~/Dropbox/distribute_within_age_band.R')

read_small_area_inputs <- function(path){
  df <- readRDS(path)
  if("gss_code_ward" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_ward)}
  if("gss_code_msoa" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_msoa)}
  return(df)
}

#Read Data
#CTODO Change this to path variables read into the function
popn_estimates <- read_small_area_inputs('input_data/small_area_model/ward_population_estimates_2010_2017.rds')
communal_est_popn  <- read_small_area_inputs('input_data/small_area_model/ward_communal_establishment_population.rds')
out_migration_rates <- read_small_area_inputs('input_data/small_area_model/ward_out_migration_rates.rds')
in_migration_characteristics <- read_small_area_inputs('input_data/small_area_model/ward_in_migration_characteristics.rds')
births <- read_small_area_inputs('input_data/small_area_model/ward_births_2001_2018.rds')
deaths <- read_small_area_inputs('input_data/small_area_model/ward_deaths_2001_2018.rds')
ldd_data <- read_small_area_inputs('input_data/small_area_model/ldd_backseries_dwellings_ward.rds')
dwelling_trajectory <- read_small_area_inputs('input_data/small_area_model/ward_shlaa_trajectory.rds')
adults_per_dwelling <- read_small_area_inputs("input_data/small_area_model/ward_adults_per_dwelling.rds") 
ward_to_district <- read_small_area_inputs("input_data/lookup/2011_ward_to_district.rds")

#----------

#TODO should the housing led model output include the backseries for the components?
#Would save having to read 2 file here and would probably be useful in other ways
births_past <- readRDS("input_data/mye/2018/births_ons.rds") %>% select(year, gss_code, age, sex, births) %>% filter(age==0)
deaths_past <- readRDS("input_data/mye/2018/deaths_ons.rds") %>% select(year, gss_code, age, sex, deaths)
popn_past <- readRDS("input_data/mye/2018/population_gla_2019-11-13.rds") %>% select(year, gss_code, age, sex, popn)

birth_constraint <- readRDS("outputs/housing_led/2018/test/births_19-12-18_1534.rds") %>% filter(substr(gss_code,1,3)=="E09") %>%
  rbind(births_past)
death_constraint <- readRDS("outputs/housing_led/2018/test/deaths_19-12-18_1534.rds") %>% filter(substr(gss_code,1,3)=="E09") %>%
  rbind(deaths_past)
popn_constraint <- readRDS("outputs/housing_led/2018/test/population_19-12-18_1534.rds") %>% filter(substr(gss_code,1,3)=="E09") %>%
  rbind(popn_past)

rm(births_past, deaths_past, popn_past)
#------------

#TODO THink about a variable that sets the trend projection rather than setting specific paths
#This is also linked to how the borough-level and small area models interact
#i.e. how 'stand alone' should this model be or is it always run with the borough level model
fertility_rates <- readRDS("outputs/trend/2018/2018_central/fertility_rates_19-11-13_2056.rds") %>% filter(substr(gss_code,1,3)=="E09")
mortality_rates <- readRDS("outputs/trend/2018/2018_central/mortality_rates_19-11-13_2056.rds") %>% filter(substr(gss_code,1,3)=="E09")

#Random variables I needed to make things work 
#TODO think about names
last_data_year <- 2018
first_proj_year <- 2018
final_proj_year <- 2021
birth_rate_n_years_to_avg <- 5
death_rate_n_years_to_avg <- 5

#There should be some checks here that everything is present
dwelling_trajectory <- filter(dwelling_trajectory, !year %in% unique(ldd_data$year)) %>%
  rbind(ldd_data) %>%
  arrange(gss_code_small_area, year)

#-------------------------

#Projection loop
curr_yr_popn <- filter(popn_estimates, year == first_proj_year-1)
projection <- list()

for(projection_year in first_proj_year:final_proj_year){
  #projection_year <- first_proj_year
  message(projection_year)
  
  curr_yr_popn_constraint <- filter(popn_constraint, year == projection_year)
  curr_yr_birth_constraint <- filter(birth_constraint, year == projection_year)
  curr_yr_death_constraint <- filter(death_constraint, year == projection_year)
  curr_yr_dwellings <- filter(dwelling_trajectory, year == projection_year)
  
  if(projection_year == last_data_year+1){
    
    #TODO make it work with age groups
    #Scaling factors for the 2019 rates and then applied to the fertility trajectory
    
    curr_yr_births <- dtplyr::lazy_dt(births) %>%
      group_by(year, gss_code_small_area) %>%
      summarise(births = sum(births)) %>%
      as.data.frame() %>%
      mutate(year = as.numeric(year))
    
    future_fertility_rates <- filter(fertility_rates, year == last_data_year+1) %>%
      select(-year)
    
    births_data_years <- (last_data_year-birth_rate_n_years_to_avg+1):last_data_year
    
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
    
    future_mortality_rates <- filter(mortality_rates, year == last_data_year+1) %>%
      select(-year)
    
    deaths_data_years <- (last_data_year-death_rate_n_years_to_avg+1):last_data_year
    
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
  
  if(projection_year > last_data_year){
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
                                                   last_data_year = last_data_year,
                                                   dwellings = curr_yr_dwellings,
                                                   adults_per_dwelling = curr_yr_adults_per_dwelling,
                                                   projection_year)
  
  curr_yr_popn <- projection[[projection_year]][['population']]
  
  if(projection_year <= last_data_year){
    popn_estimates <- filter(popn_estimates, year != projection_year) %>%
      rbind(curr_yr_popn)
  }
  
}

projection <- arrange_small_area_core_outputs(projection, first_proj_year, final_proj_year)
saveRDS(projection, "outputs/ward/test_projection.rds")

