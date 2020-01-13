####YEARS WITH BIRTH DATA####
#Borough rates are applied to all wards in the borough
#Resulting births are compared to actuals
#Scaling factors are produced for fertility rates

####YEARS WITH NO DATA####
#Calculate the geometric mean of all the scaling factors
#Apply it to the borough projected fertility rates
devtools::load_all('model_code/popmodules')

read_small_area_inputs <- function(path){
  df <- readRDS(path)
  if("gss_code_ward" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_ward)}
  if("gss_code_msoa" %in% names(df)){df <- rename(df, gss_code_small_area = gss_code_msoa)}
  return(df)
}

#Read Data
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

fertility_rates <- readRDS("outputs/trend/2018/2018_central/fertility_rates_19-11-13_2056.rds") %>% filter(substr(gss_code,1,3)=="E09")
mortality_rates <- readRDS("outputs/trend/2018/2018_central/mortality_rates_19-11-13_2056.rds") %>% filter(substr(gss_code,1,3)=="E09")

last_data_year <- 2018
first_proj_yr <- 2018
final_proj_year <- 2021
birth_rate_n_years_to_avg <- 5

dwelling_trajectory <- filter(dwelling_trajectory, !year %in% unique(ldd_data$year)) %>%
  rbind(ldd_data) %>%
  arrange(gss_code_small_area, year)

#Jump off year borough totals should match constraint - check?

#Projection loop
curr_yr_popn <- filter(popn_estimates, year == first_proj_yr-1)
projection <- list()

#for(projection_year in first_proj_yr:final_proj_year){
projection_year <- first_proj_yr

curr_yr_popn_constraint <- filter(popn_constraint, year == projection_year)
curr_yr_birth_constraint <- filter(birth_constraint, year == projection_year)
curr_yr_death_constraint <- filter(death_constraint, year == projection_year)
curr_yr_dwellings <- filter(dwelling_trajectory, year == projection_year)

if(projection_year == last_data_year+1){
  
  #TODO make it work with age groups
  #TODO make sure the columns that need to be numeric are numeric in the input scripts
  #Scaling factors for the 2019 rates and then applied to the fertility trajectory
  births <- births %>%
    group_by(year, gss_code_small_area) %>%
    summarise(births = sum(births)) %>%
    as.data.frame() %>%
    mutate(year = as.numeric(year))
  
  deaths <- deaths %>%
    group_by(year, gss_code_small_area) %>%
    summarise(deaths = sum(deaths)) %>%
    as.data.frame() %>%
    mutate(year = as.numeric(year))
  
  fertility_rates_2019 <- filter(fertility_rates, year == 2019) %>% select(-year)
  mortality_rates_2019 <- filter(mortality_rates, year == 2019) %>% select(-year)
  
  data_years <- (max(births$year)-birth_rate_n_years_to_avg+1):max(births$year)
  
  fert_scaling <- popn_age_on(as.data.frame(popn_estimates),
                              col_aggregation = c("year", "gss_code_small_area", "age", "sex")) %>%
    filter(year %in% data_years) %>%
    # apply_rate_to_population(popn_rate = fertility_rates_2019,
    #                          col_aggregation = c(nesting("gss_code","gss_code_small_area"),"sex","age"),
    #                          col_popn = "popn",
    #                          col_rate = "rate",
    #                          col_out = "births")
    left_join(fertility_rates_2019, by=c("gss_code","sex","age")) %>%
    mutate(births = rate*popn) %>%
    group_by(year, gss_code, gss_code_small_area) %>%
    summarise(births_from_rate = sum(births)) %>%
    as.data.frame()%>%
    calculate_scaling_factors(constraint = births,
                              col_aggregation = c("year","gss_code_small_area"),
                              col_popn = "births_from_rate",
                              col_constraint = "births",
                              rows_to_constrain = TRUE) %>%
    dtplyr::lazy_dt() %>%
    group_by(gss_code_small_area) %>%
    summarise(scaling = EnvStats::geoMean(scaling)) %>%
    as.data.frame() %>%
    left_join(ward_to_district, by="gss_code_small_area")
  
  mort_scaling <- popn_age_on(as.data.frame(popn_estimates),
                              col_aggregation = c("year", "gss_code_small_area", "age", "sex")) %>%
    filter(year %in% data_years) %>%
    # apply_rate_to_population(popn_rate = fertility_rates_2019,
    #                          col_aggregation = c(nesting("gss_code","gss_code_small_area"),"sex","age"),
    #                          col_popn = "popn",
    #                          col_rate = "rate",
    #                          col_out = "births")
    left_join(mortality_rates_2019, by=c("gss_code","sex","age")) %>%
    mutate(deaths = rate*popn) %>%
    group_by(year, gss_code, gss_code_small_area) %>%
    summarise(deaths_from_rate = sum(deaths)) %>%
    as.data.frame()%>%
    calculate_scaling_factors(constraint = deaths,
                              col_aggregation = c("year","gss_code_small_area"),
                              col_popn = "deaths_from_rate",
                              col_constraint = "deaths",
                              rows_to_constrain = TRUE) %>%
    dtplyr::lazy_dt() %>%
    group_by(gss_code_small_area) %>%
    summarise(scaling = EnvStats::geoMean(scaling)) %>%
    as.data.frame() %>%
    left_join(ward_to_district, by="gss_code_small_area")
  
  #TODO apply_rate_to_population()
  small_area_fertility_rates <- left_join(fertility_rates, fert_scaling, by=c("gss_code")) %>%
    mutate(fert_rate = scaling*rate)
  
  small_area_mortality_rates <- left_join(mortality_rates, mort_scaling, by=c("gss_code")) %>%
    mutate(mort_rate = scaling*rate)
}

if(projection_year > max(adults_per_dwelling$year)){
  curr_yr_adults_per_dwelling <- filter(adults_per_dwelling, year == max(year)) %>%
    select(gss_code_small_area, adults_per_dwelling)
} else {
  curr_yr_adults_per_dwelling <- filter(adults_per_dwelling, year == projection_year) %>%
    select(gss_code_small_area, adults_per_dwelling)
}


if(projection_year > last_data_year){
  curr_yr_fertiity <- filter(small_area_fertility_rates, year == projection_year)
  curr_yr_mortality <- filter(small_area_mortality_rates, year == projection_year)
} else {
  curr_yr_fertility <- NULL
  curr_yr_mortality <- filter(mortality_rates, year == 2019) %>% select(-year)
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
                                                 dwellings = curr_yr_dwellings,
                                                 adults_per_dwelling = curr_yr_adults_per_dwelling,
                                                 projection_year) 

curr_yr_popn <- projection[[projection_year]][['population']]

if(projection_year <= last_data_year){
  popn_estimates <- filter(popn_estimates, year != projection_year) %>%
    rbind(curr_yr_popn)
}

}
