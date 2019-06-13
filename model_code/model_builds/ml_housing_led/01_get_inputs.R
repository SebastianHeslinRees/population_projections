
get_popn_data <- function() {
  
  popn <- read_rds("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Inputs/2017 base/mye_components - Revised.rds") %>%
    filter(str_detect(gss_code, "^E09"), year == 2017, var == "population") %>%
    rename(value = estimate) %>%
    select(-var)
  
  return(popn)
  
}

get_mortality_data <- function() {
  
  mortality <- read_rds("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Inputs/2017 base/constant inputs/ons_asmr_curves.rds") %>%
    filter(str_detect(gss_code, "^E09")) %>%
    rename(value = death_rate)
  
  mortality <- lapply(2017:2050, function (x) mutate(mortality, year = x)) %>% bind_rows()
  
  return(mortality)
  
}

get_fertility_data <- function() {
  
  fertility <- read_rds("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Inputs/2017 base/constant inputs/smoothed_ons_asfr_curves.rds") %>%
    filter(str_detect(gss_code, "^E09")) %>%
    rename(value = fertility_rate)
  
  fertility <- lapply(2017:2050, function (x) mutate(fertility, year = x)) %>% bind_rows()
  # test that fertility has the correct years
  # test that fertility has the correct column names. 
  
  return(fertility)
  
}

get_development_data <- function() {
  
  load("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/housing data/2016 SHLAA.Rdata")
  development <- borough_dev %>% rename(value = new_homes)
  rm(ward_dev, msoa_dev, borough_dev)
  
  return(development)
  
}