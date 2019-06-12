library(dplyr)

# get the modeules for the model

source("model_code/method_modules/age_on/age_on_sya.R") # age_on

source("model_code/method_modules/births/births_from_popn_fert.R") # calc_births()
source("model_code/method_modules/births/add_births_to_aged_popn.R") # add_births()

source("model_code/method_modules/deaths/deaths_from_popn_mort.R") # calc_deaths()
source("model_code/method_modules/deaths/remove_deaths.R") # remove_deaths()

source("model_code/method_modules/housing/simple_popn_from_dev.R") # calc_housing_popn()
source("model_code/method_modules/housing/add_base_to_dev_popn.R") # add_base_to_dev_popn()
source("model_code/method_modules/constraint/scale_to_target_popn.R") # scale_to_target_popn()
 
# run the ml_housing_led model
ml_housing_led <- function(start_popn, fertility, mortality, development, ahs, n_proj_yr) {
  first_proj_yr <- unique(start_popn$year) + 1
  last_proj_yr <-  unique(start_popn$year) + n_proj_yr
  
  curr_yr_popn <- start_popn
  
  for (my_year in first_proj_yr:last_proj_yr) {
    
    aged_popn <- curr_yr_popn %>%
      age_on() 
    
    births <- calc_births(aged_popn, fertility)
    aged_popn_b <- add_births(aged_popn, births)
    # test: aged_popn_b should be same size and shape as start_popn
    
    deaths <- calc_deaths(aged_popn_b, mortality)
    aged_popn_bd <- remove_deaths(aged_popn_b, deaths)
    # test: aged_popn_bd should be same size and shape as start_popn
    
    housing_popn <- calc_housing_popn(development, ahs)
    target_popn <- add_base_to_dev_popn(curr_yr_popn, housing_popn)
    aged_popn_bdh <- scale_to_target_popn(aged_popn_bd, target_popn)
    # test: aged_popn_bdh should be same size and shape as start_popn
    
    next_yr_popn <- aged_popn_bdh
    # test: next_yr_popn should be same size and shape as start_popn
    
    # initialise output dataframes during first round
    if (my_year == first_proj_yr) {
    proj_popn <- start_popn
    proj_births <- births
    proj_deaths <- deaths
    proj_housing_popn <- housing_popn
    }
    
    proj_popn <- bind_rows(proj_popn, next_yr_popn)
    proj_births <- bind_rows(proj_births, births)
    proj_deaths <- bind_rows(proj_births, deaths)
    proj_housing_popn <- bind_rows(proj_housing_popn, housing_popn)

    curr_yr_popn <- next_yr_popn
    
  }
  
  return(list(proj_popn = proj_popn, proj_births = proj_births, proj_deaths = proj_deaths, proj_housing_popn = proj_housing_popn))
  
}
