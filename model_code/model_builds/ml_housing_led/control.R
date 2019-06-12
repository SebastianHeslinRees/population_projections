run_ml_housing_led <- function(ahs, n_proj_yr) {

  
library(tidyverse)
  
# get the input data
source("model_code/model_builds/ml_housing_led/get_input_data.R")
start_popn <- get_popn_data()
fertility <- get_fertility_data()
mortality <- get_mortality_data()
development <- get_development_data()

source("model_code/model_builds/ml_housing_led/get_input_data.R")
# test the input data

#test_popn_data(start_popn)
#test_fertility_data(fertility)
#test_mortality_data(mortality)
#test_development_data(development)

# run the model
source("model_code/model_builds/ml_housing_led/core.R")
projection <- ml_housing_led(start_popn, fertility, mortality, development, ahs, n_proj_yr)

# write the output data
source("model_code/model_builds/ml_housing_led/output.R") # Should this be a shared module? 
output_projection(projection, "outputs/ml_housing_led")

# source("model_code/qa/qa_something.R")
# qa(which_cuts_to_include)
  
}