# This is designed to work with the fertility/mortality function chains.
# The control module passes a list of filepaths to functions, and the function arguments as part of the 'plug and play' design.
# This function replaces the filepath for the actual function so that functions can be run from the list. 

filepath2function <- function(chain_element, filepath_element) {
  source("model_code/helper_functions/assign_function.R")
  chain_element[[filepath_element]] <- assign_function(chain_element[[filepath_element]])
  return(chain_element)
}