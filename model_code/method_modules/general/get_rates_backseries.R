# divides a component of change by population to give a dataframe of rates
# takes filepaths to the CoC and popn, and returns a rates df.

get_rates_backseries <- function(coc_mye_path, popn_mye_path) {
  library(dplyr)
  
  popn <- readRDS(popn_mye_path) %>%
    rename(popn = value)
  # TODO validate popn
  
  coc <- readRDS(coc_mye_path)
  # TODO validate coc
  
  # TODO validate join
  # TODO be more specific about the join?
  # TODO split the rates calculation out into a separate function
  
  rates <- suppressMessages(left_join(coc, popn) %>%
    mutate(value = value/popn) %>%
    select(-popn))
  
  # TODO validate rates
  
  # TODO add module function rules checks - must return fert/mort df
  
}
