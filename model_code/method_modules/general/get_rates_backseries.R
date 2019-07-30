# divides a component of change by population to give a dataframe of rates
# takes filepaths to the CoC and popn, and returns a rates df.

get_rates_backseries <- function(coc_mye_path, popn_mye_path) {
  library(dplyr)
  
  # population is aged on due to definitions of MYE to ensure the correct denominator
  # population in population at 30th June
  # changes are changes that occured in the 12 months up to 30th June
  # age is the age the cohort is at 30th June
  # TODO add link to ONS documentation for the above
  
  # TODO this should call an age on function instead. Pass the function in through the args
  popn <- readRDS(popn_mye_path) %>%
    rename(popn = value) %>%
    mutate(year = year + 1, age = age + 1)
  
  # TODO validate popn
  
  coc <- readRDS(coc_mye_path) %>%
    rename(coc = value)
  # TODO validate coc
  
  # TODO validate join
  # TODO be more specific about the join? Yes, pass in the expected col names into the function
  # TODO split the rates calculation out into a separate function
  # TODO check the methodological decision to set value to 0 if popn is 0
  rates <- suppressMessages(left_join(coc, popn) %>%
    mutate(value = coc/popn, value = ifelse(popn == 0, 0, value)) %>%
    select(-popn, -coc))
  
  # TODO validate rates
  
  # TODO add module function rules checks - must return fert/mort df
  
}
