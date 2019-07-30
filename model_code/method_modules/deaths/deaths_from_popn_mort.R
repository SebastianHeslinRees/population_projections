# TODO: make this a more general CoC from rates, popn
# TODO: add a by argument to the function, with default c("gss_code", "sex", "age", "year")

deaths_from_popn_mort <- function(popn, mortality) {
  library(dplyr)
  
  # TODO: is the next line in the validate join already?
  if (!identical(sort(names(popn)), sort(names(mortality)))) stop("mortality and popn dfs don't have matching names")

  popn <- popn %>% rename(popn = value)
  
  # TODO: validate popn and mortality dfs
  # TODO: validate join
  # TODO: make the join on all except into a function

  join_by <- names(popn)[names(popn) != "popn"]
  deaths <- left_join(popn, mortality, by = join_by) %>%
    mutate(value = popn * value) %>%
    select(-popn)
  
  # TODO: validate deaths df
  return(deaths)
  
}