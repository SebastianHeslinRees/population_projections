library(dplyr)

calc_births <- function(aged_popn, fertility) {
  
  # calculate total births from age specific fertility rates and population
  births <- left_join(aged_popn, fertility, by = c("gss_code", "sex", "age", "year")) %>%
    rename(popn = value.x, fertility = value.y) %>%
    mutate(value = popn * fertility) %>%
    group_by(gss_code) %>% summarise(value = sum(value)) %>% as.data.frame()
  
  # assume a 1:1 male:female births ratio
  sexes <- unique(aged_popn$sex)
  n_sexes <- length(sexes)
  
  births <- lapply(sexes, function(x) mutate(births, sex = x, value = value/n_sexes)) %>%
    bind_rows()
  
  
  return(births)
  
}