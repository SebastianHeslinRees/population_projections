library(dplyr)

message('2018-based NPP constraints')

npp_location <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2018-based NPP/model_inputs/"

#migration components
mig_constraints <- function(file, component, in_out, country_id) {
  
  x <- data.table::fread(paste0(npp_location,file)) %>%
    tibble() %>%
    filter(Flow == in_out) %>%
    tidyr::gather(year, value, 4:103) %>%
    mutate(sex = case_when(Sex == 1 ~ "male",
                           Sex == 2 ~ "female")) %>%
    mutate(age = ifelse(Age == "105+",105,Age),
           age = as.numeric(age)) %>%
    mutate(year = substr(year,8,11),
           year = as.numeric(year),
           age = ifelse(age > 90, 90, age)) %>%
    group_by(year, sex, age) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(!!component := value) %>%
    mutate(country = country_id) %>%
    select(country, year, sex, age, !!component)
}

population <- list()
deaths <- list()
births <- list()
cross_border_in <- list()
cross_border_out <- list()
international_in <- list()
international_out <- list()
country_suffix <- c("eng","wales","scotland","ni")
country_id <- c("E","W","S","N")

for(i in 1:4){
  
  #population
  population[[i]] <- data.table::fread(paste0(npp_location,"npp_2018_population_",country_suffix[[i]],".csv")) %>%
    tibble() %>%
    tidyr::gather(year, popn, 3:103) %>%
    mutate(sex = case_when(Sex == 1 ~ "male",
                           Sex == 2 ~ "female")) %>%
    mutate(age = substr(Age, 1, 3),
           age = as.numeric(age),
           age = ifelse(age < 90, age, 90)) %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year, sex, age) %>%
    summarise(popn = sum(popn)) %>%
    ungroup() %>%
    mutate(country = country_id[[i]]) %>%
    select(country, year, sex, age, popn)
  
  #deaths
  deaths[[i]] <- data.table::fread(paste0(npp_location,"npp_2018_deaths_",country_suffix[[i]],".csv")) %>%
    tibble() %>%
    tidyr::gather(year, deaths, 3:102) %>%
    mutate(sex = case_when(Sex == 1 ~ "male",
                           Sex == 2 ~ "female")) %>%
    mutate(age = ifelse(Age == "Birth", -1,
                        ifelse(Age == "105+",105,Age)),
           age = as.numeric(age),
           age = age +1) %>%
    mutate(year = substr(year,8,11),
           year = as.numeric(year),
           age = ifelse(age > 90, 90, age)) %>%
    group_by(year, sex, age) %>%
    summarise(deaths = sum(deaths)) %>%
    ungroup() %>%
    mutate(country = country_id[[i]]) %>%
    select(country, year, sex, age, deaths)
  
  #births
  births[[i]] <- data.table::fread(paste0(npp_location,"npp_2018_births_",country_suffix[[i]],".csv")) %>%
    tidyr::gather(year, births, 3:102) %>%
    mutate(sex = case_when(Sex == 1 ~ "male",
                           Sex == 2 ~ "female")) %>%
    mutate(age = as.numeric(Age)) %>%
    mutate(year = substr(year,8,11),
           year = as.numeric(year)) %>%
    mutate(country = country_id[[i]]) %>%
    select(country, year, sex, age, births)

    cross_border_in[[i]] <- mig_constraints(paste0("npp_2018_cross_border_",country_suffix[[i]],".csv"), "cross_in", "In", country_id[[i]])
    cross_border_out[[i]] <- mig_constraints(paste0("npp_2018_cross_border_",country_suffix[[i]],".csv"), "cross_out", "Out", country_id[[i]])
    international_in[[i]] <- mig_constraints(paste0("npp_2018_international_",country_suffix[[i]],".csv"), "int_in", "In", country_id[[i]])
    international_out[[i]] <- mig_constraints(paste0("npp_2018_international_",country_suffix[[i]],".csv"), "int_out", "Out", country_id[[i]])
}

population <- data.table::rbindlist(population) %>% data.frame()
births <- data.table::rbindlist(births) %>% data.frame()
deaths <- data.table::rbindlist(deaths) %>% data.frame()
cross_border_in <- data.table::rbindlist(cross_border_in) %>% data.frame()
cross_border_out <- data.table::rbindlist(cross_border_out) %>% data.frame()
international_in <- data.table::rbindlist(international_in) %>% data.frame()
international_out <- data.table::rbindlist(international_out) %>% data.frame()


#write output
dir.create("input_data/constraints", recursive = TRUE, showWarnings = FALSE)
saveRDS(population, "input_data/constraints/npp_2018_population_constraint.rds")
saveRDS(births, "input_data/constraints/npp_2018_fertility_constraint.rds")
saveRDS(deaths, "input_data/constraints/npp_2018_mortality_constraint.rds")
saveRDS(international_in, "input_data/constraints/npp_2018_international_in_constraint.rds")
saveRDS(international_out, "input_data/constraints/npp_2018_international_out_constraint.rds")
saveRDS(cross_border_in, "input_data/constraints/npp_2018_cross_border_in_constraint.rds")
saveRDS(cross_border_out, "input_data/constraints/npp_2018_cross_border_out_constraint.rds")

rm(list=ls())
#