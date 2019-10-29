library(dplyr)

npp_location <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/"

#population
population <- data.table::fread(paste0(npp_location,"npp_2016_population.csv")) %>%
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
  select(year, sex, age, popn)

#deaths
deaths <- data.table::fread(paste0(npp_location,"npp_2016_deaths.csv")) %>%
  tidyr::gather(year, deaths, 3:102) %>%
  mutate(sex = case_when(Sex == 1 ~ "male",
                         Sex == 2 ~ "female")) %>%
  mutate(age = ifelse(Age == "Birth", -1,
                      ifelse(Age == "105+",105,Age)),
         age = as.numeric(age),
         age = age +1) %>%
  mutate(year = substr(year,8,11),
         year = as.numeric(year)) %>%
  group_by(year, sex, age) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  select(year, sex, age, deaths)

#births
births <- data.table::fread(paste0(npp_location,"npp_2016_births.csv")) %>%
  tidyr::gather(year, births, 3:102) %>%
  mutate(sex = case_when(Sex == 1 ~ "male",
                         Sex == 2 ~ "female")) %>%
  mutate(age = as.numeric(Age)) %>%
  mutate(year = substr(year,8,11),
         year = as.numeric(year)) %>%
  select(year, sex, age, births)


#migration components
mig_constraints <- function(file, component, in_out) {
  
  x <- data.table::fread(paste0(npp_location,file)) %>%
    filter(Flow == in_out) %>%
    tidyr::gather(year, value, 4:103) %>%
    mutate(sex = case_when(Sex == 1 ~ "male",
                           Sex == 2 ~ "female")) %>%
    mutate(age = ifelse(Age == "105+",105,Age),
           age = as.numeric(age)) %>%
    mutate(year = substr(year,8,11),
           year = as.numeric(year)) %>%
    group_by(year, sex, age) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(!!component := value)
  
}

cross_border_in <- mig_constraints("npp_2016_cross_border.csv", "cross_in", "In")
cross_border_out <- mig_constraints("npp_2016_cross_border.csv", "cross_out", "Out")
international_in <- mig_constraints("npp_2016_international.csv", "int_in", "In")
international_out <- mig_constraints("npp_2016_international.csv", "int_out", "Out")


#write output
dir.create("input_data/constraints", recursive = TRUE, showWarnings = FALSE)
saveRDS(population, "input_data/constraints/npp_2016_population_constraint.rds")
saveRDS(births, "input_data/constraints/npp_2016_fertility_constraint.rds")
saveRDS(deaths, "input_data/constraints/npp_2016_mortality_constraint.rds")
saveRDS(international_in, "input_data/constraints/npp_2016_international_in_constraint.rds")
saveRDS(international_out, "input_data/constraints/npp_2016_international_out_constraint.rds")
saveRDS(cross_border_in, "input_data/constraints/npp_2016_cross_border_in_constraint.rds")
saveRDS(cross_border_out, "input_data/constraints/npp_2016_cross_border_out_constraint.rds")

rm(list=ls())
