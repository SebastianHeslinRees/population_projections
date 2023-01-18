#NPP Mortality Trend data
library(dplyr)
library(tidyr)
library(data.table)
library(tidyr)

message('NPP fertility trend')

max_year <- 2050
npp_data_2016 <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/"
npp_data_2018 <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2018-based NPP/model_inputs/"
npp_data_2020 <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2020-based NPP/model_inputs/"

#function to read and wrangle raw data
fertility_trend <- function(file, var, max_year, npp_data_location){
  fert <- fread(paste0(npp_data_location, file)) %>%
    tibble() %>%
    pivot_longer(names_to = "year", values_to = "rate", cols = 3:102) %>%
    mutate(sex = ifelse(Sex == 1, "male", "female")) %>%
    mutate(age = as.numeric(Age))%>%
    select(-Sex, -Age) %>%
    mutate(year = as.numeric(substr(year, 8,11))) %>%
    filter(year <= max_year) %>%
    select(sex, age, year, rate)
  
  back_to_2001 <- list()
  min_year <- min(fert$year)
  for(y in 2001:(min_year - 1)){
    back_to_2001[[y]] <- filter(fert, year == min_year) %>% mutate(year = y)
  }
  
  fert <- rbind(data.table::rbindlist(back_to_2001), fert) %>%
    data.frame()
  
  additional_ages <- list()
  for(a in 47:49){
    additional_ages[[a]] <- filter(fert, age == max(fert$age)) %>% mutate(age = a)
  }
  
  fert <- rbind(data.table::rbindlist(additional_ages), fert) %>%
    data.frame() %>%
    mutate(variant = var)
  
  return(fert)
  
}

#read in data
principal_2016 <- fertility_trend("Principal Fertility Assumptions.csv", "2016_principal", max_year, npp_data_2016)
high_2016 <- fertility_trend("High Fertility Assumptions.csv", "2016_high", max_year, npp_data_2016)
low_2016 <- fertility_trend("Low Fertility Assumptions.csv", "2016_low", max_year, npp_data_2016)

principal_2018 <- fertility_trend("fertility_principal.csv", "2018_principal", max_year, npp_data_2018)
high_2018 <- fertility_trend("fertility_high.csv", "2018_high", max_year, npp_data_2018)
low_2018 <- fertility_trend("fertility_low.csv", "2018_low", max_year, npp_data_2018)

principal_2020 <- fertility_trend("principal_fertility.csv", "2020_principal", max_year, npp_data_2020)

#bind the variants together
fert_trend <- rbind(principal_2016, high_2016, low_2016,
                    principal_2018, high_2018, low_2018,
                    principal_2020) 

#2012 data
load("Q:/Teams/D&PA/Demography/Projections/Legacy Models/Trend Model - original/Inputs/2016 base/CCM Data Inputs - UPC.RData")
rm(list=setdiff(ls(),c("fert_trend", "npp_fertility_trend")))

trend_2012 <- select(npp_fertility_trend, year, age, High.2012, Low.2012, Principal.2012) %>%
  pivot_longer(names_to = "variant", values_to = "rate", cols = c("High.2012", "Low.2012", "Principal.2012")) %>%
  mutate(sex = "female") %>%
  select(names(fert_trend)) %>%
  filter(year <= max(fert_trend$year)) %>%
  mutate(variant = case_when(variant == "High.2012" ~ "2012_high",
                             variant == "Low.2012"~ "2012_low",
                             variant == "Principal.2012" ~ "2012_principal"))


fert_trend <- fert_trend %>%
  rbind(trend_2012) %>%
  arrange(variant, sex, age, year) %>%
  mutate(last_year = lag(rate)) %>%
  mutate(change = (rate - last_year)/last_year)%>%
  mutate(change = ifelse(year == min(year),0,change)) %>%
  select(-rate, -last_year)

#The 2016 and 2018 data have different years
# popmodules::validate_population(fert_trend, 
#                                 col_aggregation = c("sex", "age", "year", "variant"), 
#                                 col_data = "change", 
#                                 check_negative_values = FALSE)

#write output
dir.create("input_data/fertility", recursive = TRUE, showWarnings = FALSE)
saveRDS(fert_trend, "input_data/fertility/npp_fertility_trend.rds" )

rm(list=ls())
