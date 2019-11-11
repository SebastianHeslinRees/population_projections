#NPP Mortality Trend data
library(dplyr)
library(tidyr)
library(data.table)
max_year <- 2050
npp_data_2016 <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs/"
npp_data_2018 <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2018-based NPP/model_inputs/"


#function to read and wrangle raw data
mortality_trend <- function(file, var, max_year, npp_data_location){
  mort <- fread(paste0(npp_data_location, file)) %>%
    gather(year, rate, 3:102) %>%
    mutate(sex = ifelse(Sex == 1, "male", "female")) %>%
    mutate(age = ifelse(Age == "Birth", -1, Age),
           age = as.numeric(age))%>%
    select(-Sex, -Age) %>%
    mutate(year = as.numeric(substr(year, 8,11))) %>%
    filter(year <= max_year) %>%
    select(sex, age, year, rate)
  
  back_to_2001 <- list()
  min_year <- min(mort$year)
  for(y in 2001:(min_year -1)){
    back_to_2001[[y]] <- filter(mort, year == min_year) %>%
      mutate(year = y)
  }
  
  mort <- rbind(data.table::rbindlist(back_to_2001), mort) %>%
    mutate(variant = var)
  
  return(mort)
  
}

#read in data
principal_2016 <- mortality_trend("Principal Mortality Assumptions.csv", "2016_principal", max_year, npp_data_2016)
high_2016 <- mortality_trend("High Mortality Assumptions.csv", "2016_high", max_year, npp_data_2016)
low_2016 <- mortality_trend("Low Mortality Assumptions.csv", "2016_low", max_year, npp_data_2016)

principal_2018 <- mortality_trend("mortality_principal.csv", "2018_principal", max_year, npp_data_2018)
high_2018 <- mortality_trend("mortality_high.csv", "2018_high", max_year, npp_data_2018)
low_2018 <- mortality_trend("mortality_low.csv", "2018_low", max_year, npp_data_2018)


#bind the variants together
mort_trend <- rbind(principal_2016, high_2016, low_2016,
                    principal_2018, high_2018, low_2018)

#bind the variants together
mort_trend <- rbind(principal_2016, high_2016, low_2016,
                    principal_2018, high_2018, low_2018) 

#2012 data
load("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model - original/Inputs/2016 base/CCM Data Inputs - UPC.RData")
rm(list=setdiff(ls(),c("mort_trend", "npp_mortality_trend")))

trend_2012 <- npp_mortality_trend %>%
  gather(variant, rate, c(High, Low, Principal)) %>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male")) %>%
  select(names(mort_trend)) %>%
  mutate(variant = case_when(variant == "High" ~ "2014_high",
                             variant == "Low"~ "2014_low",
                             variant == "Principal" ~ "2014_principal"))


mort_trend <- mort_trend %>%
  rbind(trend_2012) %>%
  mutate(age = age + 1) %>%
  filter(age %in% c(0:90)) %>%
  arrange(variant, sex, age, year) %>%
  mutate(last_year = lag(rate)) %>%
  mutate(change = (rate - last_year)/last_year)%>%
  mutate(change = ifelse(year == min(year),0,change)) %>%
  select(-rate, -last_year)



#write output
dir.create("input_data/mortality", recursive = TRUE, showWarnings = FALSE)
saveRDS(mort_trend, "input_data/mortality/npp_mortality_trend.rds" )

rm(list=ls())
