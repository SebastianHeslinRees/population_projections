#NPP Mortality Trend data
library(dplyr)
library(tidyr)
library(data.table)
max_year <- 2050
npp_data_location <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs"

#function to read and wrangle raw data
fertility_trend <- function(file, var, max_year, npp_data_location){
  fert <- fread(paste0(npp_data_location, file)) %>%
    gather(year, rate, 3:102) %>%
    mutate(sex = ifelse(Sex == 1, "male", "female")) %>%
    mutate(age = as.numeric(Age))%>%
    select(-Sex, -Age) %>%
    mutate(year = as.numeric(substr(year, 8,11))) %>%
    filter(year <= max_year) %>%
    select(sex, age, year, rate)
  
  back_to_2001 <- list()
  for(y in 2001:2016){
    back_to_2001[[y]] <- filter(fert, year == 2017) %>%
      mutate(year = y)
  }
  
  fert <- rbind(data.table::rbindlist(back_to_2001), fert) %>%
    mutate(variant = var)
  
  return(fert)
  
}

#read in data
principal <- fertility_trend("/Principal Fertility Assumptions.csv", "2016_principal", max_year, npp_data_location)
high <- fertility_trend("/High Fertility Assumptions.csv", "2016_high", max_year, npp_data_location)
low <- fertility_trend("/Low Fertility Assumptions.csv", "2016_low", max_year, npp_data_location)

#bind the variants together
fert_trend <- rbind(principal, high, low) %>%
  arrange(variant, sex, age, year) %>%
  mutate(last_year = lag(rate)) %>%
  mutate(change = (rate - last_year)/last_year)%>%
  mutate(change = ifelse(year == min(year),0,change)) %>%
  select(-rate, -last_year)

popmodules::validate_population(fert_trend, 
                                col_aggregation = c("sex", "age", "year", "variant"), 
                                col_data = "change", 
                                check_negative_values = FALSE)

#write output
dir.create("input_data/fertility", recursive = TRUE, showWarnings = FALSE)
saveRDS(fert_trend, "input_data/fertility/npp_fertility_trend.rds" )

rm(high, low, principal, fert_trend, max_year, npp_data_location, fertility_trend)
