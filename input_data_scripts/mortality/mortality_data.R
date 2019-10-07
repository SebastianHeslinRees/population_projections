#NPP Mortality Trend data

max_year <- 2050
npp_data_location <- "Q:/Teams/D&PA/Data/population_projections/ons_npp/2016-based NPP/model_inputs"

#function to read and wrangle raw data
mortality_trend <- function(file, var, max_year){
  mort <- fread(file) %>%
    gather(year, rate, 3:102) %>%
    mutate(sex = ifelse(Sex == 1, "male", "female")) %>%
    mutate(age = ifelse(Age == "Birth", -1, Age),
           age = as.numeric(age))%>%
    select(-Sex, -Age) %>%
    mutate(year = as.numeric(substr(year, 8,11))) %>%
    filter(year <= max_year) %>%
    select(sex, age, year, rate)
  
  back_to_2001 <- list()
  for(y in 2001:2016){
    back_to_2001[[y]] <- filter(mort, year == 2017) %>%
      mutate(year = y)
  }
  
  mort <- rbind(rbindlist(back_to_2001), mort) %>%
    mutate(variant = var)
  
  return(mort)
  
}

#read in data
x <- getwd()
setwd(npp_data_location)
principal <- mortality_trend("Principal Mortality Assumptions.csv", "2016_principal", max_year)
high <- mortality_trend("High Mortality Assumptions.csv", "2016_high", max_year)
low <- mortality_trend("Low Mortality Assumptions.csv", "2016_low", max_year)

#bind the variants together
mort_trend <- rbind(principal, high, low)%>%
  mutate(age = age + 1) %>%
  filter(age %in% c(0:90)) %>%
  arrange(sex, age, year) %>%
  mutate(last_year = lag(rate)) %>%
  mutate(change = (rate - last_year)/last_year)%>%
  mutate(change = ifelse(year == min(year),0,change)) %>%
  select(-rate, -last_year)

#write output
setwd(x)
saveRDS(mort_trend, "input_data/mortality/npp_mortality_trend.rds" )

rm(high, low, principal, mort_trend, max_year, npp_data_location, x, mortality_trend)
