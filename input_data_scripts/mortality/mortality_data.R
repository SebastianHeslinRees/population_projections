#NPP Mortality Trend data
mort_trend <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Inputs/2017 base/constant inputs/npp_mortality_trend.rds")

mort_trend <- setnames(mort_trend, c("sex","age","year","2018_principal","2018_low","2018_high",
                   "2016_principal","2016_low","2016_high"))

mort_trend <- gather(mort_trend, variant, national_rate, 4:9) %>%
  mutate(sex = ifelse(sex=="F", "female", ifelse(sex=="M","Male",NA))) %>%
  mutate(age = age + 1) %>%
  filter(age %in% c(0:90)) %>%
  arrange(sex, age, year) %>%
  mutate(last_year = lag(national_rate)) %>%
  #filter(year <= 2050) %>%
  mutate(change = (national_rate - last_year)/last_year)%>%
  mutate(change = ifelse(year == min(year),0,change)) %>%
  select(-national_rate, -last_year)

saveRDS(mort_trend, "input_data/mortality/npp_mortality_trend.rds" )

#SNPP ASMR curves
mortality_curves <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Inputs/2017 base/constant inputs/ons_asmr_curves.rds") %>%
  mutate(sex = ifelse(sex == "M", "male", ifelse(sex == "F", "female", NA))) %>%
  select(-year)

saveRDS(mortality_curves, "input_data/mortality/ons_asmr_curves.rds" )
