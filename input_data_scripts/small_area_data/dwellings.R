
#Data paths
census_dwellings_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/census_dwellings.rds"
ward_estimates_path <- "input_data/small_area_model/ward_population_estimates_2011_2017.rds"
ward_communal_est_path <- "input_data/small_area_model/ward_communal_establishment_population.rds"
 

#TODO TEMP
#ward_ldd_path <-
load("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/housing data/2016 SHLAA.Rdata")
ldd_units <- filter(ward_dev, year %in% 2012:2017) %>%
  rename(additional_units = new_homes)
rm(borough_dev, msoa_dev, ward_dev)

#Adults per dwelling
base_household_adults <- readRDS(ward_estimates_path) %>%
  filter(gss_code_borough != "E09000001") %>%
  left_join(readRDS(ward_communal_est_path), by = c("year", "gss_code_borough", "gss_code_ward", "sex", "age")) %>%
  mutate(household_popn = popn - ce_pop) %>%
  filter(age >= 18) %>%
  group_by(gss_code_ward, year) %>%
  summarise(adults = sum(household_popn)) %>%
  as.data.frame()

census_dwellings <- readRDS(census_dwellings_path)

# ldd_units <- readRDS(ward_ldd_path) %>%
#   filter(year %in% 2012:2017) %>%
#   select(gss_code_ward, year, additional_units)

dwellings <- left_join(census_dwellings, ldd_units, by=c("gss_code_borough","gss_code_ward")) %>%
  mutate(total_dwellings = dwellings + additional_units) %>%
  select(-dwellings, -additional_units) %>%
  select(gss_code_borough, gss_code_ward, year, total_dwellings)%>%
  rbind(census_dwellings %>%
          mutate(year = 2011) %>%
          rename(total_dwellings = dwellings) %>%
          select(gss_code_borough, gss_code_ward, year, total_dwellings))


adults_per_dwelling <- left_join(base_household_adults, dwellings, by = c("gss_code_ward","year")) %>%
  mutate(adults_per_dwelling = adults / total_dwellings)

test <- adults_per_dwelling %>%
  group_by(gss_code_borough, gss_code_ward) %>%
  mutate(change = adults_per_dwelling - lag(adults_per_dwelling))
summary(test$change)

filter(test, sqrt(change^2) > 0.1) %>% arrange(gss_code_ward, year) %>% View()


#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(adults_per_dwelling, "input_data/small_area_model/ward_adults_per_dwelling.rds")






