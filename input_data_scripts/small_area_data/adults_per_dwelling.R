#NB: Must run population, communal establishment, LDD scripts first

#Data paths
ward_estimates_path <- "input_data/small_area_model/ward_population_estimates_2010_2017.rds"
ward_communal_est_path <- "input_data/small_area_model/ward_communal_establishment_population.rds"
ward_ldd_path <- "input_data/housing_led_model/ldd_backseries_dwellings_ward.rds"

#Load previously processed LDD, ward pop and communal est pop data
#LDD data has been converted to total dwelling stock by ward by year
ldd_units <- readRDS(ward_ldd_path) %>%
  filter(year %in% 2012:2017)
ward_popn <- readRDS(ward_estimates_path) %>%
  filter(year %in% 2012:2017)
ward_ce <- readRDS(ward_communal_est_path)

#Adults per dwelling
base_household_adults <- ward_popn %>%
 # filter(gss_code != "E09000001") %>%
  left_join(ward_ce, by = c("gss_code_ward", "sex", "age")) %>%
  mutate(household_popn = popn - ce_popn) %>%
  filter(age >= 18) %>%
  group_by(gss_code_ward, year) %>%
  summarise(adults = sum(household_popn)) %>%
  as.data.frame()

adults_per_dwelling <- left_join(base_household_adults, ldd_units, by = c("gss_code_ward","year")) %>%
  mutate(adults_per_dwelling = adults / units)

#Some data checks - delete when happy
# test <- adults_per_dwelling %>%
#   group_by(gss_code_ward) %>%
#   mutate(change = adults_per_dwelling - lag(adults_per_dwelling))
# summary(test$change)
# 
# test_2 <- filter(test, sqrt(change^2) > 0.1) %>% arrange(gss_code_ward, year) %>% View()

if(length(unique(adults_per_dwelling$gss_code_ward))!=625){message("Warning: Wrong number of wards")}

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(adults_per_dwelling, "input_data/small_area_model/ward_adults_per_dwelling.rds")






