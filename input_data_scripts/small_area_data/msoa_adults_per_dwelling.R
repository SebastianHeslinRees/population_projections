#NB: Must run population, communal establishment, LDD scripts first

library(dplyr)

most_recent_data_year <- 2018

#Data paths
msoa_estimates_path <- "input_data/small_area_model/msoa_population_estimates.rds"
msoa_communal_est_path <- "input_data/small_area_model/msoa_communal_establishment_population.rds"
msoa_ldd_path <- "input_data/small_area_model/ldd_backseries_dwellings_msoa.rds"

#Load previously processed LDD, msoa pop and communal est pop data
#LDD data must be converted to total dwelling stock by msoa by year
ldd_units <- readRDS(msoa_ldd_path) %>%
  group_by(gss_code_msoa) %>%
  mutate(units = cumsum(units)) %>%
  as.data.frame() %>%
  filter(year %in% 2012:most_recent_data_year)

msoa_popn <- readRDS(msoa_estimates_path) %>%
  filter(year %in% 2012:most_recent_data_year)

msoa_ce <- readRDS(msoa_communal_est_path)

#Adults per dwelling
base_household_adults <- msoa_popn %>%
  left_join(msoa_ce, by = c("gss_code_msoa", "sex", "age")) %>%
  mutate(household_popn = popn - ce_popn) %>%
  filter(age >= 18) %>%
  group_by(gss_code_msoa, year) %>%
  summarise(adults = sum(household_popn)) %>%
  as.data.frame()

adults_per_dwelling <- left_join(base_household_adults, ldd_units, by = c("gss_code_msoa","year")) %>%
  mutate(adults_per_dwelling = adults / units) %>%
  select(year, gss_code_msoa, adults_per_dwelling)

#Some data checks - delete when happy
# test <- adults_per_dwelling %>%
#   group_by(gss_code_msoa) %>%
#   mutate(change = adults_per_dwelling - lag(adults_per_dwelling))
# summary(test$change)
# 
# test_2 <- filter(test, sqrt(change^2) > 0.1) %>% arrange(gss_code_msoa, year) %>% View()

if(length(unique(adults_per_dwelling$gss_code_msoa))!=983){message("Warning: Wrong number of msoas")}

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(adults_per_dwelling, "input_data/small_area_model/msoa_adults_per_dwelling.rds")

