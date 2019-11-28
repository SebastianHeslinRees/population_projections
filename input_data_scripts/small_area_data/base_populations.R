
#Data paths

census_dwellings_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/census_dwellings.rds"
ward_estimates_path <- "input_data/small_area_model/ward_population_estimates_2011_2017.rds"
ward_communal_est_path <- "input_data/small_area_model/ward_communal_establishment_population.rds"


#Adults per dwelling
base_household_adults <- readRDS(ward_estimates_path) %>%
  filter(gss_code_borough != "E09000001") %>%
  left_join(readRDS(ward_communal_est_path), by = c("year", "gss_code_borough", "gss_code_ward", "sex", "age")) %>%
  mutate(household_popn = popn - ce_pop) %>%
  filter(age >= 18) %>%
  group_by(gss_code_ward, year) %>%
  summarise(adults = sum(household_popn)) %>%
  as.data.frame()

adults_per_dwelling <- readRDS(census_dwellings_path)
#Births
birth_data <- data.table::fread(birth_data_path) %>%
  tidyr::pivot_longer(cols = 5:30, names_to = "year", values_to = "births") %>%
  mutate(year = substr(year,1,4),
         year = as.numeric(year),
         year = year+1) %>%
  select(gss_code_borough = LAD11CD, gss_code_ward = WD11CD, year, births) %>%
  filter(gss_code_borough != "E09000001")

#Deaths
death_data <- data.table::fread(death_data_path)



#Fertility rates

#Mortality_rates

#Save
dir.create("input_data/small_area_model", showWarnings = F)
aveRDS(adults_per_dwelling, "input_data/small_area_model/ward_adults_per_dwelling.rds")
saveRDS(birth_data, "input_data/small_area_model/ward_births_1992_2017.rds")





