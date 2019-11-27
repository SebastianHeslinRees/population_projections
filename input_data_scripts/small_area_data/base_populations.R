#Data paths
mye_pop_path <- "input_data/mye/2018/population_gla_2019-11-13.rds"
ons_comm_est_path <- "input_data/household_model/ons_communal_establishment_population.rds"
census_ward_ce_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/base_institutional_popn.rds"
census_ward_pop_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/base_population.rds"
census_dwellings_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/census_dwellings.rds"
birth_data_path <- "Q:/Teams/D&PA/Data/births_and_deaths/mid_year_births/births_london_2011wards_1991_2017.csv"
death_data_path <- "Q:/Teams/D&PA/Data/births_and_deaths/mid_year_births/deaths_london_2011wards_1991_2017.csv"

####2011 base population####
census_population <- readRDS(census_ward_pop_path) %>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male"))
  
mye_2011 <- readRDS(mye_pop_path) %>%
  filter(year == 2011 & stringr::str_detect(gss_code, "E09")) %>%
  rename(gss_code_borough = gss_code)

scaling_2011 <- census_population %>%
  group_by(gss_code_borough, sex, age) %>%
  summarise(unscaled = sum(total_population)) %>%
  as.data.frame() %>%
  left_join(mye_2011, by=c("gss_code_borough","sex","age")) %>%
  mutate(scaling = ifelse(unscaled == 0, 0, popn / unscaled)) %>%
  select(gss_code_borough, sex, age, scaling)

base_population <- left_join(census_population, scaling_2011, by=c("gss_code_borough","sex","age")) %>%
  mutate(popn = total_population * scaling,
         year = 2011) %>%
  select(year, gss_code_borough, gss_code_ward, sex, age, popn)

####Communal Establishment Population####
#Scale ce using same scaling factors
census_comm_est_popn <- readRDS(census_ward_ce_path)%>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male"))

base_comm_est_popn <- left_join(census_comm_est_popn, scaling_2011, by=c("gss_code_borough","sex","age")) %>%
  mutate(communal_est_popn = institutional_population * scaling) %>%
  select(gss_code_borough, gss_code_ward, sex, age, communal_est_popn)

#Then scale to ONS ce population
ons_comm_est_popn <- readRDS(ons_comm_est_path) %>%
  filter(year == 2011 & stringr::str_detect(gss_code, "E09")) %>%
  group_by(gss_code_borough = gss_code, year) %>%
  summarise(ce_pop = sum(ce_pop)) %>%
  as.data.frame()

scaling_ce <- base_comm_est_popn %>%
  group_by(gss_code_borough) %>%
  summarise(unscaled = sum(communal_est_popn)) %>%
  as.data.frame() %>%
  left_join(ons_comm_est_popn, by= "gss_code_borough") %>%
  mutate(scaling = ifelse(unscaled == 0, 0, ce_pop / unscaled)) %>%
  select(gss_code_borough, scaling)

ward_comm_est_popn <- left_join(base_comm_est_popn, scaling_ce, by="gss_code_borough") %>%
  mutate(communal_est_popn = communal_est_popn * scaling,
         year = 2011) %>%
  select(year, gss_code_borough, gss_code_ward, sex, age, communal_est_popn) %>%
  popmodules::project_forward_flat(2050)


####Housheold population####]
base_household_popn <- left_join(base_population, filter(ward_comm_est_popn, year == 2011),
                                 by=c("year","gss_code_borough","gss_code_ward","sex","age")) %>%
  mutate(household_popn = popn - communal_est_popn) %>%
  select(year, gss_code_borough, gss_code_ward, sex, age, household_popn)


#Adults per dwelling
base_household_adults <- base_household_popn %>%
  filter(age >= 18) %>%
  group_by(gss_code_ward) %>%
  summarise(adults = sum(household_popn)) %>%
  as.data.frame()

adults_per_dwelling <- readRDS(census_dwellings_path) %>%
  left_join(base_household_adults, by="gss_code_ward") %>%
  mutate(ratio = adults/dwellings) %>%
  select(gss_code_borough, gss_code_ward, ratio)

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
saveRDS(ward_comm_est_popn, "input_data/small_area_model/ward_communal_establishment_population.rds")
saveRDS(base_population, "input_data/small_area_model/ward_2011_base_population.rds")
saveRDS(base_household_popn, "input_data/small_area_model/ward_2011_base_household_population.rds")
saveRDS(adults_per_dwelling, "input_data/small_area_model/ward_adults_per_dwelling.rds")
saveRDS(birth_data, "input_data/small_area_model/ward_births_1992_2017.rds")





