#Data paths
census_ward_ce_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/base_institutional_popn.rds"
census_ward_pop_path <- "Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/base_population.rds"
ward_estimates_path <- "input_data/small_area_model/ward_population_estimates_2011_2017.rds"
ons_comm_est_path <- "input_data/household_model/ons_communal_establishment_population.rds"


#The population at mid-year is different than at census day
#The cnesus total population is compared to the ward estimates at mid-year
#Scaling factors are derived to convert the census population
#These are applied to the census ce population to bring it up to mid-year
census_population <- readRDS(census_ward_pop_path) %>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male")) %>%
  rename(census_pop = total_population)

ward_estaimates <- readRDS(ward_estimates_path) %>%
  filter(year == 2011) %>%
  filter(gss_code_borough != "E09000001")

ward_scaling_1 <- ward_estaimates %>%
  left_join(census_population, by=c("gss_code_borough","gss_code_ward","sex","age")) %>%
  mutate(factor = ifelse(census_pop == 0, 0, popn / census_pop)) %>%
  select(-census_pop, -popn)

base_ce_estimates <- readRDS(census_ward_ce_path)%>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male")) %>%
  left_join(ward_scaling_1, by=c("gss_code_borough","gss_code_ward","sex","age")) %>%
  mutate(base_ce = factor*institutional_population) %>%
  select(-factor, -institutional_population)
  

#The ward-level ce pop estimates are scaled to agree with the
#borough-level data from ONS for all years
ons_borough_ce <- readRDS(ons_comm_est_path) %>%
  filter(year %in% 2011:2017 & stringr::str_detect(gss_code, "E09")) %>%
  filter(gss_code != "E09000001") %>%
  rename(gss_code_borough = gss_code)

ward_age_groups <- base_ce_estimates %>%
  mutate(age_group = cut(age, breaks=c(0, 15, seq(19, 89, 5), Inf),
                         labels = unique(ons_borough_ce$age_group),
                         include.lowest = T),
         age_group = as.character(age_group)) %>%
  select(-year)

ward_comm_est_popn <- ward_age_groups %>%
  group_by(gss_code_borough, sex, age_group) %>%
  summarise(base_ce = sum(base_ce)) %>%
  left_join(ons_borough_ce, by = c("gss_code_borough", "sex", "age_group")) %>%
  mutate(factor = ifelse(ce_pop == 0, 0, ce_pop/base_ce)) %>%
  select(-ce_pop, -base_ce) %>%
  left_join(ward_age_groups, by=c("gss_code_borough","sex","age_group")) %>%
  mutate(ce_pop = factor*base_ce) %>%
  select(gss_code_borough, gss_code_ward, year, sex, age, ce_pop)


assertthat::assert_that(sum(ward_comm_est_popn$ce_pop)==sum(ons_borough_ce$ce_pop))

#--------------

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(ward_comm_est_popn, "input_data/small_area_model/ward_communal_establishment_population.rds")
