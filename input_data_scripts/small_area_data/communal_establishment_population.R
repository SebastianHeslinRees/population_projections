library(dplyr)
devtools::load_all("model_code/popmodules")

#Data paths
census_ward_ce_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/DC1104EW_London_CMWD11.rds"
ward_estimates_path <- "input_data/small_area_model/ward_population_estimates_2010_2018.rds"

#lookup
merged_to_electoral_ward <- readRDS("input_data/lookup/2011_merged_ward_to_electoral_ward.rds")#The population at mid-year is different than at census day
city_merged_wards <- unique(filter(merged_to_electoral_ward, LA == "E09000001")$`MERGED WARD`)

#The cnesus total population is compared to the ward estimates at mid-year
#Scaling factors are derived to convert the census population
#These are applied to the census ce population to bring it up to mid-year
census_ward_ce <- readRDS(census_ward_ce_path) %>%
  filter(C_RESIDENCE_TYPE == 2,
         C_SEX != 0,
         C_AGE != 0)

city_ce <- filter(census_ward_ce, GEOGRAPHY_CODE %in% city_merged_wards) %>%
  mutate(gss_code_ward = "E09000001") %>%
  group_by(gss_code_ward, sex = C_SEX_NAME, age_group = C_AGE_NAME) %>%
  summarise(ce_popn = sum(OBS_VALUE)) %>%
  as.data.frame()

census_ward_ce <- census_ward_ce %>%
  left_join(merged_to_electoral_ward, by=c("GEOGRAPHY_CODE"="MERGED WARD")) %>%
  filter(substr(LA,1,3)=="E09") %>%
  filter(LA != "E09000001") %>%
  group_by(gss_code_ward = WARD, sex = C_SEX_NAME, age_group = C_AGE_NAME) %>%
  summarise(ce_popn = sum(OBS_VALUE)) %>%
  as.data.frame() %>%
  rbind(city_ce) %>%
  mutate(sex = case_when(sex == "Males" ~ "male",
                         sex == "Females" ~ "female")) %>%
  mutate(min_age = substr(age_group, 5, 6),
         f = nchar(age_group),
         max_age = ifelse(min_age == "85", 90, substr(age_group, f-1, f)),
         min_age = as.numeric(min_age),
         max_age = as.numeric(max_age)) %>%
  select(-f)

ward_estimates <- readRDS(ward_estimates_path) %>% filter(year == 2011)

lower_bounds <- unique(census_ward_ce$min_age)
upper_bounds <- unique(census_ward_ce$max_age)
age_mapping <- data.frame(age = 0:90) %>%
  mutate(min_age = sapply(age, function(x) max(lower_bounds[lower_bounds <= x])),
         max_age = sapply(age, function(x) min(upper_bounds[upper_bounds >= x])),
         age_group = paste("Age", min_age, "to", max_age),
         age_group = case_when(age_group == "Age 15 to 15" ~ "Age 15",
                               age_group == "Age 85 to 90" ~ "Age 85 and over",
                               TRUE ~ age_group)) %>%
  select(age, age_group)

ward_estimates <- left_join(ward_estimates, age_mapping, by="age")

ward_ce_by_sya <- constrain_component(ward_estimates, census_ward_ce,
                               col_aggregation = c("gss_code_ward","sex","age_group"),
                               col_popn = "popn",
                               col_constraint = "ce_popn") %>%
  select(gss_code_ward, sex, age, ce_popn = popn) %>%
  as.data.frame()

# sum(filter(ward_ce_by_sya, gss_code_ward == "E05000026")$ce_popn)
# sum(filter(census_ward_ce, gss_code_ward == "E05000026")$ce_popn)

if(length(unique(ward_ce_by_sya$gss_code_ward))!=625){message("Warning: Wrong number of wards")}
#--------------

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(ward_ce_by_sya, "input_data/small_area_model/ward_communal_establishment_population.rds")
