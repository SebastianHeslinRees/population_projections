library(dplyr)
library(popmodules)

#Data paths
census_msoa_ce_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/DC1104EW_London_MSOA11.rds"
msoa_estimates_path <- "input_data/small_area_model/msoa_population_estimates.rds"

#The census total population is compared to the msoa estimates at mid-year
#Scaling factors are derived to convert the census population
#These are applied to the census ce population to bring it up to mid-year
census_msoa_ce <- readRDS(census_msoa_ce_path) %>%
  filter(C_RESIDENCE_TYPE == 2,
         C_SEX != 0,
         C_AGE != 0) %>%
  group_by(gss_code_msoa = GEOGRAPHY_CODE, sex = C_SEX_NAME, age_group = C_AGE_NAME) %>%
  summarise(ce_popn = sum(OBS_VALUE)) %>%
  as.data.frame() %>%
  mutate(sex = case_when(sex == "Males" ~ "male",
                         sex == "Females" ~ "female")) %>%
  mutate(min_age = substr(age_group, 5, 6),
         f = nchar(age_group),
         max_age = ifelse(min_age == "85", 90, substr(age_group, f-1, f)),
         min_age = as.numeric(min_age),
         max_age = as.numeric(max_age)) %>%
  select(-f)

msoa_estimates <- readRDS(msoa_estimates_path) %>% filter(year == 2011)

lower_bounds <- unique(census_msoa_ce$min_age)
upper_bounds <- unique(census_msoa_ce$max_age)
age_mapping <- data.frame(age = 0:90) %>%
  mutate(min_age = sapply(age, function(x) max(lower_bounds[lower_bounds <= x])),
         max_age = sapply(age, function(x) min(upper_bounds[upper_bounds >= x])),
         age_group = paste("Age", min_age, "to", max_age),
         age_group = case_when(age_group == "Age 15 to 15" ~ "Age 15",
                               age_group == "Age 85 to 90" ~ "Age 85 and over",
                               TRUE ~ age_group)) %>%
  select(age, age_group)

msoa_estimates <- left_join(msoa_estimates, age_mapping, by="age")

msoa_ce_by_sya <- constrain_component(msoa_estimates, census_msoa_ce,
                               col_aggregation = c("gss_code_msoa","sex","age_group"),
                               col_popn = "popn",
                               col_constraint = "ce_popn") %>%
  select(gss_code_msoa, sex, age, ce_popn = popn) %>%
  as.data.frame()

# sum(filter(msoa_ce_by_sya, gss_code_msoa == "E05000026")$ce_popn)
# sum(filter(census_msoa_ce, gss_code_msoa == "E05000026")$ce_popn)

if(length(unique(msoa_ce_by_sya$gss_code_msoa))!=983){message("Warning: Wrong number of msoas")}
#--------------

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(msoa_ce_by_sya, "input_data/small_area_model/msoa_communal_establishment_population.rds")
