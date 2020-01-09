#Data paths
census_ward_ce_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/DC1104EW_London_CMWD11.rds"
ward_estimates_path <- "input_data/small_area_model/ward_population_estimates_2010_2017.rds"

#lookup
merged_to_electoral_ward <- readRDS("input_data/lookup/2011_merged_ward_to_electoral_ward.rds")#The population at mid-year is different than at census day

#The cnesus total population is compared to the ward estimates at mid-year
#Scaling factors are derived to convert the census population
#These are applied to the census ce population to bring it up to mid-year
census_ward_ce <- readRDS(census_ward_ce_path) %>%
  filter(C_RESIDENCE_TYPE == 2,
         C_SEX != 0,
         C_AGE != 0) %>%
  left_join(merged_to_electoral_ward, by=c("GEOGRAPHY_CODE"="MERGED WARD")) %>%
  filter(substr(LA,1,3)=="E09") %>%
  mutate(gss_code_ward = ifelse(LA == "E09000001", "E09000001", WARD)) %>%
  group_by(gss_code_ward, sex = C_SEX_NAME, age_group = C_AGE_NAME) %>%
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

groups <- unique(census_ward_ce$age_group)

ward_estimates <- readRDS(ward_estimates_path) %>% filter(year == 2011)
distributed <- list()

for(i in 1:length(groups)){
  
  a <- filter(census_ward_ce, age_group == groups[[i]])
  
  min <- unique(a$min_age)
  max <- unique(a$max_age)
  
  distributed[[i]] <- distribute_within_age_band(popn_1=a, popn_2=ward_estimates,
                                                 popn_1_col="ce_popn", popn_2_col="popn",
                                                 min_age=min, max_age=max,
                                                 col_aggregation=c("gss_code_ward","sex"))      
}

ward_ce_by_sya <- data.table::rbindlist(distributed) %>%
  select(gss_code_ward, sex, age, ce_popn)

# sum(filter(ward_ce_by_sya, gss_code_ward == "E05000026")$ce_popn)
# sum(filter(census_ward_ce, gss_code_ward == "E05000026")$ce_popn)

if(length(unique(ward_ce_by_sya$gss_code_ward))!=625){message("Warning: Wrong number of wards")}
#--------------

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(ward_ce_by_sya, "input_data/small_area_model/ward_communal_establishment_population.rds")
