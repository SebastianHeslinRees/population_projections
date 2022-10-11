library(dplyr)
library(data.table)
library(popmodules)

message("Ward 2013 backseries")

# Create file structure

input_data_dir <- "input_data/flexible_area_model/"
dir.create(input_data_dir, showWarnings = FALSE)
dir.create(paste0(input_data_dir, "lookups"), showWarnings = FALSE)
dir.create(paste0(input_data_dir, "backseries"), showWarnings = FALSE)
dir.create(paste0(input_data_dir, "processed"), showWarnings = FALSE)
dir.create(paste0(input_data_dir, "development_data"), showWarnings = FALSE)

#-------------------------------------------------------------------------------

# Location of raw data on the Q drive

data_dir_Q_drive <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"

#-------------------------------------------------------------------------------
# Lookups

lsoa_to_ward <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_to_WD13_lookup.rds"))
lsoa_to_lsoa <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))
oa_to_ward <- readRDS(paste0(data_dir_Q_drive, "lookups/oa_to_WD13_lookup.rds"))
ward_name_lookup <- readRDS(paste0(data_dir_Q_drive, "lookups/ward_2013_name_lookup.rds"))

saveRDS(lsoa_to_ward, paste0(input_data_dir, "lookups/lsoa_to_WD13_lookup.rds"))
saveRDS(lsoa_to_lsoa, paste0(input_data_dir, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))
saveRDS(oa_to_ward, paste0(input_data_dir, "lookups/oa_to_WD13_lookup.rds"))
saveRDS(ward_name_lookup, paste0(input_data_dir, "lookups/ward_2013_name_lookup.rds"))

#-------------------------------------------------------------------------------

# data contains non-London wards from the regrosser process
# these present as NA in the WD13CD column
# filter these out

ward_pop <- readRDS(paste0(data_dir_Q_drive, "regrosser/created/ward_population_estimates_WD13.rds")) %>% 
  filter(!is.na(WD13CD)) %>% 
  rename(popn = value,
         gss_code_ward = WD13CD) %>% 
  mutate(geography = "WD13CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, popn)

ward_births <- readRDS(paste0(data_dir_Q_drive, "regrosser/created/births_ward_WD13.rds")) %>% 
  filter(!is.na(WD13CD)) %>% 
  rename(gss_code_ward = WD13CD) %>% 
  mutate(births = value,
         geography = "WD13CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, births)

ward_deaths <- readRDS(paste0(data_dir_Q_drive, "regrosser/created/deaths_ward_WD13_sya.rds")) %>% 
  filter(!is.na(WD13CD)) %>% 
  rename(deaths = value,
         gss_code_ward = WD13CD) %>% 
  mutate(geography = "WD13CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, deaths)

#-------------------------------------------------------------------------------

# Migration flows are saved as 1 file per borough
# Loop through to read in then bind together

flows <- list.files(paste0(data_dir_Q_drive, "regrosser/outputs/ward_2013"), full.names = TRUE)
gross_flows <- list()

for(i in 1:33){
  gross_flows[[i]] <- readRDS(flows[i]) %>% 
    mutate(geography = "WD13CD") %>%
    rename(gss_code_ward = WD13CD) %>% 
    select(geography, year, gss_code, gss_code_ward, sex, age, inflow, outflow)
}

gross_flows <- rbindlist(gross_flows)
ward_inflow <- gross_flows %>% select(-outflow) %>% data.frame()
ward_outflow <- gross_flows %>% select(-inflow) %>% data.frame()
rm(flows, gross_flows, i)

#-------------------------------------------------------------------------------

# Dwelling to household ratio

dwellinngs_to_hh <- fread(paste0(data_dir_Q_drive, "dwelling_to_hh_LSOA.csv")) %>%
  data.frame() %>% 
  left_join(lsoa_to_ward, by="gss_code_lsoa") %>% 
  group_by(gss_code_ward) %>% 
  summarise(d2hh_ratio = sum(households)/sum(dwellings),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(gss_code_ward %in% ward_pop$gss_code_ward)

#-------------------------------------------------------------------------------

# Communal Establishment populations
# Create single year of age from grouped data

communal_est_lsoa <- rbind(
  fread(paste0(data_dir_Q_drive, 'communal_est_males_lsoa.csv'), header = TRUE),
  fread(paste0(data_dir_Q_drive, 'communal_est_females_lsoa.csv'), header = TRUE)) %>% 
  tidyr::pivot_longer(values_to = "ce_popn", names_to = "age_group", cols = starts_with("Age")) %>% 
  mutate(age_min = as.numeric(substr(age_group, 5,6))) %>% 
  mutate(age_max = ifelse(age_min < 10, substr(age_group, 9,10),
                          substr(age_group, 11,12))) %>% 
  mutate(age_max = ifelse(age_min == 15, 15,
                          ifelse(age_min == 85, 90,
                                 as.numeric(age_max)))) %>% 
  data.frame()

communal_est_ward <- communal_est_lsoa  %>% 
  left_join(lsoa_to_ward, by="gss_code_lsoa") %>% 
  group_by(gss_code_ward, sex, age_group, age_min, age_max) %>% 
  summarise(ce_popn = sum(ce_popn), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(gss_code_ward %in% ward_pop$gss_code_ward)

comm_est_ward_sya <- list()

for(group in unique(communal_est_ward$age_group)){
  
  pop_1 <- filter(communal_est_ward, age_group == group)
  pop_2 <- filter(ward_pop, age %in%  unique(pop_1$age_min): unique(pop_1$age_max), year == 2011)
  
  comm_est_ward_sya[[group]] <- distribute_within_age_band(pop_1,pop_2,
                                                           "ce_popn","popn",
                                                           unique(pop_1$age_min),
                                                           unique(pop_1$age_max),
                                                           col_aggregation = c("gss_code_ward","sex"))
}

message("ingore warnings about NAs - its lying")

comm_est_ward_sya <- rbindlist(comm_est_ward_sya) %>% 
  select(gss_code_ward, sex, age, ce_popn) %>% 
  data.frame()

rm(pop_1, pop_2, communal_est_lsoa, communal_est_ward)

#-------------------------------------------------------------------------------

#Basic checks

range(ward_births$year)
range(ward_deaths$year)
range(ward_pop$year)
range(ward_inflow$year)
range(ward_outflow$year)

popmodules::validate_same_geog(ward_pop, ward_births, "gss_code_ward", "gss_code_ward")
popmodules::validate_same_geog(ward_pop, ward_deaths, "gss_code_ward", "gss_code_ward")
popmodules::validate_same_geog(ward_pop, ward_inflow, "gss_code_ward", "gss_code_ward")
popmodules::validate_same_geog(ward_pop, ward_outflow, "gss_code_ward", "gss_code_ward")
popmodules::validate_same_geog(ward_pop, comm_est_ward_sya, "gss_code_ward", "gss_code_ward")
popmodules::validate_same_geog(ward_pop, dwellinngs_to_hh, "gss_code_ward", "gss_code_ward")

#-------------------------------------------------------------------------------

saveRDS(ward_births, paste0(input_data_dir, "backseries/ward_births_WD13CD.rds"))
saveRDS(ward_deaths, paste0(input_data_dir, "backseries/ward_deaths_WD13CD.rds"))
saveRDS(ward_pop, paste0(input_data_dir, "backseries/ward_population_WD13CD.rds"))
saveRDS(ward_inflow, paste0(input_data_dir, "backseries/ward_inflow_WD13CD.rds"))
saveRDS(ward_outflow, paste0(input_data_dir, "backseries/ward_outflow_WD13CD.rds"))
saveRDS(dwellinngs_to_hh, paste0(input_data_dir, "processed/ward_dwelling_2_hh_ratio_WD13CD.rds"))
saveRDS(comm_est_ward_sya, paste0(input_data_dir, "processed/communal_establishment_popn_WD13CD.rds"))

rm(list=ls())
