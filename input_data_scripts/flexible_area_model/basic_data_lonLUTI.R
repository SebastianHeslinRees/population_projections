library(dplyr)
library(data.table)
library(popmodules)

message("LonLUTI zones backseries")

# Create file structure

input_data_dir <- "input_data/flexible_area_model/"

#-------------------------------------------------------------------------------

# Location of raw data on the Q drive

data_dir_Q_drive <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"

#-------------------------------------------------------------------------------
# Lookups

lsoa_to_lonLUTI <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_to_LonLUTI3.rds")) %>% rename(gss_code_lsoa = LSOA11CD)
oa_to_lonLUTI <- readRDS(paste0(data_dir_Q_drive, "lookups/oa_to_LonLUTI3_lookup.rds")) 
lonLUTI_name_lookup <- oa_to_lonLUTI %>%
  left_join(readRDS("input_data/lookup/gss_code_to_name.rds"), by = "gss_code") %>% 
  select(LonLUTI3, gss_code, gss_name) %>%
  mutate(name = LonLUTI3)  %>% 
  data.frame() %>% 
  distinct() %>% 
  select(gss_code, la_name = gss_name, LonLUTI3, name)

saveRDS(lsoa_to_lonLUTI, paste0(input_data_dir, "lookups/lsoa_to_LonLUTI3_lookup.rds"))
saveRDS(oa_to_lonLUTI, paste0(input_data_dir, "lookups/oa_to_LonLUTI3_lookup.rds"))
saveRDS(lonLUTI_name_lookup, paste0(input_data_dir, "lookups/lonLUTI_name_lookup.rds"))

#-------------------------------------------------------------------------------

# data contains non-London lonLUTIs from the regrosser process
# these present as NA in the WD13CD column
# filter these out

lonLUTI_pop <- readRDS(paste0(data_dir_Q_drive, "regrosser/created/population_estimates_lonLUTI.rds")) %>% 
  rename(popn = value) %>% 
  mutate(geography = "LonLUTI3") %>% 
  select(geography, year, gss_code, LonLUTI3, sex, age, popn)

lonLUTI_births <- readRDS(paste0(data_dir_Q_drive, "regrosser/created/births_lonLUTI.rds")) %>% 
  rename(births = value) %>% 
  mutate(geography = "LonLUTI3") %>% 
  select(geography, year, gss_code, LonLUTI3, sex, age, births)

lonLUTI_deaths <- readRDS(paste0(data_dir_Q_drive, "regrosser/created/deaths_lonLUTI_sya.rds")) %>% 
  rename(deaths = value) %>% 
  mutate(geography = "LonLUTI3") %>% 
  select(geography, year, gss_code, LonLUTI3, sex, age, deaths)

#-------------------------------------------------------------------------------

gross_flows <- readRDS(paste0(data_dir_Q_drive, "regrosser/outputs/LonLUTI/gross_flows_lonLUTI.rds"))
lonLUTI_inflow <- gross_flows %>% select(-outflow) %>% data.frame()
lonLUTI_outflow <- gross_flows %>% select(-inflow) %>% data.frame()

#-------------------------------------------------------------------------------

# Dwelling to household ratio

dwellinngs_to_hh <- fread(paste0(data_dir_Q_drive, "dwelling_to_hh_LSOA.csv")) %>%
  data.frame() %>%
  left_join(lsoa_to_lonLUTI, by="gss_code_lsoa") %>%
  group_by(LonLUTI3) %>%
  summarise(d2hh_ratio = sum(households)/sum(dwellings),
            .groups = 'drop_last') %>%
  data.frame() %>%
  filter(LonLUTI3 %in% lonLUTI_pop$LonLUTI3)

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

lsoa_lookup_2020 <- filter(lsoa_to_lonLUTI, year == 2020)

communal_est_lonLUTI <- communal_est_lsoa  %>%
  left_join(lsoa_lookup_2020, by="gss_code_lsoa") %>%
  group_by(LonLUTI3, sex, age_group, age_min, age_max) %>%
  summarise(ce_popn = sum(ce_popn*lsoa_share), .groups = 'drop_last') %>%
  data.frame() %>%
  filter(LonLUTI3 %in% lonLUTI_pop$LonLUTI3)

comm_est_lonLUTI_sya <- list()

for(group in unique(communal_est_lonLUTI$age_group)){

  pop_1 <- filter(communal_est_lonLUTI, age_group == group)
  pop_2 <- filter(lonLUTI_pop, age %in%  unique(pop_1$age_min): unique(pop_1$age_max), year == 2011)

  comm_est_lonLUTI_sya[[group]] <- distribute_within_age_band(pop_1,pop_2,
                                                           "ce_popn","popn",
                                                           unique(pop_1$age_min),
                                                           unique(pop_1$age_max),
                                                           col_aggregation = c("LonLUTI3","sex"))
}

message(paste("There are", sum(is.na(communal_est_lsoa)), "NAs"))

comm_est_lonLUTI_sya <- rbindlist(comm_est_lonLUTI_sya) %>%
  select(LonLUTI3, sex, age, ce_popn) %>%
  data.frame()

rm(pop_1, pop_2, communal_est_lsoa, communal_est_lonLUTI)

#-------------------------------------------------------------------------------

#Basic checks

range(lonLUTI_births$year)
range(lonLUTI_deaths$year)
range(lonLUTI_pop$year)
range(lonLUTI_inflow$year)
range(lonLUTI_outflow$year)

popmodules::validate_same_geog(lonLUTI_pop, lonLUTI_births, "LonLUTI3", "LonLUTI3")
popmodules::validate_same_geog(lonLUTI_pop, lonLUTI_deaths, "LonLUTI3", "LonLUTI3")
popmodules::validate_same_geog(lonLUTI_pop, lonLUTI_inflow, "LonLUTI3", "LonLUTI3")
popmodules::validate_same_geog(lonLUTI_pop, lonLUTI_outflow, "LonLUTI3", "LonLUTI3")
popmodules::validate_same_geog(lonLUTI_pop, comm_est_lonLUTI_sya, "LonLUTI3", "LonLUTI3")
popmodules::validate_same_geog(lonLUTI_pop, dwellinngs_to_hh, "LonLUTI3", "LonLUTI3")

sum(is.na(lonLUTI_pop))
sum(is.na(lonLUTI_births))
sum(is.na(lonLUTI_deaths))
sum(is.na(lonLUTI_inflow))
sum(is.na(lonLUTI_outflow))
sum(is.na(comm_est_lonLUTI_sya))
sum(is.na(dwellinngs_to_hh))
#-------------------------------------------------------------------------------

saveRDS(lonLUTI_births, paste0(input_data_dir, "backseries/lonLUTI_births.rds"))
saveRDS(lonLUTI_deaths, paste0(input_data_dir, "backseries/lonLUTI_deaths.rds"))
saveRDS(lonLUTI_pop, paste0(input_data_dir, "backseries/lonLUTI_population.rds"))
saveRDS(lonLUTI_inflow, paste0(input_data_dir, "backseries/lonLUTI_inflow.rds"))
saveRDS(lonLUTI_outflow, paste0(input_data_dir, "backseries/lonLUTI_outflow.rds"))
saveRDS(dwellinngs_to_hh, paste0(input_data_dir, "processed/dwelling_2_hh_ratio_lonLUTI.rds"))
saveRDS(comm_est_lonLUTI_sya, paste0(input_data_dir, "processed/communal_establishment_popn_lonLUTI.rds"))

rm(list=ls())
