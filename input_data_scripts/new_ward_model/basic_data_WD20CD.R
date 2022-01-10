library(dplyr)
library(data.table)
library(popmodules)

data_dir <- "input_data/new_ward_model/"
dir.create(data_dir, showWarnings = FALSE)

#location of the regrosser project
#data outputs are also on the DPA box
regrosser_dir <- "C:/Projects_c/regrosser/data/"

#-------------------------------------------------------------------------------

# data contains non-London wards from the regrosser process
# these present as NA in the WD20CD column
# filter these out

ward_pop <- readRDS(paste0(regrosser_dir, "annual_estimates/ward_population_estimates.rds")) %>% 
  filter(!is.na(WD20CD)) %>% 
  rename(popn = value,
         gss_code_ward = WD20CD) %>% 
  mutate(geography = "WD20CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, popn)

ward_births <- readRDS(paste0(regrosser_dir, "created/births_ward.rds")) %>% 
  filter(!is.na(WD20CD)) %>% 
  rename(gss_code_ward = WD20CD) %>% 
  mutate(births = value,
         geography = "WD20CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, births)

ward_deaths <- readRDS(paste0(regrosser_dir, "created/deaths_ward_sya.rds")) %>% 
  filter(!is.na(WD20CD)) %>% 
  rename(deaths = value,
         gss_code_ward = WD20CD) %>% 
  mutate(geography = "WD20CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, deaths)

#-------------------------------------------------------------------------------

flows <- list.files(paste0(regrosser_dir, "outputs/ward"), full.names = TRUE)
gross_flows <- list()

for(i in 1:33){
  gross_flows[[i]] <- readRDS(flows[i]) %>% 
    mutate(geography = "WD20CD") %>%
    rename(gss_code_ward = WD20CD) %>% 
    select(geography, year, gss_code, gss_code_ward, sex, age, inflow, outflow)
}

gross_flows <- rbindlist(gross_flows)
ward_inflow <- gross_flows %>% select(-outflow) %>% data.frame()
ward_outflow <- gross_flows %>% select(-inflow) %>% data.frame()
rm(flows, gross_flows, i)

#-------------------------------------------------------------------------------
lsoa_to_ward <- readRDS("input_data/new_ward_model/lookups/lsoa_to_WD20_lookup.rds")

dwellinngs_to_hh <- fread(paste0(data_dir,"raw/dwelling_to_hh_LSOA.csv")) %>%
  data.frame() %>% 
  left_join(lsoa_to_ward, by="gss_code_lsoa") %>% 
  group_by(gss_code_ward) %>% 
  summarise(d2hh_ratio = sum(households)/sum(dwellings),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(gss_code_ward %in% ward_pop$gss_code_ward)

#-------------------------------------------------------------------------------

#Communal Establishments

comm_est_lsoa <- rbind(
  fread('input_data/new_ward_model/raw/communal_est_males_lsoa.csv', header = TRUE),
  fread('input_data/new_ward_model/raw/communal_est_females_lsoa.csv', header = TRUE)) %>% 
  tidyr::pivot_longer(values_to = "ce_popn", names_to = "age_group", cols = starts_with("Age")) %>% 
  mutate(age_min = as.numeric(substr(age_group, 5,6))) %>% 
  mutate(age_max = ifelse(age_min < 10, substr(age_group, 9,10),
                          substr(age_group, 11,12))) %>% 
  mutate(age_max = ifelse(age_min == 15, 15,
                          ifelse(age_min == 85, 90,
                                 as.numeric(age_max)))) %>% 
  data.frame()

communal_est_ward <- comm_est_lsoa  %>% 
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

comm_est_ward_sya <- rbindlist(comm_est_ward_sya) %>% 
  select(gss_code_ward, sex, age, ce_popn) %>% 
  data.frame()

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

saveRDS(ward_births, paste0(data_dir, "backseries/ward_births_WD20CD.rds"))
saveRDS(ward_deaths, paste0(data_dir, "backseries/ward_deaths_WD20CD.rds"))
saveRDS(ward_pop, paste0(data_dir, "backseries/ward_population_WD20CD.rds"))
saveRDS(ward_inflow, paste0(data_dir, "backseries/ward_inflow_WD20CD.rds"))
saveRDS(ward_outflow, paste0(data_dir, "backseries/ward_outflow_WD20CD.rds"))
saveRDS(dwellinngs_to_hh, paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD20CD.rds"))
saveRDS(comm_est_ward_sya, paste0(data_dir, "processed/communal_establishment_popn_WD20CD.rds"))

rm(list=ls())
