library(dplyr)
library(data.table)
library(popmodules)
library(stringr)

message("Ward 2022 backseries")

# Create file structure

input_data_dir <- "input_data/flexible_area_model/"
regrosser_data <- "E:/project_folders/demography/wil/regrosser/data/"
Q_data <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"

dir.create(input_data_dir, showWarnings = FALSE)
dir.create(paste0(input_data_dir, "backseries"), showWarnings = FALSE)
dir.create(paste0(input_data_dir, "processed"), showWarnings = FALSE)
dir.create(paste0(input_data_dir, "development_data"), showWarnings = FALSE)

ward_LAD <- readRDS(paste0(input_data_dir, "lookups/ward_2022_name_lookup.rds"))

#-------------------------------------------------------------------------------

# data contains non-London wards from the regrosser process
# these present as NA in the WD22CD column
# filter these out

ward_pop <- readRDS(paste0(regrosser_data, "estimates_series/WD22CD/WD22CD_population_London_2021_series.rds")) %>% 
  rename(gss_code_ward = WD22CD) %>% 
  left_join(ward_LAD, by = "gss_code_ward") %>% 
  mutate(geography = "WD22CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, popn)

ward_births <- readRDS(paste0(regrosser_data, "estimates_series/WD22CD/WD22CD_births_London_2021_series.rds")) %>% 
  rename(gss_code_ward = WD22CD) %>%
  left_join(ward_LAD, by = "gss_code_ward") %>% 
  mutate(geography = "WD22CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, births)

ward_deaths <- readRDS(paste0(regrosser_data, "estimates_series/WD22CD/WD22CD_deaths_London_2021_series.rds")) %>% 
  rename(gss_code_ward = WD22CD) %>% 
  left_join(ward_LAD, by = "gss_code_ward") %>% 
  mutate(geography = "WD22CD") %>% 
  select(geography, year, gss_code, gss_code_ward, sex, age, deaths)

#-------------------------------------------------------------------------------

# Gross flows are saved by year
# Loop through to read in then bind together
gross_flows <- list()

for(i in 2011:2021){

  gross_flows[[i]] <- readRDS(paste0(regrosser_data, "gross_migration/WD22CD/WD22_gross_flows_London_2021_series_",i,".rds")) %>% 
    mutate(geography = "WD22CD") %>%
    rename(gss_code_ward = WD22CD) %>% 
    select(geography, year, gss_code_ward, sex, age, inflow, outflow)
}

gross_flows <- rbindlist(gross_flows) %>% 
  left_join(ward_LAD, by = "gss_code_ward")

ward_inflow <- gross_flows %>%
  select(geography, year, gss_code, gss_code_ward, sex, age, inflow) %>%
  data.frame()

ward_outflow <- gross_flows %>% select(-inflow)  %>%
  select(geography, year, gss_code, gss_code_ward, sex, age, outflow) %>%
  data.frame()

rm(gross_flows, i)

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

sum(is.na(ward_births))
sum(is.na(ward_deaths))
sum(is.na(ward_pop))
sum(is.na(ward_inflow))
sum(is.na(ward_outflow))

#-------------------------------------------------------------------------------

saveRDS(ward_births, paste0(input_data_dir, "backseries/ward_births_WD22CD.rds"))
saveRDS(ward_deaths, paste0(input_data_dir, "backseries/ward_deaths_WD22CD.rds"))
saveRDS(ward_pop, paste0(input_data_dir, "backseries/ward_population_WD22CD.rds"))
saveRDS(ward_inflow, paste0(input_data_dir, "backseries/ward_inflow_WD22CD.rds"))
saveRDS(ward_outflow, paste0(input_data_dir, "backseries/ward_outflow_WD22CD.rds"))

rm(list=ls())
