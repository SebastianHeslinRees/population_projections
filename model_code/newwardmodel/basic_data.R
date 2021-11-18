library(dplyr)
library(data.table)

data_dir <- "model_code/newwardmodel/data/"
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

#-------------------------------------------------------------------------------

saveRDS(ward_births, paste0(data_dir, "ward_births_WD20CD.rds"))
saveRDS(ward_deaths, paste0(data_dir, "ward_deaths_WD20CD.rds"))
saveRDS(ward_pop, paste0(data_dir, "ward_population_WD20CD.rds"))
saveRDS(ward_inflow, paste0(data_dir, "ward_inflow_WD20CD.rds"))
saveRDS(ward_outflow, paste0(data_dir, "ward_outflow_WD20CD.rds"))

rm(list=ls())