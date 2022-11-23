library(data.table)
library(dplyr)
library(stringr)

#This script makes lookups and saves them to the Q drive & into the project

data_dir_Q_drive <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"
data_dir <- "input_data/flexible_area_model/"

#-------------------------------------------------------------------------------

#Output Area to Wards - Best Fit

oa_to_wd22 <- fread(paste0(data_dir_Q_drive, 'lookups/oa_to_WD22.csv')) %>%
  select(gss_code_oa = ZONEID, ward_name, gss_code = la_gsscode, gss_name = la_distric) %>% 
  mutate(ward_name = str_remove_all(ward_name, "\\."),
         ward_name = str_replace_all(ward_name, "Shirly", "Shirley")) %>% 
  data.frame() %>% 
  left_join(provisional_WD22, by = c("ward_name", "gss_code")) %>% 
  select(gss_code_oa, gss_code_ward, ward_name, gss_code, gss_name)  %>%
  mutate(ward_name = ifelse(gss_code == "E09000001", "City of London", ward_name),
         gss_code_ward = ifelse(gss_code == "E09000001", "E09000001", gss_code_ward))

sum(is.na(oa_to_wd22))

#There is no OA linked to Stratford Olympic Park because of population weighting 2011
#Manually adjust so that that OA that covers the ward is associated with it
oa_to_wd22 <- oa_to_wd22 %>% 
  mutate(gss_code_ward = ifelse(gss_code_oa == "E00175033", "E05013925", gss_code_ward),
         ward_name = ifelse(gss_code_oa == "E00175033", "Stratford Olympic Park", ward_name))

saveRDS(oa_to_wd22, paste0(data_dir_Q_drive, "lookups/oa_to_WD22_lookup_best_fit.rds"))

#-------------------------------------------------------------------------------

#LSOA to ward - best fit

lsoa_to_WD20 <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_to_WD20_lookup.rds"))

lsoa_to_wd22 <-  fread(paste0(data_dir_Q_drive, 'lookups/lsoa_to_WD22.csv')) %>%
  data.frame() %>% 
  mutate(ward_name = str_remove_all(ward_name, "\\."),
         ward_name = str_replace_all(ward_name, "Shirly", "Shirley")) %>% 
  filter(substr(la_gsscode,1,3)=="E09") %>%
  mutate(ward_name = ifelse(la_gsscode == "E09000001", "City of London", ward_name)) %>% 
  left_join(provisional_WD22, by = c("ward_name", "la_gsscode"="gss_code")) %>% 
  select(gss_code_lsoa = lsoa11cd, lsoa_name = lsoa11nm, gss_code_ward, ward_name, gss_code = la_gsscode, gss_name = la_distric) %>% 
  rbind(filter(lsoa_to_WD20, substr(gss_code,1,3)!="E09")) 

#Same as with OAs above
lsoa_to_wd22 <- lsoa_to_wd22 %>% 
  mutate(gss_code_ward = ifelse(gss_code_lsoa == "E01033583", "E05013925", gss_code_ward),
         ward_name = ifelse(gss_code_lsoa == "E01033583", "Stratford Olympic Park", ward_name))

saveRDS(lsoa_to_wd22, paste0(data_dir_Q_drive, "lookups/lsoa_to_WD22_lookup_best_fit.rds"))

#-------------------------------------------------------------------------------

proportional_oa_ward <- list.files(paste0(data_dir_Q_drive, "oa_WD22_lookups"), full.names = T) %>%
  lapply(fread) %>%
  rbindlist() %>% 
  data.frame() %>% 
  rename(gss_code_oa = oa, dummy_code = ward_gss, gss_code = gss) %>% 
  mutate(dummy_code = ifelse(gss_code == "E09000001", "E09000001", dummy_code)) %>% 
  group_by(gss_code_oa, dummy_code, year) %>% 
  summarise(oa_ward_weight = sum(oa_ward_weight), .groups = 'drop_last') %>% 
  data.frame() %>% 
  left_join(provisional_WD22, by = "dummy_code") %>% 
  select(-dummy_code) 

saveRDS(proportional_oa_ward, paste0(data_dir_Q_drive,"lookups/oa_to_WD22_proportional.rds"))

#-------------------------------------------------------------------------------

#Ward to NUTS2 (HMA) lookup
LAD_to_NUTS2 <- readRDS("input_data/flexible_area_model/lookups/LAD_to_NUTS2.rds")
ward_to_LAD <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds")
wD22_to_NUTS2 <- left_join(ward_to_LAD, LAD_to_NUTS2, by = "gss_code") %>% 
  select(gss_code_ward, constraint_area)
saveRDS(wD22_to_NUTS2, "input_data/flexible_area_model/lookups/WD22CD_to_NUTS2.rds")

#-------------------------------------------------------------------------------

# Save to project folder

lsoa_to_lsoa <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))
ward_names <- select(provisional_WD22, gss_code_ward, ward_name, gss_code, la_name)

saveRDS(lsoa_to_wd22, paste0(data_dir, "lookups/lsoa_to_WD22_lookup_best_fit.rds"))
saveRDS(provisional_WD22, paste0(data_dir, "lookups/dummy_WD22_codes.rds"))
saveRDS(ward_names, paste0(data_dir, "lookups/ward_2022_name_lookup.rds"))
saveRDS(oa_to_wd22, paste0(data_dir, "lookups/oa_to_WD22_lookup_best_fit.rds"))
saveRDS(proportional_oa_ward, paste0(data_dir,"lookups/oa_to_WD22_proportional.rds"))
saveRDS(lsoa_to_lsoa, paste0(data_dir, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))

#-------------------------------------------------------------------------------

rm(list = ls())
