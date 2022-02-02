library(data.table)
library(dplyr)
library(stringr)

#This script makes lookups and saves them to the Q drive
#A separate script saves them into the projections project

data_dir_Q_drive <- "Q:/Teams/D&PA/Demography/Projections/new_ward_model_data/"

#-------------------------------------------------------------------------------

#Dummy codes

dummy_ward_codes <- fread(paste0(data_dir_Q_drive, "lookups/London_wards_2022.csv")) %>% 
  select(gss_code = la_gsscode, ward_name, la_name = la_distric) %>% 
  distinct %>% 
  arrange(gss_code, ward_name) %>% 
  group_by(gss_code) %>% 
  mutate(count = 1:n()) %>% 
  data.frame() %>% 
  mutate(count = ifelse(count<10, paste0("0",count), as.character(count))) %>% 
  mutate(gss_code_ward = paste0(gss_code,"_",count)) %>% 
  mutate(ward_name = str_replace_all(ward_name, "\\.", "")) %>% 
  select(-count) %>% 
  filter(gss_code != "E09000001") %>% 
  rbind(data.frame(gss_code = "E09000001",
                   ward_name = "City of London",
                   gss_code_ward = "E09000001",
                   la_name = "City of London"))

#saveRDS(dummy_ward_codes, paste0(data_dir_Q_drive, "lookups/dummy_WD22_code_lookup.rds"))

#-------------------------------------------------------------------------------

provisional_WD22 <- fread(paste0(data_dir_Q_drive, "lookups/WD22_LAD22_UK_LU_provisional.csv")) %>% 
  right_join(dummy_ward_codes, by=c("WD22NM"="ward_name", "LAD22CD"="gss_code")) %>% 
  mutate(WD22CD = ifelse(LAD22CD == "E09000001", "E09000001", WD22CD)) %>% 
  data.frame() %>% 
  rename(dummy_code = gss_code_ward) %>% 
  rename(gss_code_ward = WD22CD, ward_name = WD22NM, gss_code = LAD22CD) %>% 
  select(-LAD22NM)

saveRDS(provisional_WD22, paste0(data_dir_Q_drive, "lookups/provisional_and_dummy_WD22_code_lookup.rds"))

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

rm(data_dir_Q_drive, dummy_ward_codes, provisional_WD22, oa_to_wd22, lsoa_to_WD20,
   lsoa_to_wd22, proportional_oa_ward)
