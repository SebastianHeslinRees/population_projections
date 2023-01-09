library(data.table)
library(dplyr)
library(stringr)

#This script makes lookups and saves them to the Q drive & into the project

data_dir_Q_drive <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"
data_dir <- "input_data/flexible_area_model/"
dir.create(data_dir, showWarnings = FALSE)
dir.create(paste0(data_dir, "lookups"), showWarnings = FALSE)

#-------------------------------------------------------------------------------

# Ward 2022 to LAD 22
# Directly from ONS Geo-portal
WD22_to_LAD22 <- fread("Q:/Teams/D&PA/Demography/Projections/model_lookups/WD22_LAD22_UK_LU_provisional.csv") %>% 
  mutate(WD22NM = str_remove_all(WD22NM, "\\.")) %>% 
  data.frame()

city_wards <- filter(WD22_to_LAD22, LAD22CD == "E09000001") %>% pull(WD22CD)

WD22_to_LAD22 <- WD22_to_LAD22 %>% 
  mutate(WD22CD = ifelse(LAD22CD=="E09000001", LAD22CD, WD22CD),
         WD22NM = ifelse(LAD22CD=="E09000001", LAD22NM, WD22NM)) %>% 
  select(-WD22NMW) %>% 
  distinct()

#Output Area to Wards - Best Fit

oa_to_wd22 <- fread(paste0(data_dir_Q_drive, 'lookups/oa_to_WD22.csv')) %>%
  select(OA11CD = ZONEID, ward_name, LAD22CD = la_gsscode, gss_name = la_distric) %>% 
  mutate(ward_name = str_remove_all(ward_name, "\\."),
         ward_name = str_replace_all(ward_name, "Shirly", "Shirley")) %>% 
  data.frame() %>% 
  left_join(WD22_to_LAD22, by = c("ward_name"="WD22NM", "LAD22CD")) %>% 
  select(OA11CD, WD22CD, ward_name, LAD22CD, gss_name)  %>%
  mutate(ward_name = ifelse(LAD22CD == "E09000001", "City of London", ward_name),
         WD22CD = ifelse(LAD22CD == "E09000001", "E09000001", WD22CD))

#sum(is.na(oa_to_wd22))

#There is no OA linked to Stratford Olympic Park because of population weighting from 2011
#Manually adjust so that that the OA that covers the ward is associated with it
oa_to_wd22 <- oa_to_wd22 %>% 
  mutate(WD22CD = ifelse(OA11CD == "E00175033", "E05013925", WD22CD),
         ward_name = ifelse(OA11CD == "E00175033", "Stratford Olympic Park", ward_name))

saveRDS(oa_to_wd22, paste0(data_dir_Q_drive, "lookups/oa_to_WD22_lookup_best_fit.rds"))



#-------------------------------------------------------------------------------

#LSOA to ward - best fit

lsoa_to_WD_not_LDN <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_to_WD20_lookup.rds")) %>% 
  rename(LSOA11CD = gss_code_lsoa, WD22CD = gss_code_ward, LAD22CD = gss_code) %>% 
  filter(substr(LAD22CD,1,3)!="E09") %>% 
  select(LSOA11CD, lsoa_name, WD22CD, ward_name, LAD22CD, gss_name)

lsoa_to_wd22 <-  fread(paste0(data_dir_Q_drive, 'lookups/lsoa_to_WD22.csv')) %>%
  data.frame() %>% 
  mutate(ward_name = str_remove_all(ward_name, "\\."),
         ward_name = str_replace_all(ward_name, "Shirly", "Shirley")) %>% 
  filter(substr(la_gsscode,1,3)=="E09") %>%
  mutate(ward_name = ifelse(la_gsscode == "E09000001", "City of London", ward_name)) %>% 
  left_join(WD22_to_LAD22, by = c("ward_name"="WD22NM", "la_gsscode"="LAD22CD")) %>% 
  select(LSOA11CD = lsoa11cd, lsoa_name = lsoa11nm, WD22CD,
         ward_name, LAD22CD = la_gsscode, gss_name = LAD22NM) %>% 
  rbind(lsoa_to_WD_not_LDN) 

#Same problem for Stratford Olympic Park as with OAs above
lsoa_to_wd22 <- lsoa_to_wd22 %>% 
  mutate(WD22CD = ifelse(LSOA11CD == "E01033583", "E05013925", WD22CD),
         ward_name = ifelse(LSOA11CD == "E01033583", "Stratford Olympic Park", ward_name))

saveRDS(lsoa_to_wd22, paste0(data_dir_Q_drive, "lookups/lsoa_to_WD22_lookup_best_fit.rds"))

#-------------------------------------------------------------------------------

# proportional_oa_ward <- list.files(paste0(data_dir_Q_drive, "oa_WD22_lookups"), full.names = T) %>%
#   lapply(fread) %>%
#   rbindlist() %>% 
#   data.frame() %>% 
#   rename(OA11CD = oa, WD22CD = ward_gss, gss_code = gss) %>% 
#   mutate(WD22CD = ifelse(gss_code == "E09000001", "E09000001", WD22CD)) %>% 
#   group_by(OA11CD, WD22CD, year) %>% 
#   summarise(oa_ward_weight = sum(oa_ward_weight), .groups = 'drop_last') %>% 
#   data.frame() %>% 
#   left_join(WD22_to_LAD22, by = "WD22CD") %>% 
#   select(OA11CD, WD22CD, year, oa_ward_weight)
# 
# saveRDS(proportional_oa_ward, paste0(data_dir_Q_drive,"lookups/oa11_to_WD22_proportional.rds"))

#-------------------------------------------------------------------------------

#Ward to NUTS2 (HMA) lookup
LAD_to_NUTS2 <- readRDS(paste0(data_dir_Q_drive, "lookups/LAD_to_NUTS2.rds"))
wD22_to_NUTS2 <- left_join(WD22_to_LAD22, LAD_to_NUTS2, by = c("LAD22CD"="gss_code")) %>% 
  select(gss_code_ward = WD22CD, constraint_area)

#OA21 to WD22
oa21_ward22 <- fread("Q:/Teams/D&PA/Census/2021 Census/central_data_folder/geog_lookups/2021_oa_2022_ward.csv") %>% 
  mutate(WD22NM = ifelse(WD22CD %in% city_wards, "City of London", WD22NM),
         WD22CD = ifelse(WD22CD %in% city_wards, "E09000001", WD22CD)) %>% 
  data.frame()

saveRDS(oa21_ward22, paste0(data_dir, "lookups/OA21CD_to_WD22CD_proportional.rds"))

#OA11 to WD22
oa11_ward22 <- readRDS("Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/lookups/oa_to_ward_2022_proportional_london_only.rds") %>% 
  rename(oa_ward_weight = scaling_factor)
saveRDS(oa11_ward22, paste0(data_dir, "lookups/OA11CD_to_WD22CD_proportional.rds"))

#OA11 to WD13
oa11_ward13 <- readRDS("Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/lookups/oa_to_WD13_lookup.rds")
saveRDS(oa11_ward13, paste0(data_dir, "lookups/OA11CD_to_WD13CD_best_fit.rds"))

#-------------------------------------------------------------------------------

# Save to project folder

lsoa_to_lsoa <- readRDS(paste0(data_dir_Q_drive, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))
ward_names <- select(WD22_to_LAD22,
                     gss_code_ward=WD22CD,
                     ward_name=WD22NM,
                     gss_code=LAD22CD,
                     la_name=LAD22NM)

saveRDS(lsoa_to_wd22, paste0(data_dir, "lookups/lsoa_to_WD22_lookup_best_fit.rds"))
saveRDS(WD22_to_LAD22, paste0(data_dir, "lookups/dummy_WD22_codes.rds"))
saveRDS(ward_names, paste0(data_dir, "lookups/ward_2022_name_lookup.rds"))
saveRDS(oa_to_wd22, paste0(data_dir, "lookups/oa11_to_WD22_lookup_best_fit.rds"))
saveRDS(lsoa_to_lsoa, paste0(data_dir, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))
saveRDS(wD22_to_NUTS2, "input_data/flexible_area_model/lookups/WD22CD_to_NUTS2.rds")

#-------------------------------------------------------------------------------

rm(list = ls())
