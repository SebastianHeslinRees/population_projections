library(dplyr)
#library(data.table)
library(popmodules)
#library(stringr)

message("Ward 2022 - dwelling to household ratios")

# Create file structure

input_data_dir <- "input_data/flexible_area_model/"
Q_data <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"

dir.create(paste0(input_data_dir, "processed"), showWarnings = FALSE)

lsoa_to_ward <- readRDS(paste0(input_data_dir, "lookups/lsoa_to_WD22_lookup_best_fit.rds")) %>% 
  select(gss_code_lsoa = LSOA11CD, gss_code_ward = WD22CD)

ward_pop <- readRDS(paste0(input_data_dir, "backseries/ward_population_WD22CD.rds"))

#-------------------------------------------------------------------------------

# Dwelling to household ratio

dwellinngs_to_hh <- fread(paste0(Q_data, "dwelling_to_hh_LSOA.csv")) %>%
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

# communal_est_lsoa <- rbind(
#   fread(paste0(Q_data, 'communal_est_males_lsoa.csv'), header = TRUE),
#   fread(paste0(Q_data, 'communal_est_females_lsoa.csv'), header = TRUE)) %>% 
#   tidyr::pivot_longer(values_to = "ce_popn", names_to = "age_group", cols = starts_with("Age")) %>% 
#   mutate(age_min = as.numeric(substr(age_group, 5,6))) %>% 
#   mutate(age_max = ifelse(age_min < 10, substr(age_group, 9,10),
#                           substr(age_group, 11,12))) %>% 
#   mutate(age_max = ifelse(age_min == 15, 15,
#                           ifelse(age_min == 85, 90,
#                                  as.numeric(age_max)))) %>% 
#   data.frame()
# 
# message("ignore warnings about NAs - its lying")
# 
# communal_est_ward <- communal_est_lsoa  %>% 
#   left_join(lsoa_to_ward, by="gss_code_lsoa") %>% 
#   group_by(gss_code_ward, sex, age_group, age_min, age_max) %>% 
#   summarise(ce_popn = sum(ce_popn), .groups = 'drop_last') %>% 
#   data.frame() %>% 
#   filter(gss_code_ward %in% ward_pop$gss_code_ward)
# 
# comm_est_ward_sya <- list()
# 
# for(group in unique(communal_est_ward$age_group)){
#   
#   pop_1 <- filter(communal_est_ward, age_group == group)
#   pop_2 <- filter(ward_pop, age %in% unique(pop_1$age_min):unique(pop_1$age_max), year == 2011)
#   
#   comm_est_ward_sya[[group]] <- distribute_within_age_band(pop_1,pop_2,
#                                                            "ce_popn","popn",
#                                                            unique(pop_1$age_min),
#                                                            unique(pop_1$age_max),
#                                                            col_aggregation = c("gss_code_ward","sex"))
# }
# 
# comm_est_ward_sya <- rbindlist(comm_est_ward_sya) %>% 
#   select(gss_code_ward, sex, age, ce_popn) %>% 
#   data.frame()
# 
# #-------------------------------------------------------------------------------
# 
# # NOT USED YET 
# 
# #2021 Census Data - OA total CE popn
# oa21_wd22 <- readRDS("input_data/flexible_area_model/lookups/OA21CD_to_WD22CD_proportional.rds") %>% 
#   data.frame() %>% 
#   mutate(WD22CD = ifelse(LAD22CD == "E09000001", "E09000001", WD22CD)) %>% 
#   group_by(OA21CD, WD22CD) %>% 
#   summarise(area = sum(area_of_OA_inside_ward), .groups = 'drop_last') %>% 
#   group_by(OA21CD) %>% 
#   mutate(total_area = sum(area)) %>% 
#   data.frame() %>% 
#   mutate(proportion = area/total_area) %>% 
#   select(OA21CD, WD22CD, proportion)
# 
# ce_2021_census <- fread("Q:/Teams/D&PA/Census/2021 Census/central_data_folder/raw_data/2021/1. Demography and migration/oa/residence_type.csv") %>% 
#   data.frame() %>% 
#   filter(name == "Lives in a communal establishment") %>% 
#   select(OA21CD = geography,
#          ce_2021 = value) %>% 
#   full_join(oa21_wd22, by= "OA21CD") %>% 
#   filter(!is.na(proportion)) %>% 
#   group_by(gss_code_ward = WD22CD) %>% 
#   summarise(ce_2021 = sum(ce_2021*proportion), .groups = 'drop_last') %>% 
#   data.frame()
# 
# # OPTION 1 - Scale based on 2011 CE age/sex distribution
# 
# ce_popn_scaled_1 <- comm_est_ward_sya %>% 
#   group_by(gss_code_ward) %>% 
#   mutate(summed = sum(ce_popn)) %>% 
#   left_join(ce_2021_census, by = "gss_code_ward") %>% 
#   mutate(scaling = ifelse(summed == 0, 0, ce_2021 / summed),
#          ce_popn2 = ce_popn * scaling)
# 
# # OPTION 2 - Scale based on total ward age/sex distribution
# 
# ce_popn_scaled_2 <- ward_pop %>% 
#   filter(year == 2021) %>% 
#   group_by(gss_code_ward) %>% 
#   mutate(summed_popn = sum(popn),
#          scaling = ifelse(summed_popn==0, 0, popn/summed_popn)) %>% 
#   left_join(ce_2021_census, by = "gss_code_ward") %>% 
#   mutate(ce_popn = ce_2021 * scaling) %>% 
#   data.frame() %>% 
#   select(gss_code_ward, sex, age, ce_popn)

#-------------------------------------------------------------------------------

#popmodules::validate_same_geog(ward_pop, comm_est_ward_sya, "gss_code_ward", "gss_code_ward")
popmodules::validate_same_geog(ward_pop, dwellinngs_to_hh, "gss_code_ward", "gss_code_ward")

#sum(is.na(comm_est_ward_sya))
sum(is.na(dwellinngs_to_hh))

saveRDS(dwellinngs_to_hh, paste0(input_data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"))
#saveRDS(comm_est_ward_sya, paste0(input_data_dir, "processed/communal_establishment_popn_WD22CD.rds")) # TODO Change this when option decided

rm(list=ls())