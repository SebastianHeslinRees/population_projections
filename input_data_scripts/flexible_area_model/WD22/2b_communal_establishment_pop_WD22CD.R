# Output Area and Ward Communal Establishment Populations
message("Ward 2022 - Communal Establishment Populations")

library(dplyr)
library(data.table)
library(popmodules)
library(stringr)

fpaths <- list(msoa_ce_pop = "input_data/household_model/2021_census_ce_pop_MSOA21CD_sya.rds",
               oa_ce_pop_2021 = "Q:/Teams/D&PA/Census/2021 Census/central_data_folder/raw_data/2021/1. Demography and migration/oa/residence_type.csv",
               oa21_wd22 = "input_data/flexible_area_model/lookups/OA21CD_to_WD22CD_proportional.rds",
               oa21_msoa21 = "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/lookups/OAs_to_LSOAs_to_MSOAs_to_LEP_to_LAD_(May_2022)_Lookup_in_England.csv")

#----

#### OA

#lookup
oa21_msoa21 <- fread(fpaths$oa21_msoa21) %>% 
  select(OA21CD, MSOA21CD) %>% 
  distinct() %>% 
  data.frame() # TODO ONLY HAS ENGLAND - WHY?

#lookup
oa21_wd22 <- readRDS(fpaths$oa21_wd22) %>% 
  data.frame() %>% 
  mutate(WD22CD = ifelse(LAD22CD == "E09000001", "E09000001", WD22CD)) %>% 
  group_by(OA21CD, WD22CD) %>% 
  summarise(area = sum(area_of_OA_inside_ward), .groups = 'drop_last') %>% 
  group_by(OA21CD) %>% 
  mutate(total_area = sum(area)) %>% 
  data.frame() %>% 
  mutate(proportion = area/total_area) %>% 
  select(OA21CD, WD22CD, proportion)

#2021 census OA total ce pop
ce_pop_OA <- fread(fpaths$oa_ce_pop_2021) %>% 
  data.frame() %>% 
  filter(name == "Lives in a communal establishment") %>% 
  select(OA21CD = geography,
         OA_total = value) %>% 
  filter(substr(OA21CD,1,1)=="E") #TODO

#Give the OA total the MOSA age/sex distribution
ce_pop_msoa_sya <- readRDS(fpaths$msoa_ce_pop)

msoa_distribution <- ce_pop_msoa_sya %>% 
  group_by(MSOA21CD) %>% 
  mutate(msoa_total = sum(ce_pop)) %>% 
  data.frame() %>% 
  mutate(msoa_dist = ifelse(msoa_total == 0, 0, ce_pop / msoa_total)) %>%
  rename(msoa_ce_pop = ce_pop) %>% 
  select(MSOA21CD, sex, age, msoa_ce_pop, msoa_total, msoa_dist)

ce_pop_OA_sya <- ce_pop_OA %>%
  left_join(oa21_msoa21, by = "OA21CD") %>%
  right_join(msoa_distribution, by = "MSOA21CD") %>%
  filter(substr(MSOA21CD,1,1)=="E") %>% #TODO
  mutate(oa_ce_pop = OA_total * msoa_dist,
         year = 2021) %>% 
  select(OA21CD, MSOA21CD, year, sex, age, oa_ce_pop)

# Constrain to MSOA because the OA data has non-residents
ce_pop_OA_sya <- ce_pop_OA_sya %>% 
  constrain_component(constraint = ce_pop_msoa_sya,
                      col_aggregation = c("MSOA21CD","year","sex","age"),
                      col_popn = "oa_ce_pop",
                      col_constraint = "ce_pop") %>% 
  rename(ce_pop = oa_ce_pop) %>% 
  select(OA21CD, year, sex, age, ce_pop)

#### WARD 2022

ce_ward_sya <- ce_pop_OA_sya %>% 
  full_join(oa21_wd22, by= "OA21CD") %>% 
  filter(!is.na(proportion)) %>% 
  group_by(gss_code_ward = WD22CD, sex, age) %>% 
  summarise(ce_popn = sum(ce_pop*proportion), .groups = 'drop_last') %>% 
  data.frame()

#---

saveRDS(ce_ward_sya, "input_data/flexible_area_model/processed/communal_establishment_popn_WD22CD.rds")

#---

rm(list=ls())