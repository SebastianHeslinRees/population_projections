library(dplyr)
library(data.table)
library(popmodules)
library(stringr)

fpaths <- list(lad_ce_pop_2021 = "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/2021_census_TS047_CE_pop_LAD.csv",
               msoa_ce_pop_2021 = "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/2021_census_TS047_CE_pop_MSOA.csv",
               mye_pop = "input_data/mye/2021/population_gla.rds",
               msoa_to_lad_lookup = "Q:/Teams/D&PA/Demography/Projections/model_lookups/MSOA_(2021)_to_Local_Authority_District_(2022).csv",
               ons_2016_hh_pop = "input_data/household_model/ons_communal_establishment_population_(2021_geog).rds")

#----

#LAD

ce_pop_LAD <- fread(fpaths$lad_ce_pop_2021) %>% 
  data.frame() %>% 
  rename(gss_code = LAD22CD) %>% 
  filter(str_detect(var, "Resident")) %>% 
  mutate(sex = ifelse(str_detect(var, "Female"), "female", "male"),
         age_group = ifelse(sex == "female", substr(var,24,31), substr(var,22,29)),
         min_age = substr(age_group, 1, 2),
         max_age = case_when(min_age == "0 " ~ substr(age_group, 6, 7),
                             min_age == 85 ~ "90",
                             TRUE ~ substr(age_group, 7, 8)),
         min_age = as.numeric(min_age),
         max_age = as.numeric(max_age)) %>% 
  select(-var)

all_pop <- readRDS(fpaths$mye_pop) %>% 
  filter(year == 2021) %>% 
  filter_to_LAs()

ce_pop_LAD_list <- list()

for(group in unique(ce_pop_LAD$age_group)){
  
  p1 <- filter(ce_pop_LAD, age_group == group)
  a <- unique(p1$min_age)
  b <- unique(p1$max_age)
  p2 <- filter(all_pop, age %in% a:b)
  
  ce_pop_LAD_list[[group]] <- distribute_within_age_band(popn_1 = p1,
                                                         popn_2 = p2,
                                                         popn_1_col = "value",
                                                         popn_2_col = "popn",
                                                         min_age = a,
                                                         max_age = b,
                                                         col_aggregation = c("gss_code", "sex"),
                                                         additional_dist_cols = "year")
  
  #print(paste(sum(ce_pop_LAD_list[[group]]$value), sum(p1$value)))
}

ce_pop_LAD_sya <- bind_rows(ce_pop_LAD_list) %>% 
  rename(ce_pop = value) %>% 
  select(gss_code, year, sex, age, ce_pop) %>% 
  aggregate_regions(england = TRUE)

all_pop <- aggregate_regions(all_pop, england = TRUE)

#---

# HOUSEHOLD MODEL AGE GROUPS

ons_2016_ce_pop <- readRDS(fpaths$ons_2016_hh_pop)

ons_groups <- ons_2016_ce_pop %>% 
  select(age_group) %>% 
  distinct() %>% 
  mutate(min_age = case_when(substr(age_group,1,1) == "0" ~ "0",
                             TRUE ~ substr(age_group,1,2)),
         max_age = case_when(substr(age_group,1,1) == "0" ~ "15",
                             substr(age_group,1,2) == "90" ~ "90",
                             TRUE ~ substr(age_group,4,5)))

ce_groups_LAD_list <- list()

for(group in unique(ons_groups$age_group)){
  
  p1 <- filter(ons_groups, age_group == group) %>% mutate(year = 2021)
  a <- unique(p1$min_age)
  b <- unique(p1$max_age)
  ce_groups_LAD_list[[group]] <- data.frame(age = a:b) %>% 
    mutate(year = 2021) %>% 
    left_join(p1, by = "year") %>% 
    select(age_group, age)
}

ce_groups_LAD <- bind_rows(ce_groups_LAD_list) %>% 
  right_join(ce_pop_LAD_sya, by = "age") %>% 
  left_join(all_pop, by = c("gss_code", "year", "sex", "age")) %>% 
  group_by(gss_code, age_group, sex, year) %>% 
  summarise(ce_pop = sum(ce_pop), popn = sum(popn),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(ce_rate = ifelse(age_group %in% c("75_79","80_84","85_89", "90+"), ce_pop / popn, NA)) %>% 
  select("gss_code","age_group","sex","year","ce_pop","ce_rate") %>% 
  project_forward_flat(last_proj_yr = 2050) %>% 
  filter(gss_code %in% ons_2016_ce_pop$gss_code) %>% 
  bind_rows(filter(ons_2016_ce_pop, year < 2021))


#---

saveRDS(ce_pop_LAD_sya, "input_data/household_model/2021_census_ce_pop_LAD21CD_SYA.rds")
saveRDS(ce_groups_LAD, "input_data/household_model/2021_census_ce_pop_LAD21CD_grouped.rds")

#---

#### MSOA

ce_pop_msoa <- fread(fpaths$msoa_ce_pop_2021) %>% 
  data.frame() %>% 
  filter(str_detect(var, "Resident")) %>% 
  mutate(sex = ifelse(str_detect(var, "Female"), "female", "male"),
         age_group = ifelse(sex == "female", substr(var,24,31), substr(var,22,29)),
         min_age = substr(age_group, 1, 2),
         max_age = case_when(min_age == "0 " ~ substr(age_group, 6, 7),
                             min_age == 85 ~ "90",
                             TRUE ~ substr(age_group, 7, 8)),
         min_age = as.numeric(min_age),
         max_age = as.numeric(max_age)) %>% 
  select(-var)

seed_pop_msoa <- expand.grid(MSOA21CD = unique(ce_pop_msoa$MSOA21CD),
                             sex = c("female","male"),
                             age = 0:90,
                             value = 1,
                             year = 2021,
                             stringsAsFactors = FALSE) %>% 
  data.frame()

ce_pop_list_msoa <- list()

for(group in unique(ce_pop_msoa$age_group)){
  
  p1 <- filter(ce_pop_msoa, age_group == group)
  a <- unique(p1$min_age)
  b <- unique(p1$max_age)
  p2 <- filter(seed_pop_msoa, age %in% a:b)
  
  ce_pop_list_msoa[[group]] <- distribute_within_age_band(popn_1 = p1,
                                                          popn_2 = p2,
                                                          popn_1_col = "value",
                                                          popn_2_col = "value",
                                                          min_age = a,
                                                          max_age = b,
                                                          col_aggregation = c("MSOA21CD", "sex"),
                                                          additional_dist_cols = "year")
}

msoa_to_lad <- fread(fpaths$msoa_to_lad_lookup) %>%
  data.frame() %>% 
  select(MSOA21CD, LAD22CD)

ce_pop_msoa_sya <- bind_rows(ce_pop_list_msoa) %>% 
  rename(ce_pop = value) %>% 
  select(MSOA21CD, year, sex, age, ce_pop) %>% 
  left_join(msoa_to_lad, by = "MSOA21CD") %>% 
  rename(gss_code = LAD22CD) %>% 
  constrain_component(constraint = ce_pop_LAD_sya,
                      col_aggregation = c("gss_code","year","sex","age"),
                      col_popn = "ce_pop") %>% 
  select(MSOA21CD, LAD22CD = gss_code, year, sex, age, ce_pop)

#---

saveRDS(ce_pop_msoa_sya, "input_data/household_model/2021_census_ce_pop_MSOA21CD_sya.rds")

rm(list=ls())
