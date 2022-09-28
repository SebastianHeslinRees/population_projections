library(dplyr)
library(data.table)
library(popmodules)

message("LonLUTI zones backseries")

# Create file structure

f_paths <- list(input_data_dir = "input_data/flexible_area_model/",
                data_dir_Q_drive = "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/",
                target_lsoa_lookup_bf = "lookups/lsoa_to_lonLUTI3_best_fit.rds",
                target_lsoa_lookup_prop = "lookups/lsoa_to_lonLUTI3_proportional.rds",
                target_oa_lookup =  "lookups/oa_to_LonLUTI3_lookup.rds",
                gss_code_lookup = "input_data/lookup/gss_code_to_name.rds",
                target_population = "estimates_series/LonLUTI3/LonLUTI3_population.rds",
                target_births = "estimates_series/LonLUTI3/LonLUTI3_births.rds",
                target_deaths = "estimates_series/LonLUTI3/LonLUTI3_deaths.rds",
                target_gross_flows = "gross_migration/LonLUTI3/LonLUTI3_gross_flows_")


out_paths <- list(target_lsoa_lookup_bf = "lookups/lsoa_to_lonLUTI3_best_fit_lookup.rds",
                  target_lsoa_lookup_prop = "lookups/lsoa_to_lonLUTI3_proportional.rds",
                  target_oa_lookup = "lookups/oa_to_LonLUTI3_lookup.rds",
                  target_name_lookup = "lookups/lonLUTI_name_lookup.rds",
                  target_births = paste0(f_paths$input_data_dir, "backseries/lonLUTI_births.rds"),
                  target_deaths = paste0(f_paths$input_data_dir, "backseries/lonLUTI_deaths.rds"),
                  target_population = paste0(f_paths$input_data_dir, "backseries/lonLUTI_population.rds"),
                  target_inflow = paste0(f_paths$input_data_dir, "backseries/lonLUTI_inflow.rds"),
                  target_outflow = paste0(f_paths$input_data_dir, "backseries/lonLUTI_outflow.rds"),
                  dwellings_to_hh = paste0(f_paths$input_data_dir, "processed/dwelling_2_hh_ratio_lonLUTI.rds"),
                  comm_est_sya = paste0(f_paths$input_data_dir, "processed/communal_establishment_popn_lonLUTI.rds"))

#-------------------------------------------------------------------------------

# Lookups

#LSOA point in polygon best fit lookup
source("E:/project_folders/demography/wil/regrosser/functions/fn_point_in_polygon_lookup_sf.R")
lsoa_to_lonLUTI_best_fit <- .point_in_polygon_lookup(polygon_dir = "Q:/Teams/D&PA/Data/TfL/LonLUTI13GIS",
                                            polygon_shape_file = "LonLUTI3",
                                            target_name = "LonLUTI3",
                                            match_unmatched_points = TRUE,
                                            points_dir = "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/EnglandWales/Lower/ESRI",
                                            points_shape_file = "LSOA_2011_EW_PWC",
                                            points_name = "LSOA11CD")[[1]] %>% 
  rename(gss_code_lsoa = LSOA11CD)

#LSOA polygon overlap lookup
source("E:/project_folders/demography/wil/regrosser/functions/fn_ploygon_to_polygon_lookup_sf.R")
lsoa_to_lonLUTI_proportional <- .point_in_polygon_lookup(polygon_dir = "Q:/Teams/D&PA/Data/TfL/LonLUTI13GIS",
                                                     polygon_shape_file = "LonLUTI3",
                                                     target_name = "LonLUTI3",
                                                     match_unmatched_points = TRUE,
                                                     points_dir = "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/EnglandWales/Lower/ESRI",
                                                     points_shape_file = "LSOA_2011_EW_PWC",
                                                     points_name = "LSOA11CD")[[1]] %>% 
  rename(gss_code_lsoa = LSOA11CD) %>%
  rename(lsoa_share = )


oa_to_lonLUTI <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_oa_lookup)) 

lonLUTI_name_lookup <- oa_to_lonLUTI %>%
  left_join(readRDS(f_paths$gss_code_lookup), by = "gss_code") %>% 
  select(LonLUTI3, gss_code, gss_name) %>%
  mutate(name = LonLUTI3)  %>% 
  data.frame() %>% 
  distinct() %>% 
  select(gss_code, la_name = gss_name, LonLUTI3, name)

saveRDS(lsoa_to_lonLUTI_best_fit, paste0(f_paths$input_data_dir, out_paths$target_lsoa_lookup_bf))
saveRDS(lsoa_to_lonLUTI_proportional, paste0(f_paths$input_data_dir, out_paths$target_lsoa_lookup_prop))
saveRDS(oa_to_lonLUTI, paste0(f_paths$input_data_dir, out_paths$target_oa_lookup))
saveRDS(lonLUTI_name_lookup, paste0(f_paths$input_data_dir, out_paths$target_name_lookup))

#-------------------------------------------------------------------------------

# data contains non-London lonLUTIs from the regrosser process
# these present as NA in the WD13CD column
# filter these out

lonLUTI_pop <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_population)) %>% 
  mutate(geography = "LonLUTI3") %>% 
  select(geography, year, LonLUTI3, sex, age, popn)

lonLUTI_births <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_births)) %>% 
  mutate(geography = "LonLUTI3") %>% 
  select(geography, year, LonLUTI3, sex, age, births)

lonLUTI_deaths <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_deaths)) %>% 
  mutate(geography = "LonLUTI3") %>% 
  select(geography, year, LonLUTI3, sex, age, deaths)

#-------------------------------------------------------------------------------

gross_flows <- lapply(2011:2020, function(x){
  readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_gross_flows, x, ".rds"))
}) %>% rbindlist()

lonLUTI_inflow <- gross_flows %>% select(-outflow) %>% data.frame()
lonLUTI_outflow <- gross_flows %>% select(-inflow) %>% data.frame()

#-------------------------------------------------------------------------------

# Dwelling to household ratio

dwellings_to_hh <- fread(paste0(f_paths$data_dir_Q_drive, "dwelling_to_hh_LSOA.csv")) %>%
  data.frame() %>%
  left_join(lsoa_to_lonLUTI_best_fit, by="gss_code_lsoa") %>%
  group_by(LonLUTI3) %>%
  summarise(d2hh_ratio = sum(households)/sum(dwellings),
            .groups = 'drop_last') %>%
  data.frame() %>%
  filter(LonLUTI3 %in% lonLUTI_pop$LonLUTI3)

#-------------------------------------------------------------------------------

# Communal Establishment populations
# Create single year of age from grouped data

communal_est_lsoa <- rbind(
  fread(paste0(f_paths$data_dir_Q_drive, 'communal_est_males_lsoa.csv'), header = TRUE),
  fread(paste0(f_paths$data_dir_Q_drive, 'communal_est_females_lsoa.csv'), header = TRUE)) %>%
  tidyr::pivot_longer(values_to = "ce_popn", names_to = "age_group", cols = starts_with("Age")) %>%
  mutate(age_min = as.numeric(substr(age_group, 5,6))) %>%
  mutate(age_max = ifelse(age_min < 10, substr(age_group, 9,10),
                          substr(age_group, 11,12))) %>%
  mutate(age_max = ifelse(age_min == 15, 15,
                          ifelse(age_min == 85, 90,
                                 as.numeric(age_max)))) %>%
  data.frame()

communal_est_lonLUTI <- communal_est_lsoa  %>%
  left_join(lsoa_to_lonLUTI_best_fit, by="gss_code_lsoa") %>%
  group_by(LonLUTI3, sex, age_group, age_min, age_max) %>%
  summarise(ce_popn = sum(ce_popn), .groups = 'drop_last') %>%
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

message(paste("There are", sum(is.na(communal_est_lsoa)), "NAs in the communal establishment dataframe"))

comm_est_sya <- rbindlist(comm_est_lonLUTI_sya) %>%
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
popmodules::validate_same_geog(lonLUTI_pop, comm_est_sya, "LonLUTI3", "LonLUTI3")
popmodules::validate_same_geog(lonLUTI_pop, dwellings_to_hh, "LonLUTI3", "LonLUTI3")

sum(is.na(lonLUTI_pop))
sum(is.na(lonLUTI_births))
sum(is.na(lonLUTI_deaths))
sum(is.na(lonLUTI_inflow))
sum(is.na(lonLUTI_outflow))
sum(is.na(comm_est_lonLUTI_sya))
sum(is.na(dwellings_to_hh))

#-------------------------------------------------------------------------------

saveRDS(lonLUTI_births, out_paths$target_births)
saveRDS(lonLUTI_deaths, out_paths$target_deaths)
saveRDS(lonLUTI_pop, out_paths$target_population)
saveRDS(lonLUTI_inflow, out_paths$target_inflow)
saveRDS(lonLUTI_outflow, out_paths$target_outflow)
saveRDS(dwellings_to_hh, out_paths$dwellings_to_hh)
saveRDS(comm_est_sya, out_paths$comm_est_sya)

rm(list=ls())

