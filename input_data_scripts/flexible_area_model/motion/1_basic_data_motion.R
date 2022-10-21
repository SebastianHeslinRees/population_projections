library(dplyr)
library(data.table)
library(popmodules)

message("motion_zone zones backseries")

# Create file structure

f_paths <- list(input_data_dir = "input_data/flexible_area_model/",
                data_dir_Q_drive = "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/",
                target_lsoa_lookup_bf = "lookups/lsoa_to_motion_zone_best_fit.rds",
                target_lsoa_lookup_prop = "lookups/lsoa_to_motion_zone_proportional.rds",
                target_oa_lookup =  "lookups/oa_to_motion_zones_lookup.rds",
                gss_code_lookup = "input_data/lookup/gss_code_to_name.rds",
                target_population = "estimates_series/motion_zone/motion_zone_population.rds",
                target_births = "estimates_series/motion_zone/motion_zone_births.rds",
                target_deaths = "estimates_series/motion_zone/motion_zone_deaths.rds",
                target_gross_flows = "gross_migration/motion_zone/motion_zone_gross_flows_")


out_paths <- list(target_lsoa_lookup_bf = "lookups/lsoa_to_motion_zone_best_fit_lookup.rds",
                  target_lsoa_lookup_prop = "lookups/lsoa_to_motion_zone_proportional.rds",
                  target_oa_lookup = "lookups/oa_to_motion_zones_lookup.rds",
                  target_name_lookup = "lookups/motion_zone_name_lookup.rds",
                  target_births = paste0(f_paths$input_data_dir, "backseries/motion_zone_births.rds"),
                  target_deaths = paste0(f_paths$input_data_dir, "backseries/motion_zone_deaths.rds"),
                  target_population = paste0(f_paths$input_data_dir, "backseries/motion_zone_population.rds"),
                  target_inflow = paste0(f_paths$input_data_dir, "backseries/motion_zone_inflow.rds"),
                  target_outflow = paste0(f_paths$input_data_dir, "backseries/motion_zone_outflow.rds"),
                  dwellings_to_hh = paste0(f_paths$input_data_dir, "processed/dwelling_2_hh_ratio_motion_zone.rds"),
                  comm_est_sya = paste0(f_paths$input_data_dir, "processed/communal_establishment_popn_motion_zone.rds"),
                  constraint_lookup = "lookups/motion_zone_to_london_constraint.rds")

#-------------------------------------------------------------------------------

# Lookups

#LSOA point in polygon best fit lookup
source("E:/project_folders/demography/wil/regrosser/functions/fn_point_in_polygon_lookup_sf.R")
lsoa_to_motion_zone_best_fit <- .point_in_polygon_lookup(polygon_dir = "Q:/Teams/D&PA/Data/TfL/demand_zones",
                                                         polygon_shape_file = "Demand_Zones_wv9.3_polygon",
                                                         target_name = "Sequential",
                                                         match_unmatched_points = TRUE,
                                                         points_dir = "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/EnglandWales/Lower/ESRI",
                                                         points_shape_file = "LSOA_2011_EW_PWC",
                                                         points_name = "LSOA11CD")[[1]] %>% 
  rename(gss_code_lsoa = LSOA11CD) %>% 
  rename(motion_zone = Sequential)

#---

#LSOA polygon overlap lookup
source("E:/project_folders/demography/wil/regrosser/functions/fn_polygon_to_polygon_lookup_sf.R")
lsoa_to_motion_zone_proportional <- .polygon_to_polygon_lookup(target_polygon_dir = "Q:/Teams/D&PA/Data/TfL/demand_zones",
                                                               target_polygon_shape_file = "Demand_Zones_wv9.3_polygon",
                                                               target_name = "Sequential",
                                                               target_output_name = "motion_zone",
                                                               match_unmatched_base_polygons = TRUE,
                                                               base_polygon_dir = "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/EnglandWales/Lower/ESRI",
                                                               base_polygon_shape_file = "LSOA_2011_EW",
                                                               base_name = "LSOA11CD")[[1]] %>% 
  rename(gss_code_lsoa = LSOA11CD) %>%
  rename(lsoa_share = scaling_factor)

#---

#Name lookup needed for output functions
motion_zone_name_lookup <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_oa_lookup))  %>% 
  mutate(name = motion_zone)  %>%
  select(motion_zone, name) %>% 
  distinct()

#---

#London lookup for constraining
constraint_lookup <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_oa_lookup)) %>% 
  left_join(readRDS("input_data/flexible_area_model/lookups/oa_lsoa_msoa_lad11_lad21.rds"),
            by = "OA11CD") %>% 
  filter(substr(LAD11CD,1,3)=="E09") %>% 
  mutate(constraint_area = "London") %>% 
  select(motion_zone, constraint_area) %>% 
  unique()

#---

#save out
saveRDS(lsoa_to_motion_zone_best_fit, paste0(f_paths$input_data_dir, out_paths$target_lsoa_lookup_bf))
saveRDS(lsoa_to_motion_zone_proportional, paste0(f_paths$input_data_dir, out_paths$target_lsoa_lookup_prop))
saveRDS(oa_to_motion_zone, paste0(f_paths$input_data_dir, out_paths$target_oa_lookup))
saveRDS(motion_zone_name_lookup, paste0(f_paths$input_data_dir, out_paths$target_name_lookup))
saveRDS(constraint_lookup, paste0(f_paths$input_data_dir, out_paths$constraint_lookup))

#read back in - so that it can be run from here if needs be
lsoa_to_motion_zone_best_fit <- readRDS(paste0(f_paths$input_data_dir, out_paths$target_lsoa_lookup_bf))
lsoa_to_motion_zone_proportional <- readRDS(paste0(f_paths$input_data_dir, out_paths$target_lsoa_lookup_prop))
oa_to_motion_zone <- readRDS(paste0(f_paths$input_data_dir, out_paths$target_oa_lookup))
motion_zone_name_lookup <- readRDS(paste0(f_paths$input_data_dir, out_paths$target_name_lookup))
#-------------------------------------------------------------------------------

# data contains non-London motion_zones from the regrosser process
# these present as NA in the WD13CD column
# filter these out

motion_zone_pop <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_population)) %>% 
  mutate(geography = "motion_zone") %>% 
  select(geography, year, motion_zone, sex, age, popn)

motion_zone_births <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_births)) %>% 
  mutate(geography = "motion_zone") %>% 
  select(geography, year, motion_zone, sex, age, births)

motion_zone_deaths <- readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_deaths)) %>% 
  mutate(geography = "motion_zone") %>% 
  select(geography, year, motion_zone, sex, age, deaths)

#-------------------------------------------------------------------------------

gross_flows <- lapply(2011:2020, function(x){
  readRDS(paste0(f_paths$data_dir_Q_drive, f_paths$target_gross_flows, x, ".rds"))
}) %>% rbindlist()

motion_zone_inflow <- gross_flows %>% select(-outflow) %>% data.frame()
motion_zone_outflow <- gross_flows %>% select(-inflow) %>% data.frame()

#-------------------------------------------------------------------------------

# Dwelling to household ratio

dwellings_to_hh <- fread(paste0(f_paths$data_dir_Q_drive, "dwelling_to_hh_LSOA.csv")) %>%
  data.frame() %>%
  left_join(lsoa_to_motion_zone_proportional, by="gss_code_lsoa") %>%
  group_by(motion_zone) %>%
  summarise(d2hh_ratio = sum(households*lsoa_share)/sum(dwellings*lsoa_share),
            .groups = 'drop_last') %>%
  data.frame() %>%
  filter(motion_zone %in% motion_zone_pop$motion_zone)

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

communal_est_motion_zone <- communal_est_lsoa  %>%
  left_join(lsoa_to_motion_zone_proportional, by="gss_code_lsoa") %>%
  group_by(motion_zone, sex, age_group, age_min, age_max) %>%
  summarise(ce_popn = sum(ce_popn*lsoa_share), .groups = 'drop_last') %>%
  data.frame() %>%
  filter(motion_zone %in% motion_zone_pop$motion_zone)

comm_est_motion_zone_sya <- list()

for(group in unique(communal_est_motion_zone$age_group)){
  
  pop_1 <- filter(communal_est_motion_zone, age_group == group)
  pop_2 <- filter(motion_zone_pop, age %in%  unique(pop_1$age_min): unique(pop_1$age_max), year == 2011)
  
  comm_est_motion_zone_sya[[group]] <- distribute_within_age_band(pop_1,pop_2,
                                                                  "ce_popn","popn",
                                                                  unique(pop_1$age_min),
                                                                  unique(pop_1$age_max),
                                                                  col_aggregation = c("motion_zone","sex"))
}

message(paste("There are", sum(is.na(communal_est_lsoa)), "NAs in the communal establishment dataframe"))

comm_est_sya <- rbindlist(comm_est_motion_zone_sya) %>%
  select(motion_zone, sex, age, ce_popn) %>%
  data.frame()

rm(pop_1, pop_2, communal_est_lsoa, communal_est_motion_zone)

#-------------------------------------------------------------------------------

#Basic checks

range(motion_zone_births$year)
range(motion_zone_deaths$year)
range(motion_zone_pop$year)
range(motion_zone_inflow$year)
range(motion_zone_outflow$year)

popmodules::validate_same_geog(motion_zone_pop, motion_zone_births, "motion_zone", "motion_zone")
popmodules::validate_same_geog(motion_zone_pop, motion_zone_deaths, "motion_zone", "motion_zone")
popmodules::validate_same_geog(motion_zone_pop, motion_zone_inflow, "motion_zone", "motion_zone")
popmodules::validate_same_geog(motion_zone_pop, motion_zone_outflow, "motion_zone", "motion_zone")
popmodules::validate_same_geog(motion_zone_pop, comm_est_sya, "motion_zone", "motion_zone")
popmodules::validate_same_geog(motion_zone_pop, dwellings_to_hh, "motion_zone", "motion_zone")

sum(is.na(motion_zone_pop))
sum(is.na(motion_zone_births))
sum(is.na(motion_zone_deaths))
sum(is.na(motion_zone_inflow))
sum(is.na(motion_zone_outflow))
sum(is.na(comm_est_motion_zone_sya))
sum(is.na(dwellings_to_hh))

#-------------------------------------------------------------------------------

saveRDS(motion_zone_births, out_paths$target_births)
saveRDS(motion_zone_deaths, out_paths$target_deaths)
saveRDS(motion_zone_pop, out_paths$target_population)
saveRDS(motion_zone_inflow, out_paths$target_inflow)
saveRDS(motion_zone_outflow, out_paths$target_outflow)
saveRDS(dwellings_to_hh, out_paths$dwellings_to_hh)
saveRDS(comm_est_sya, out_paths$comm_est_sya)

rm(list=ls())

