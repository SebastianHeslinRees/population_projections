library(dplyr)
library(stringr)
library(data.table)
library(popmodules)

message("ONS small area population estimates")

most_recent_data_year <- 2019

#data paths
lsoa_dir <- "Q:/Teams/D&PA/Data/population_estimates/ons_small_area_population_estimates/lsoa/machine_readable/by_indiviudal_year"
mye_pop_path <- "input_data/mye/2019/population_gla.rds"

#lookups
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>% select(-ward_name)
lsoa_to_ward <- readRDS("input_data/lookup/2011_lsoa_to_ward.rds")

london_wards <- filter(ward_to_district, str_detect(gss_code, "E09"))
london_wards <- london_wards$gss_code_ward

msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds") %>% select(-msoa_name)
lsoa_to_msoa <- readRDS("input_data/lookup/lsoa_to_msoa.rds")

london_msoas <- filter(msoa_to_district, str_detect(gss_code, "E09"))
london_msoas <- london_msoas$gss_code_msoa

#process data for different years/sexes into single dataframe
lsoa_data_raw <- list()
for(yr in 2010:most_recent_data_year) {
  f <- readRDS(paste0(lsoa_dir,"/sape_lsoa_mid_",yr,"_f.rds")) %>%
    mutate(year = as.numeric(substr(mye_year,5,8)),
           sex = "female") %>%
    select(gss_code_lsoa = LSOA11CD, year, sex, age, popn = value)
  m <- readRDS(paste0(lsoa_dir,"/sape_lsoa_mid_",yr,"_m.rds"))%>%
    mutate(year = as.numeric(substr(mye_year,5,8)),
           sex = "male") %>%
    select(gss_code_lsoa = LSOA11CD, year, sex, age, popn = value)
  
  lsoa_data_raw[[yr]] <- rbind(m,f)
}
lsoa_data <- data.table::rbindlist(lsoa_data_raw)

####Ward data####
ward_data <- dtplyr::lazy_dt(lsoa_data) %>%
  left_join(lsoa_to_ward, by="gss_code_lsoa") %>%
  filter(gss_code_ward %in% london_wards)   %>%
  left_join(ward_to_district, by="gss_code_ward") %>%
  as.data.frame() %>%
  aggregate_city_wards("popn") %>%
  group_by(year, gss_code_ward, gss_code, sex, age) %>%
  summarise(popn = sum(popn), .groups = 'drop_last') %>%
  as.data.frame() %>%
  mutate(age = as.numeric(substr(age,1,2))) %>%
  validate_population(col_aggregation = c("year","gss_code_ward", "sex","age"),
                      col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

#Faraday ward in Southwark
#Due to large amounts of demolitions the pop ests don't track the total units well
#Population for this ward is calculated by applying the 2011 AHS for the ward
#to the LDD dev data for the ward
faraday_ldd <- readRDS("input_data/small_area_model/ldd_backseries_dwellings_ward.rds") %>%
  filter(gss_code_ward == "E05000541") %>%
  mutate(total_units = cumsum(units)) %>%
  select(year, total_units)

faraday_total <- filter(ward_data, gss_code_ward == "E05000541") %>%
  group_by(year) %>%
  summarise(total_pop = sum(popn), .groups = 'drop_last') %>%
  as.data.frame()

faraday_2011_ahs <- (filter(faraday_total, year == 2011)$total_pop)/(filter(faraday_ldd, year == 2011)$total_units)

faraday_total <- mutate(faraday_ldd, total_popn = faraday_2011_ahs*total_units) %>%
  select(year, total_popn)

faraday_scaled <- filter(ward_data, gss_code_ward == "E05000541", year > 2010) %>%
  popmodules::constrain_component(faraday_total,
                                 col_aggregation = "year",
                                 col_popn = "popn",
                                 col_constraint = "total_popn")

ward_data <- filter(ward_data, gss_code_ward != "E05000541") %>%
  rbind(faraday_scaled) %>%
  rbind(filter(ward_data, gss_code_ward == "E05000541" & year == 2010))

rm(faraday_ldd, faraday_2011_ahs, faraday_scaled)

if(length(unique(ward_data$gss_code_ward))!=625){stop("Wrong number of wards")}

#Scale to borough totals
borough_scaling_factors_ward <- dtplyr::lazy_dt(ward_data)  %>%
  group_by(gss_code, year, sex, age) %>%
  summarise(aggregated_popn = sum(popn)) %>%
  as.data.frame() %>%
  left_join(readRDS(mye_pop_path), by=c("year", "gss_code","sex","age")) %>%
  rename(mye_popn = popn) %>%
  mutate(scaling_factor = ifelse(aggregated_popn == 0, 0, mye_popn / aggregated_popn)) %>%
  select(gss_code, year, sex, age, scaling_factor) %>%
  validate_population(col_data = "scaling_factor",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

scaled_ward_data <- ward_data  %>%
  left_join(borough_scaling_factors_ward, by=c("gss_code", "year", "sex", "age")) %>%
  mutate(popn = popn*scaling_factor) %>%
  select(year, gss_code, gss_code_ward, sex, age, popn) %>%
  arrange(year, gss_code, gss_code_ward, sex, age) %>%
  as.data.frame() %>%
  validate_population(col_aggregation = c("gss_code_ward", "year", "sex", "age"),
                      col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

####Check on Faraday
faraday_check <- filter(scaled_ward_data, gss_code_ward == "E05000541") %>%
  group_by(year) %>%
  summarise(scaled_pop = sum(popn), .groups = 'drop_last') %>%
  as.data.frame() %>%
  left_join(faraday_total, by="year") %>%
  rename(unscaled_pop = total_popn)
print(faraday_check)

####MSOA data####
msoa_data <- dtplyr::lazy_dt(lsoa_data) %>%
  left_join(lsoa_to_msoa, by="gss_code_lsoa") %>%
  filter(gss_code_msoa %in% london_msoas)   %>%
  left_join(msoa_to_district, by="gss_code_msoa") %>%
  as.data.frame() %>%
  group_by(year, gss_code_msoa, gss_code, sex, age) %>%
  summarise(popn = sum(popn), .groups = 'drop_last') %>%
  as.data.frame() %>%
  mutate(age = as.numeric(substr(age,1,2))) %>%
  validate_population(col_aggregation = c("year","gss_code_msoa", "sex","age"),
                      col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

if(length(unique(msoa_data$gss_code_msoa))!=983){stop("Wrong number of msoas")}

borough_scaling_factors_msoa <- dtplyr::lazy_dt(msoa_data)  %>%
  group_by(gss_code, year, sex, age) %>%
  summarise(aggregated_popn = sum(popn)) %>%
  as.data.frame() %>%
  left_join(readRDS(mye_pop_path), by=c("year", "gss_code","sex","age")) %>%
  rename(mye_popn = popn) %>%
  mutate(scaling_factor = ifelse(aggregated_popn == 0, 0, mye_popn / aggregated_popn)) %>%
  select(gss_code, year, sex, age, scaling_factor) %>%
  validate_population(col_data = "scaling_factor",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

scaled_msoa_data <- msoa_data  %>%
  left_join(borough_scaling_factors_msoa, by=c("gss_code", "year", "sex", "age")) %>%
  mutate(popn = popn*scaling_factor) %>%
  select(year, gss_code, gss_code_msoa, sex, age, popn) %>%
  arrange(year, gss_code, gss_code_msoa, sex, age) %>%
  as.data.frame() %>%
  validate_population(col_aggregation = c("gss_code_msoa", "year", "sex", "age"),
                      col_data = "popn",
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(scaled_ward_data, paste0("input_data/small_area_model/ward_population_estimates.rds"))
saveRDS(scaled_msoa_data, paste0("input_data/small_area_model/msoa_population_estimates.rds"))

rm(list = ls())
