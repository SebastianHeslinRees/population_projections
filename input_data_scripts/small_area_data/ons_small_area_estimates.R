library(dplyr)
library(stringr)
library(data.table)
devtools::load_all('model_code/popmodules')

most_recent_data_year <- 2018
#data paths
lsoa_dir <- "Q:/Teams/D&PA/Data/population_estimates/ons_small_area_population_estimates/lsoa/machine_readable/by_indiviudal_year"
mye_pop_path <- "input_data/mye/2018/population_gla_2019-11-13.rds"

#lookups
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds")%>%
  select(-ward_name)
lsoa_to_ward <- readRDS("input_data/lookup/2011_lsoa_to_ward.rds")

london_wards <- filter(ward_to_district, str_detect(gss_code, "E09"))
london_wards <- london_wards$gss_code_ward

msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds")
lsoa_to_msoa <- readRDS("input_data/lookup/lsoa_to_msoa.rds")

london_msoas <- filter(msoa_to_district, str_detect(gss_code, "E09"))
london_msoas <- london_msoas$gss_code_msoa

#process data for different years/sexes into signle dataframe
lsoa_data <- list()
for(yr in 2010:most_recent_data_year) {
  f <- readRDS(paste0(lsoa_dir,"/sape_lsoa_mid_",yr,"_f.rds")) %>%
    mutate(year = as.numeric(substr(mye_year,5,8)),
           sex = "female") %>%
    select(gss_code_lsoa = LSOA11CD, year, sex, age, popn = value)
  m <- readRDS(paste0(lsoa_dir,"/sape_lsoa_mid_",yr,"_m.rds"))%>%
    mutate(year = as.numeric(substr(mye_year,5,8)),
           sex = "male") %>%
    select(gss_code_lsoa = LSOA11CD, year, sex, age, popn = value)
  
  lsoa_data[[yr]] <- rbind(m,f)
}
lsoa_data <- data.table::rbindlist(lsoa_data)

####Ward data####
ward_data <- dtplyr::lazy_dt(lsoa_data) %>%
  left_join(lsoa_to_ward, by="gss_code_lsoa") %>%
  filter(gss_code_ward %in% london_wards)   %>%
  left_join(ward_to_district, by="gss_code_ward") %>%
  as.data.frame() %>%
  .aggregate_city_wards("popn") %>%
  group_by(year, gss_code_ward, gss_code, sex, age) %>%
  summarise(popn = sum(popn)) %>%
  as.data.frame() %>%
  mutate(age = as.numeric(substr(age,1,2))) %>%
  validate_population(col_aggregation = c("year","gss_code_ward", "sex","age"), col_data = "popn")

if(length(unique(ward_data$gss_code_ward))!=625){message("Warning: Wrong number of wards")}

borough_scaling_factors_ward <- dtplyr::lazy_dt(ward_data)  %>%
  group_by(gss_code, year, sex, age) %>%
  summarise(borough_popn = sum(popn)) %>%
  as.data.frame() %>%
  left_join(readRDS(mye_pop_path), by=c("year", "gss_code","sex","age")) %>%
  mutate(scaling_factor = ifelse(borough_popn == 0, 0, popn / borough_popn)) %>%
  select(gss_code, year, sex, age, scaling_factor) %>%
  validate_population(col_data = "scaling_factor")

scaled_ward_data <- ward_data  %>%
  left_join(borough_scaling_factors_ward, by=c("gss_code", "year", "sex", "age")) %>%
  mutate(popn = popn*scaling_factor) %>%
  select(year, gss_code, gss_code_ward, sex, age, popn) %>%
  arrange(year, gss_code, gss_code_ward, sex, age) %>%
  as.data.frame() %>%
  validate_population(col_aggregation = c("gss_code_ward", "year", "sex", "age"), col_data = "popn")

####MSOA data####
msoa_data <- dtplyr::lazy_dt(lsoa_data) %>%
  left_join(lsoa_to_msoa, by="gss_code_lsoa") %>%
  filter(gss_code_msoa %in% london_msoas)   %>%
  left_join(msoa_to_district, by="gss_code_msoa") %>%
  as.data.frame() %>%
  group_by(year, gss_code_msoa, gss_code, sex, age) %>%
  summarise(popn = sum(popn)) %>%
  as.data.frame() %>%
  mutate(age = as.numeric(substr(age,1,2))) %>%
  validate_population(col_aggregation = c("year","gss_code_msoa", "sex","age"), col_data = "popn")

if(length(unique(msoa_data$gss_code_msoa))!=983){message("Warning: Wrong number of msoas")}

borough_scaling_factors_msoa <- dtplyr::lazy_dt(msoa_data)  %>%
  group_by(gss_code, year, sex, age) %>%
  summarise(borough_popn = sum(popn)) %>%
  as.data.frame() %>%
  left_join(readRDS(mye_pop_path), by=c("year", "gss_code","sex","age")) %>%
  mutate(scaling_factor = ifelse(popn == 0, 0, borough_popn / popn)) %>%
  select(gss_code, year, sex, age, scaling_factor) %>%
  validate_population(col_data = "scaling_factor")

scaled_msoa_data <- msoa_data  %>%
  left_join(borough_scaling_factors_msoa, by=c("gss_code", "year", "sex", "age")) %>%
  mutate(popn = popn*scaling_factor) %>%
  select(year, gss_code, gss_code_msoa, sex, age, popn) %>%
  arrange(year, gss_code, gss_code_msoa, sex, age) %>%
  as.data.frame() %>%
  validate_population(col_aggregation = c("gss_code_msoa", "year", "sex", "age"), col_data = "popn")

#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(scaled_ward_data, "input_data/small_area_model/ward_population_estimates.rds")
saveRDS(scaled_msoa_data, "input_data/small_area_model/msoa_population_estimates.rds")
