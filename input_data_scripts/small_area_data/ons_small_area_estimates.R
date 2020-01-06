library(dplyr)
library(stringr)
library(data.table)

#data paths
lsoa_dir <- "Q:/Teams/D&PA/Data/population_estimates/ons_small_area_population_estimates/lsoa/machine_readable/by_indiviudal_year"
mye_pop_path <- "input_data/mye/2018/population_gla_2019-11-13.rds"

#lookups
ward_to_district <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/ward to district.rds") %>%
  rename(gss_code = gss_code_district)
lsoa_to_ward <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to ward.rds")
london_wards <- c(filter(ward_to_district, str_detect(gss_code, "E09"))$gss_code_ward,"E09000001")

#process data for different years/sexes into signle dataframe
lsoa_data <- list()
for(yr in 2010:2017){
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

#TODO Check this is doing what I think its doing
ward_data <- dtplyr::lazy_dt(lsoa_data) %>%
  left_join(lsoa_to_ward, by="gss_code_lsoa") %>%
  as.data.frame() %>%
  filter(gss_code_ward %in% london_wards)   %>%
  left_join(ward_to_district, by="gss_code_ward") %>%
  .aggregate_city_wards("popn") %>%
  group_by(year, gss_code_ward, gss_code, sex, age) %>%
  summarise(popn = sum(popn)) %>%
  as.data.frame() %>%
  mutate(age = as.numeric(substr(age,1,2))) 

borough_scaling_factors <- dtplyr::lazy_dt(ward_data)  %>%
  group_by(gss_code, year, sex, age) %>%
  summarise(borough_popn = sum(popn)) %>%
  as.data.frame() %>%
  left_join(readRDS(mye_pop_path), by=c("year", "gss_code","sex","age")) %>%
  mutate(scaling_factor = ifelse(popn == 0, 0, borough_popn / popn)) %>%
  select(gss_code, year, sex, age, scaling_factor)

scaled_ward_data <- ward_data  %>%
  left_join(borough_scaling_factors, by=c("gss_code", "year", "sex", "age")) %>%
  mutate(popn = popn*scaling_factor) %>%
  select(year, gss_code_borough=gss_code, gss_code_ward, sex, age, popn)


#Save
dir.create("input_data/small_area_model", showWarnings = F)
saveRDS(scaled_ward_data, "input_data/small_area_model/ward_population_estimates_2010_2017.rds")
