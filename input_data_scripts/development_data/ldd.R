#Add the LDD development to the census stock
#Group by ward, msoa and borough for use in models
library(dplyr)

message("LDD development data")

#Data up to what year is considered good quality
last_data_year <- 2019

#lookups
lsoa_to_ward <- readRDS("input_data/lookup/2011_lsoa_to_ward.rds")
lsoa_to_msoa <- readRDS("input_data/lookup/lsoa_to_msoa.rds") %>%
  select(gss_code_lsoa, gss_code_msoa)
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds")
msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds")
lsoa_to_borough <- left_join(lsoa_to_msoa, msoa_to_district, by = c("gss_code_msoa")) %>%
  select(gss_code_lsoa, gss_code)

#base census dwellings
lsoa_census_dwellings <- data.table::fread("Q:/Teams/D&PA/Data/census_tables/housing_led_model/LSOA_DWELLINGS_CENSUS.CSV")

#ldd data
load("N:/LDD/Unit flow analysis/output_data/IMA/2020_06/0_ldd_development_unit_flow.Rda")

ldd_raw <- ldd_development_unit_flow %>%
  select(permission_id, lsoa11cd, status_line_flow, ssd, date_work_start, date_work_comp,
         no_of_bedrooms, demolition, unit_line_flow)

#Assign to lsoa
polygon_split <- readRDS("input_data/housing_led_model/lsoa_polygon_splits.rds") %>%
  select(permission_id, lsoa11cd, demolition, area_prop)

permissions_w_polygons <- select(ldd_raw, -lsoa11cd) %>%
  filter(permission_id %in% polygon_split$permission_id) %>%
  right_join(polygon_split, by=c("permission_id", "demolition"))

permissions_no_polygons <- filter(ldd_raw, !permission_id %in% polygon_split$permission_id) %>%
  mutate(area_prop = 1) %>%
  select(names(permissions_w_polygons))

x <- rbind(permissions_w_polygons, permissions_no_polygons)

#completions - date completed
comps <- x %>%
  filter(demolition==FALSE,
         status_line_flow == "comp") %>% 
  rename(gss_code_lsoa = lsoa11cd) %>%
  mutate(date = as.character(date_work_comp))

#demolitions - date started
#use start where poss if not comp
#don't filter by comp
demos <- x %>%
  filter(demolition==TRUE,
         status_line_flow %in% c("comp","start")) %>%
  mutate(date = ifelse(!is.na(date_work_start),
                       as.character(date_work_start),
                       as.character(date_work_comp)),
         unit_line_flow = unit_line_flow*-1) %>%
  rename(gss_code_lsoa = lsoa11cd) %>%
  select(names(comps)) %>%
  ##This is temporary fix to solve a problem in the underlying data
  ##Libby is looking into a more permanent and robust fix
  filter(permission_id != 347048)
  warning("Filtering out permission 347048 while waiting for a better fix")

#Assign to mid-year
ldd_by_mid_year <- rbind(demos, comps) %>%
  mutate(year = as.numeric(substr(date,1,4)),
         month = as.numeric(substr(date,6,7))) %>%
  mutate(mid_year = ifelse(month <= 6, year, year+1)) %>%
  filter(date >= "2011-04-01") %>%
  filter(mid_year %in% 2011:last_data_year)

#group by mid-year and lsoa
additional_units <- ldd_by_mid_year %>%
  mutate(lsoa_units = unit_line_flow*area_prop) %>%
  dtplyr::lazy_dt() %>%
  group_by(year = mid_year, gss_code_lsoa) %>%
  summarise(add_units = sum(lsoa_units)) %>%
  as.data.frame() %>%
  tidyr::complete(year = 2011:last_data_year, gss_code_lsoa = unique(lsoa_to_borough$gss_code_lsoa), fill = list(add_units = 0))

#Calculate stock
lsoa_units <- lsoa_census_dwellings %>%
  as.data.frame() %>%
  mutate(year = 2011) %>%
  rename(add_units = dwellings) %>%
  select_at(names(additional_units)) %>%
  rbind(additional_units) %>%
  group_by(year, gss_code_lsoa) %>%
  summarise(units = sum(add_units)) %>%
  as.data.frame()

#group it into different geographies
ward_units <- left_join(lsoa_units, lsoa_to_ward, by="gss_code_lsoa") %>%
  left_join(ward_to_district, by = "gss_code_ward") %>%
  mutate(gss_code_ward = ifelse(gss_code == "E09000001", "E09000001", gss_code_ward)) %>%
  group_by(year, gss_code_ward) %>%
  summarise(units = sum(units)) %>%
  ungroup() %>%
  as.data.frame()

msoa_units <- left_join(lsoa_units, lsoa_to_msoa, by="gss_code_lsoa") %>%
  group_by(year, gss_code_msoa) %>%
  summarise(units = sum(units)) %>%
  ungroup() %>%
  as.data.frame()

borough_units <- left_join(lsoa_units, lsoa_to_borough, by="gss_code_lsoa") %>%
  group_by(year, gss_code) %>%
  summarise(units = sum(units)) %>%
  ungroup() %>%
  as.data.frame()

#save it all
dir.create("input_data/small_area_model", showWarnings = FALSE)
saveRDS(borough_units, "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds")
saveRDS(ward_units, "input_data/small_area_model/ldd_backseries_dwellings_ward.rds")
saveRDS(msoa_units, "input_data/small_area_model/ldd_backseries_dwellings_msoa.rds")
