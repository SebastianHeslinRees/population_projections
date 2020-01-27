#Add the LDD development to the census stock
#Group by ward, msoa and borough for use in models
library(dplyr)

#lookups
lsoa_to_ward <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to ward.rds")
lsoa_to_msoa <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to msoa and borough.rds") %>%
  select(gss_code_lsoa, gss_code_msoa)
lsoa_to_borough <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to msoa and borough.rds") %>%
  select(gss_code_lsoa, gss_code = gss_code_borough)
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds")

#base census dwellings
lsoa_census_dwellings <- data.table::fread("Q:/Teams/D&PA/Data/census_tables/housing_led_model/LSOA_DWELLINGS_CENSUS.CSV")

#ldd data
load("//glashare/public/LDD/Unit flow analysis/output_data/IMA/2019_12/0_ldd_development_unit_flow.Rda")

ldd_raw <- ldd_development_unit_flow %>%
  select(permission_id, lsoa11cd, status_line_flow, ssd, date_work_start, date_work_comp,
         no_of_bedrooms, demolition, unit_line_flow)

#Assign to lsoa
polygon_split <- data.table::fread("input_data/housing_led_model/lsoa_polygon_splits_16-01-20.csv") %>%
  as.data.frame() %>%
  mutate(permission_id = as.character(permission_id)) %>%
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
#dont filter by comp
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
  filter(mid_year %in% 2011:2019)

#group by mid-year and lsoa
additional_units <- ldd_by_mid_year %>%
  mutate(lsoa_units = unit_line_flow*area_prop) %>%
  dtplyr::lazy_dt() %>%
  group_by(year = mid_year, gss_code_lsoa) %>%
  summarise(add_units = sum(lsoa_units)) %>%
  as.data.frame() %>%
  tidyr::complete(year = 2011:2019, gss_code_lsoa = unique(lsoa_to_borough$gss_code_lsoa), fill = list(add_units = 0))

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

# cumulative_units <- additional_units %>%
#   arrange(gss_code_lsoa, year) %>%
#   group_by(gss_code_lsoa) %>%
#   mutate(cum_units = cumsum(add_units)) %>%
#   ungroup() %>%
#   left_join(lsoa_census_dwellings, by = "gss_code_lsoa") %>%
#   mutate(units = cum_units + dwellings)
# 
# lsoa_units <- select(cumulative_units, year, gss_code_lsoa, units)

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


####
#Tests/Checks/Comparissons
#
# load("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/housing data/2016 SHLAA.Rdata")
# 
# b <- left_join(lsoa_census_dwellings, lsoa_to_borough, by="gss_code_lsoa") %>%
#   dtplyr::lazy_dt() %>%
#   group_by(gss_code) %>%
#   summarise(dwellings = sum(dwellings)) %>%
#   as.data.frame()
# 
# borough_dev <- borough_dev %>%
#   arrange(gss_code, year) %>%
#   group_by(gss_code) %>%
#   mutate(cum_units = cumsum(new_homes)) %>%
#   ungroup() %>%
#   left_join(b, by = "gss_code") %>%
#   mutate(new_homes = cum_units + dwellings)
# 
# x <- left_join(borough_units, borough_dev, by = c("year", "gss_code")) %>%
#   filter(year == 2016) %>%
#   left_join(ocp) %>%
#   select(gss_code, units, new_homes, del_points)
#   mutate(diff = units - new_homes)
# 
# os_code_point <- data.table::fread("Q:/Teams/D&PA/Data/housing_development/OSCodePoint/Postcodes_2007_to_2018_LSOA.csv")
# 
# ocp <- os_code_point %>%
#   filter(date %in% c("2011_Aug","2012_Aug","2013_Oct","2014_Oct","2015_Nov","2016_Feb","2017_Jan")) %>%
#   mutate(year = as.numeric(substr(date,1,4))) %>%    
#   dtplyr::lazy_dt() %>%
#   group_by(year, LAD11CD) %>%
#   summarise(del_points = sum(DOMESTIC)) %>%
#   as.data.frame() %>%
#   rename(gss_code = LAD11CD)
# 
# g <- left_join(borough_units, borough_dev, by = c("year", "gss_code")) %>%
#   left_join(ocp, by = c("year", "gss_code")) %>%
#   select(-cum_units, -dwellings) %>%
#   rename(new_method = units, old_method = new_homes, code_point = del_points) %>%
#   tidyr::pivot_longer(cols = 3:5, names_to = "source", values_to = "units")
# 
# 
# gss <-  c(paste0("E0900000",1:9),paste0("E090000",10:33))
# i <- i+1
# filter(g, gss_code == gss[[i]]) %>%
#   ggplot2::ggplot(aes(year, units, colour = source)) +
#   geom_line(size=1.02) +
#   ggtitle(gss[[i]])

#save it all
saveRDS(borough_units, "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds")
saveRDS(ward_units, "input_data/small_area_model/ldd_backseries_dwellings_ward.rds")
saveRDS(msoa_units, "input_data/small_area_model/ldd_backseries_dwellings_msoa.rds")
