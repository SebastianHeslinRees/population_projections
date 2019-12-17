#Add the LDD development to the census stock
#Group by ward, msoa and borough for use in models

#lookups
lsoa_to_ward <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to ward.rds")
lsoa_to_msoa <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to msoa and borough.rds") %>%
  select(gss_code_lsoa, gss_code_msoa)
lsoa_to_borough <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to msoa and borough.rds") %>%
  select(gss_code_lsoa, gss_code = gss_code_borough)

#base census dwellings
lsoa_census_dwellings <- data.table::fread("input_data/housing_led_model/LSOA_DWELLINGS_CENSUS.CSV")

#ldd data


#Assign to lsoa


#Assign to mid-year
additional_units <- x %>%
  mid_year = ifelse(month <= 6, year, year+1) %>%
  filter(date >= "2011-04-01") %>%
  filter(mid_year %in% 2011:2019) %>%
  dtplyr::lazy_dt() %>%
  group_by(year = mid_year, gss_code_lsoa, mid_year) %>%
  summarise(add_units = sum(units)) %>%
  as.data.frame()

#Calculate stock
cumulative_units <- additional_units %>%
  arrange(gss_code, year) %>%
  group_by(gss_code) %>%
  mutate(cum_units = cumsum(add_units)) %>%
  ungroup() %>%
  left_join(lsoa_census_dwellings, by = "gss_code_lsoa") %>%
  mutate(units = cum_units + dwellings)
  
#group it into differnet geographies
ward_units <- left_join(lsoa_units, lsoa_to_ward, by="gss_code_lsoa") %>%
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
saveRDS(borough_units, "input_data/housing_led_model/ldd_backseries_dwellings_borough.rds")
saveRDS(ward_units, "input_data/housing_led_model/ldd_backseries_dwellings_ward.rds")
saveRDS(msoa_units, "input_data/housing_led_model/ldd_backseries_dwellings_msoa.rds")

