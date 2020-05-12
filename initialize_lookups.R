#Lookups
ward_district_lookup <- data.table::fread("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/2011_ward_to_district.csv") %>%
  rbind(data.frame(gss_code_ward = "E09000001", ward_name = "City of London", gss_code = "E09000001", stringsAsFactors = FALSE)) %>%
  as.data.frame() %>%
  mutate(ward_name = gsub(",", "", .$ward_name))
saveRDS(ward_district_lookup, "input_data/lookup/2011_ward_to_district.rds")

lsoa_to_ward_lookup <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to ward.rds") %>%
  as.data.frame()
saveRDS(lsoa_to_ward_lookup, "input_data/lookup/2011_lsoa_to_ward.rds")

merged_to_electoral_ward <- data.table::fread("Q:/Teams/D&PA/Census/Lookups/EW/2011 Ward to Merged Ward.csv") %>%
  as.data.frame()
saveRDS(merged_to_electoral_ward, "input_data/lookup/2011_merged_ward_to_electoral_ward.rds")

msoa_to_district <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/msoa to district.rds") %>%
  rename(gss_code = gss_code_district) %>%
  left_join(data.table::fread("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/MSOA_names.csv"), by="gss_code_msoa") %>%
  as.data.frame() %>%
  select(gss_code_msoa, msoa_name, gss_code)
saveRDS(msoa_to_district, "input_data/lookup/msoa_to_district.rds")

lsoa_to_msoa <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/lsoa to msoa and borough.rds") %>%
  select(gss_code_lsoa, gss_code_msoa)
saveRDS(lsoa_to_msoa, "input_data/lookup/lsoa_to_msoa.rds")

rm(ward_district_lookup, lsoa_to_ward_lookup, merged_to_electoral_ward, msoa_to_district, lsoa_to_msoa)