library(dplyr)
library(data.table)

lookup <- readRDS("C:/Projects_c/population_projections/input_data/lookup/2011_ward_to_district.rds") %>% 
  left_join(readRDS("input_data/lookup/gss_code_to_name_(2021_geog).rds")) %>% 
  rename(la_name = gss_name) %>% 
  filter(substr(gss_code, 1, 3)=="E09")

ldd_2013 <- readRDS("input_data/small_area_model/development_data/ldd_backseries_dwellings_ward.rds") %>% 
  filter(year %in% 2012:2019) %>% 
  left_join(lookup) %>% 
  select(year, gss_code_ward, units, ward_name)

template <- expand.grid(year = 2020:2041, gss_code_ward = unique(lookup$gss_code_ward), units = NA) %>%
  left_join(select(lookup, -gss_code, -la_name), by = "gss_code_ward") %>% 
  rbind(ldd_2013) %>% 
  arrange(year, gss_code_ward) %>% 
  mutate(units = round(units,1)) %>% 
  data.frame()

ldd_template <- template %>%
  tidyr::pivot_wider(names_from = year, values_from = units) %>%
  as.data.frame()

empty_template <- template %>% 
  mutate(units = NA) %>%
  tidyr::pivot_wider(names_from = year, values_from = units) %>%
  as.data.frame()
  


empty <- ldd_template[1,]
for(i in 1:ncol(ldd_template)){
  empty[1,i] <- NA
}

col_names <- names(ldd_template)
header <- empty
for(i in 1:ncol(empty)){
  header[1,i] <- ifelse(is.numeric(header[,i]), as.numeric(col_names[i]), col_names[i])
}

self <- empty
self[1,1] <- "Self-contained"

non_self <- empty
non_self[1,1] <- "Non-self-contained"

notes <- rbind(empty, empty, empty, empty, empty)
notes[1,1] <- "Notes"
notes[2,1] <- "By default LDD data will be used for years 2012-2019"
notes[3,1] <- "Empty cells will be filled with SHLAA data. To indicate no development input a 0"
notes[4,1] <- "Non-self-contained units should be completed with the following ratios applied - 2.5:1 for student housing, 1:1 for housing for older people (C2), and 1.8:1 for all other non self-contained housing"
notes[5,1] <- "See the 2020-based BPO Developmenty Template Guide for more information"

#i <- "Harrow"

for(i in unique(lookup$la_name)){
  
  borough <- filter(lookup, la_name == i) %>% 
    distinct() %>% 
    select(-gss_code, -la_name)
  
  borough_template_self <- left_join(borough, ldd_template, by=c("gss_code_ward","ward_name"))
  borough_template_non_self <- left_join(borough, empty_template, by=c("gss_code_ward","ward_name"))
  
  output <- rbind(self, empty, header, borough_template_self, empty,
                  non_self, empty, header, borough_template_non_self, empty,
                  notes)
  
  folder <- "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/blank_templates/2013_wards/"
  fwrite(output, paste0(folder, i, " (2013 wards).csv"), col.names = FALSE)
  
}
