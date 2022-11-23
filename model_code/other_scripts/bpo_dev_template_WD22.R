library(dplyr)
library(data.table)

ward_2022_name_lookup <- readRDS("C:/Projects_c/population_projections/input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds") 

ldd_2022 <- readRDS("input_data/flexible_area_model/development_data/ldd_backseries_dwellings_ward_WD22CD.rds") %>% 
  left_join(select(ward_2022_name_lookup, gss_code_ward, ward_name), by = "gss_code_ward") %>% 
  #filter(substr(gss_code, 1, 3)=="E09")%>% 
  group_by(gss_code_ward, ward_name, year) %>% 
  summarise(units = sum(units), .groups = 'drop_last') %>% 
  filter(year %in% 2012:2019)

template <- expand.grid(year = 2020:2041, gss_code_ward = unique(ward_2022_name_lookup$gss_code_ward), units = NA) %>%
  left_join(select(ward_2022_name_lookup, gss_code_ward, ward_name), by = "gss_code_ward") %>% 
  rbind(ldd_2022) %>% 
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

folder <- "Q:/Teams/D&PA/Demography/Projections/bpo_2021_based/blank_templates/2022_wards/"
dir.create(folder, recursive = T, showWarnings = F)

for(i in unique(ward_2022_name_lookup$la_name)){
  
  borough <- filter(ward_2022_name_lookup, la_name == i) %>% 
    distinct() %>% 
    select(-gss_code, -la_name)
  
  borough_template_self <- left_join(borough, ldd_template, by=c("gss_code_ward","ward_name"))
  borough_template_non_self <- left_join(borough, empty_template, by=c("gss_code_ward","ward_name"))
  
  output <- rbind(self, empty, header, borough_template_self, empty,
                  non_self, empty, header, borough_template_non_self, empty,
                  notes)
  
  fwrite(output, paste0(folder, i, " (2022 wards).csv"), col.names = FALSE)
  
}
