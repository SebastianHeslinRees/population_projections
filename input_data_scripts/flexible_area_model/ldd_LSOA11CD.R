library(dplyr)
library(data.table)

message("LSOA LDD development data")

input_data_dir <- "input_data/flexible_area_model/"

#-------------------------------------------------------------------------------

#Data up to this year is considered good quality
last_data_year <- 2019

#-------------------------------------------------------------------------------

#base census dwellings
lsoa_census_dwellings <- fread("Q:/Teams/D&PA/Data/census_tables/housing_led_model/LSOA_DWELLINGS_CENSUS.CSV")

#ldd data
load("N:/LDD/Unit flow analysis/output_data/IMA/2020_06/0_ldd_development_unit_flow.Rda")

#lookup
lsoa_to_district <- readRDS("input_data/lookup/lsoa_to_msoa.rds") %>%
  left_join(readRDS("input_data/lookup/msoa_to_district.rds"), by = c("gss_code_msoa")) %>%
  select(gss_code_lsoa, gss_code)

#-------------------------------------------------------------------------------

ldd_raw <- ldd_development_unit_flow %>%
  group_by(permission_id, borough_ref, lsoa11cd, status_line_flow, ssd,
           date_work_start, date_work_comp, demolition) %>% 
  summarise(unit_line_flow = sum(unit_line_flow),
            .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

#Filter to completions & demolitions
#Add mid-year
comp_date <- ldd_raw %>%
  filter(demolition==FALSE,
         status_line_flow == "comp") %>%
  mutate(date = as.character(date_work_comp)) 

#for demos use start date if possible, if not then use completion date
demos_date <- ldd_raw %>%
  filter(demolition==TRUE,
         status_line_flow %in% c("comp","start")) %>%
  mutate(date = ifelse(is.na(date_work_start),
                       as.character(date_work_comp),
                       as.character(date_work_start)))

ldd_mid_year <- rbind(comp_date, demos_date) %>% 
  mutate(year = as.numeric(substr(date,1,4)),
         month = as.numeric(substr(date,6,7))) %>%
  mutate(mid_year = ifelse(month <= 6, year, year+1)) %>%
  filter(mid_year %in% 2011:last_data_year) %>% 
  #these 2 lines because we only want the period between census and mid-year in 2011
  filter(!(year == 2011 & month %in% 1:3)) %>% 
  filter(year != 2010) %>% 
  select(-month, -year)

ldd_comps <- ldd_mid_year %>%
  filter(demolition==FALSE)

ldd_demos <- ldd_mid_year %>%
  filter(demolition==TRUE)

rm(comp_date, demos_date, ldd_mid_year)

#------------------------------------------------------------------------------- 

#Assign to lsoa
polygon_split <- readRDS("input_data/housing_led_model/lsoa_polygon_splits.rds") %>%
  select(permission_id, lsoa11cd, demolition, area_prop)

#------------------------------------------------------------------------------- 
#Completions

polygon_split_comp <- filter(polygon_split, demolition == FALSE) %>% 
  filter(permission_id %in% ldd_comps$permission_id)

comps_w_polygons <- select(ldd_comps, -lsoa11cd) %>%
  filter(permission_id %in% polygon_split_comp$permission_id) %>%
  right_join(polygon_split_comp, by=c("permission_id", "demolition"))

comps_no_polygons <- ldd_comps %>% 
  filter(!permission_id %in% polygon_split_comp$permission_id,
         demolition == FALSE) %>%
  mutate(area_prop = 1) %>%
  select(names(comps_w_polygons))

#-------------------------------------------------------------------------------
#Demolitions

polygon_split_demo <- filter(polygon_split, demolition == TRUE) %>% 
  filter(permission_id %in% ldd_demos$permission_id)

demos_w_polygons <- select(ldd_demos, -lsoa11cd) %>%
  filter(permission_id %in% polygon_split_demo$permission_id) %>%
  right_join(polygon_split_demo, by=c("permission_id", "demolition"))

demos_no_polygons <- ldd_demos %>% 
  filter(!permission_id %in% polygon_split_demo$permission_id,
         demolition == TRUE) %>%
  mutate(area_prop = 1) %>%
  select(names(demos_w_polygons))

#-------------------------------------------------------------------------------
#Bring everything together

processed <- rbind(comps_w_polygons, comps_no_polygons,
                   demos_w_polygons, demos_no_polygons) %>% 
  rename(gss_code_lsoa = lsoa11cd,
         year = mid_year)

rm(polygon_split, polygon_split_comp, comps_w_polygons, comps_no_polygons,
   polygon_split_demo, demos_w_polygons, demos_no_polygons)

#-------------------------------------------------------------------------------

##This is temporary fix to solve a problem in the underlying data
processed <- filter(processed, permission_id != 347048)
warning("Filtering out permission 347048 while waiting for a better fix")

#-------------------------------------------------------------------------------

#group by mid-year and lsoa
additional_units <- processed %>%
  mutate(unit_line_flow = ifelse(demolition == TRUE,
                                 unit_line_flow*-1,
                                 unit_line_flow)) %>% 
  mutate(lsoa_units = unit_line_flow*area_prop) %>%
  dtplyr::lazy_dt() %>%
  group_by(year, gss_code_lsoa) %>%
  summarise(add_units = sum(lsoa_units), .groups = 'drop_last') %>%
  as.data.frame() %>%
  tidyr::complete(year = 2011:last_data_year,
                  gss_code_lsoa = unique(lsoa_to_district$gss_code_lsoa),
                  fill = list(add_units = 0))

#-------------------------------------------------------------------------------

#Calculate stock
lsoa_units <- lsoa_census_dwellings %>%
  as.data.frame() %>%
  mutate(year = 2011) %>%
  rename(add_units = dwellings) %>%
  select_at(names(additional_units)) %>%
  rbind(additional_units) %>%
  group_by(year, gss_code_lsoa) %>%
  summarise(units = sum(add_units), .groups = 'drop_last') %>%
  as.data.frame()

#-------------------------------------------------------------------------------

#save
dir.create(paste0(input_data_dir,"development_data/"), showWarnings = FALSE)
saveRDS(lsoa_units, paste0(input_data_dir,"development_data/ldd_backseries_dwellings_LSOA11CD.rds"))

rm(list=setdiff(ls(), c("ldd_lsoa","input_data_dir")))

