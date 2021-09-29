devtools::load_all("Q:/Teams/D&PA/Demography/demogtools")
library(dplyr)
library(data.table)

options(scipen=999)

components <- c("population","births","deaths","dom_in","dom_out",
                "int_in","int_out")

ward_components <- c("population","births","deaths", "migration")

projections_list <- list('projection 1' = "Identified_Capacity_21-09-22_1121",
                         'projection 2' = "Past_Delivery_21-09-22_1121",
                         'projection 3' = "Housing_Targets_21-09-22_1121") %>% 
  lapply(function(x){paste0("outputs/housing_led/2020/",x)})

borough_data_list <- list()

#-------------------------------------------------------------------------------

#Read in borough data all components

for(i in components){
  print(i)
  borough_data_list[[i]] <- read_multiple_projections(projections_list,
                                                      component = i,
                                                      col_aggregation = c("gss_code","year","sex","age")) %>% 
    mutate(component = i) %>% 
    filter(substr(gss_code,1,3)=="E09")
  
  nm <- names(borough_data_list[[i]])
  nm <- nm[length(nm)-2]
  
  borough_data_list[[i]] <- rename(borough_data_list[[i]], value := all_of(nm)) %>% 
    select(gss_code, year, sex, age, component, variant, value)
}

borough_data <- rbindlist(borough_data_list) %>% data.frame()
rm(borough_data_list, i, components, nm)
gc()

#-------------------------------------------------------------------------------

#Create borough components for Net and Total migration from gross inputs

dom_net <- borough_data %>% 
  filter(component %in% c("dom_in","dom_out")) %>% 
  mutate(value = ifelse(component == "dom_out", value *-1, value),
         component = "dom_net") %>% 
  group_by(gss_code, year, sex , age, component, variant) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  data.frame()

int_net <- borough_data %>% 
  filter(component %in% c("int_in","int_out")) %>% 
  mutate(value = ifelse(component == "int_out", value *-1, value),
         component = "int_net") %>% 
  group_by(gss_code, year, sex , age, component, variant) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  data.frame()

total_in <-  borough_data %>% 
  filter(component %in% c("dom_in","int_in")) %>% 
  mutate(component = "total_in") %>% 
  group_by(gss_code, year, sex , age, component, variant) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  data.frame()

total_out <-  borough_data %>% 
  filter(component %in% c("dom_out","int_out")) %>% 
  mutate(component = "total_out") %>% 
  group_by(gss_code, year, sex , age, component, variant) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  data.frame()

total_net <- rbind(total_in, total_out) %>% 
  mutate(value = ifelse(component == "total_out", value *-1, value),
         component = "total_net") %>% 
  group_by(gss_code, year, sex , age, component, variant) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

#Bind all borough data into one massive dataframe

all_borough_data <- rbind(borough_data, dom_net, int_net, total_in, total_out, total_net)

rm(borough_data, dom_net, int_net, total_in, total_out, total_net)
gc()

#-------------------------------------------------------------------------------

#Create London data from borough data

final_london <- all_borough_data %>% 
  mutate(gss_code = "E12000007") %>% 
  group_by(gss_code, year, sex , age, component, variant) %>% 
  summarise(value = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(!component %in% c("dom_in", "dom_out", "total_in", "total_out")) %>% 
  mutate(gss_code_ward = NA) %>% 
  rename(projection = variant) %>% 
  select(projection, gss_code, gss_code_ward, year, sex, age, component, value) 

#-------------------------------------------------------------------------------

#Read in ward data all components

ward_data_list <- list()
projections_list <- lapply(projections_list, function(x) paste0(x, "/ward"))

for(i in ward_components){
  
  print(i)
  j <- paste0(i, "_ward")
  
  ward_data_list[[i]] <- read_multiple_projections(projections_list,
                                                   component = j,
                                                   col_aggregation = c("gss_code","gss_code_ward","year","sex","age")) %>% 
    mutate(component = i)
  
  nm <- names(ward_data_list[[i]])
  nm <- nm[length(nm)-2]
  
  ward_data_list[[i]] <- rename(ward_data_list[[i]], value := all_of(nm)) %>% 
    select(gss_code_ward, year, sex, age, component, variant, value)
}

ward_data <- rbindlist(ward_data_list) %>% data.frame() %>% 
  mutate(component = ifelse(component == "migration", "total_net", component))

rm(ward_data_list, i, j, ward_components, nm)

#-------------------------------------------------------------------------------

#Add borough and ward names to data and tidy into final format

borough_names <- readRDS("input_data/lookup/gss_code_to_name_(2021_geog).rds")
ward_district_lookup <- readRDS("input_data/lookup/2011_ward_to_district.rds")

final_borough <- all_borough_data %>% 
  rename(projection = variant) %>% 
  mutate(gss_code_ward = NA) %>%
  select(projection, gss_code, gss_code_ward, year, sex, age, component, value)

final_ward <- left_join(ward_data, ward_district_lookup, by="gss_code_ward") %>% 
  filter(gss_code_ward != "E09000001") %>% 
  left_join(borough_names, by="gss_code") %>% 
  rename(projection = variant)  %>%
  select(projection, gss_code, gss_code_ward, year, sex, age, component, value)

rm(all_borough_data, ward_data, borough_names, ward_district_lookup)
gc()

#-------------------------------------------------------------------------------

#bind everything together and do final filter

final_ALL <- rbind(final_london, final_borough, final_ward) %>% 
  data.frame() %>% 
  mutate(value = round(value,0))

final_ALL <- final_ALL %>% 
  filter(year %in% 2011:2041)

final_ALL <- final_ALL %>% 
  mutate(value = ifelse(is.na(value),"NULL",value))

#filter into separate projections dataframes

proj_1 <- filter(final_ALL, projection == 'projection 1')
proj_2 <- filter(final_ALL, projection == 'projection 2')
proj_3 <- filter(final_ALL, projection == 'projection 3')

#write out

fwrite(proj_1, "notebooks_and_analysis/2020_based_projections_tool_data_1.csv")
fwrite(proj_2, "notebooks_and_analysis/2020_based_projections_tool_data_2.csv")
fwrite(proj_3, "notebooks_and_analysis/2020_based_projections_tool_data_3.csv")

q_drive <- "Q:/Teams/D&PA/Demography/Projections/population_models/temp/"

fwrite(proj_1, paste0(q_drive, "2020_based_projections_tool_data_1.csv"))
fwrite(proj_2, paste0(q_drive, "2020_based_projections_tool_data_2.csv"))
fwrite(proj_3, paste0(q_drive, "2020_based_projections_tool_data_3.csv"))

#-------------------------------------------------------------------------------
message("tests")
library(testthat)

test_that("proj 1", {
  expect_equal(length(unique(proj_1$gss_code)),34)
  expect_equal(length(unique(proj_1$gss_code_ward)),625)
  expect_equal(range(proj_1$year),c(2011,2041))
  expect_equal(unique(proj_1$sex),c("female","male"))
  expect_equal(range(proj_1$age),c(0,90))
  expect_equal(length(unique(proj_1$age)),91)
  expect_equal(length(unique(proj_1$component)),12)
})

test_that("proj 2", {
  expect_equal(length(unique(proj_2$gss_code)),34)
  expect_equal(length(unique(proj_2$gss_code_ward)),625)
  expect_equal(range(proj_2$year),c(2011,2041))
  expect_equal(unique(proj_2$sex),c("female","male"))
  expect_equal(range(proj_2$age),c(0,90))
  expect_equal(length(unique(proj_2$age)),91)
  expect_equal(length(unique(proj_2$component)),12)
})

test_that("proj 3", {
  expect_equal(length(unique(proj_3$gss_code)),34)
  expect_equal(length(unique(proj_3$gss_code_ward)),625)
  expect_equal(range(proj_3$year),c(2011,2041))
  expect_equal(unique(proj_3$sex),c("female","male"))
  expect_equal(range(proj_3$age),c(0,90))
  expect_equal(length(unique(proj_3$age)),91)
  expect_equal(length(unique(proj_3$component)),12)
})
