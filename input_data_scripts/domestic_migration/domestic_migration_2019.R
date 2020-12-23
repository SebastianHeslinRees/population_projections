library(dplyr)
library(data.table)
library(popmodules)

data_location <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/"
output_location <- "input_data/domestic_migration/2019/"
dir.create(output_location, showWarnings = FALSE, recursive = TRUE)

#--------------------------

file_1 <- fread(paste0(data_location, "raw_data/y2019/Detailed_Estimates_2019_LA_2019_Dataset_1.csv"))
file_2 <- fread(paste0(data_location, "raw_data/y2019/Detailed_Estimates_2019_LA_2019_Dataset_2.csv"))

past_data <- readRDS("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds")

#----------------------------

#combine and clean 2019 data
dom_flows <- rbind(file_1, file_2) %>%
  dtplyr::lazy_dt() %>% 
  rename(gss_in = InLA,
         gss_out = OutLA) %>% 
  mutate(sex = case_when(Sex == "M" ~ "male",
                         Sex == "F" ~ "female",
                         TRUE ~ "NA"),
         age = ifelse(Age > 90, 90, Age),
         year = 2019) %>% 
  group_by(gss_out, gss_in, year, sex, age) %>%
  summarise(value = sum(Moves)) %>% 
  data.frame()


#checks
# unique(dom_flows$sex)
# range(dom_flows$age)
# unique(substr(dom_flows$gss_in,1,3))
# unique(substr(dom_flows$gss_out,1,3))
# str(dom_flows)
# sum(is.na(dom_flows))

#----------------------------

#set past data & new to same geography
#this will take a while
past_data <- past_data %>% 
  popmodules::recode_gss_codes(col_geog = "gss_in", data_cols = "value",
                               recode_to_year = 2020) %>%
  popmodules::recode_gss_codes(col_geog = "gss_out", data_cols = "value",
                               recode_to_year = 2020)  

dom_flows <- dom_flows %>% 
  popmodules::recode_gss_codes(col_geog = "gss_in", data_cols = "value",
                               recode_to_year = 2020) %>%
  popmodules::recode_gss_codes(col_geog = "gss_out", data_cols = "value",
                               recode_to_year = 2020) 

popmodules::validate_same_geog(past_data, dom_flows, "gss_in", "gss_in")
popmodules::validate_same_geog(past_data, dom_flows, "gss_out", "gss_out")


#------------------------------------

#combine and save
dom_flows_output <- rbind(past_data, dom_flows)
rm(list=setdiff(ls(), c("dom_flows_output","output_location")))
saveRDS(dom_flows_output, paste0(output_location, "domestic_migration_flows_ons_(2020_geog).rds"))

#------------------------------------

#gross at net flows files
dom_in <- sum_domestic_flows(dom_flows_output, "in", "value")

dom_out <- sum_domestic_flows(dom_flows_output, "out", "value")

dom_net <- dom_out %>%
  mutate(dom_in = dom_out *-1) %>% 
  select(-dom_out) %>% 
  rbind(dom_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(dom_net = sum(dom_in)) %>% 
  data.frame()

#--------------------------------------

#regional flows
district_to_region <- readRDS("input_data/lookup/district_to_region.rds")
aggregated_flows <- aggregate_regional_flows(dom_flows_output, district_to_region, "value")

reg_dom_in<- sum_domestic_flows(aggregated_flows[[1]], "in", "value") %>% 
  filter(substr(gss_code,1,1)=="E")

reg_dom_out<- sum_domestic_flows(aggregated_flows[[1]], "out", "value") %>% 
  filter(substr(gss_code,1,1)=="E")

reg_dom_net <- reg_dom_out %>%
  mutate(dom_in = dom_out *-1) %>% 
  select(-dom_out) %>% 
  rbind(reg_dom_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(dom_net = sum(dom_in)) %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1)=="E")

#---------------------------------------------------
#sub regional flows
sub_reg_dom_out <- sum_domestic_flows(aggregated_flows[[3]], "out", "value") %>% 
  filter(gss_code %in% c("E13000001","E13000002"))

sub_reg_dom_in <- sum_domestic_flows(aggregated_flows[[3]], "in", "value") %>% 
  filter(gss_code %in% c("E13000001","E13000002"))

sub_regional_net <- sub_reg_dom_out %>%
  mutate(dom_in = dom_out *-1) %>% 
  select(-dom_out) %>% 
  rbind(sub_reg_dom_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(dom_net = sum(dom_in)) %>% 
  data.frame()

#---------------------------------------------------

#national flows
national_flows <- aggregated_flows[[2]]

#Scotland and Northern Ireland are in the district-level data
#Only England & Wales are missing
nat_dom_in <- sum_domestic_flows(aggregated_flows[[2]], "in", "value")  %>%
  filter(gss_code %in% c("E92000001","W92000004"))

nat_dom_out <- sum_domestic_flows(aggregated_flows[[2]], "out", "value") %>%
  filter(gss_code %in% c("E92000001","W92000004"))

nat_dom_net <- nat_dom_out %>%
  mutate(dom_in = dom_out *-1) %>% 
  select(-dom_out) %>% 
  rbind(nat_dom_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(dom_net = sum(dom_in)) %>% 
  data.frame() %>%
  filter(gss_code %in% c("E92000001","W92000004"))

#---------------------------------------------------

dom_in_all <- rbind(dom_in, reg_dom_in, nat_dom_in, sub_reg_dom_in)
dom_out_all <- rbind(dom_out, reg_dom_out, nat_dom_out, sub_reg_dom_out)
dom_net_all <- rbind(dom_net, reg_dom_net, nat_dom_net, sub_regional_net)

#validate
validate_population(dom_in_all, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(dom_out_all, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(dom_net_all, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)


#Save

saveRDS(dom_in_all, paste0(output_location, "domestic_migration_in_(2020_geog).rds"))
saveRDS(dom_out_all, paste0(output_location, "domestic_migration_out_(2020_geog).rds"))
saveRDS(dom_net_all, paste0(output_location, "domestic_migration_net_(2020_geog).rds"))

saveRDS(national_flows, paste0(output_location, "national_domestic_migration_flows.rds"))
saveRDS(aggregated_flows[[1]], paste0(output_location, "regional_domestic_migration_flows.rds"))

rm(list=ls())
