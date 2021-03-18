library(dplyr)
library(tidyr)
library(assertthat)
library(data.table)
library(popmodules)

rm(list=ls())
message("domestic migration 2018")

output_location <- "input_data/domestic_migration/2018/"
dir.create(output_location, recursive = TRUE, showWarnings = FALSE)

region_lookup <- readRDS("input_data/lookup/district_to_region.rds")

domestic_file <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/2018/2002-2018 but codes not changed.rds"

domestic <- readRDS(domestic_file) %>%
  mutate(sex = as.character(sex)) %>%
  rename(gss_in = in_la,
         gss_out = out_la)

domestic <- domestic %>%
  recode_gss_codes(col_geog="gss_in", data_col="value", fun=list(sum), recode_gla_codes = TRUE) %>%
  recode_gss_codes(col_geog="gss_out", data_col="value", fun=list(sum), recode_gla_codes = TRUE)

domestic <-  domestic %>%
  dtplyr::lazy_dt() %>% 
  mutate(year = as.integer(year)) %>%
  mutate(age = ifelse(age < 90, age, 90)) %>%   
  group_by(gss_out, gss_in, year, sex, age) %>%
  summarise(value = sum(value)) %>%
  data.frame() 

saveRDS(domestic, file = paste0(output_location, "domestic_migration_flows_ons.rds"))

#------------------------------------

#gross at net flows files
dom_in <- sum_domestic_flows(domestic, "in", "value")

dom_out <- sum_domestic_flows(domestic, "out", "value")

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
aggregated_flows <- aggregate_regional_flows(domestic, district_to_region, "value")

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

saveRDS(dom_in_all, paste0(output_location, "domestic_migration_in.rds"))
saveRDS(dom_out_all, paste0(output_location, "domestic_migration_out.rds"))
saveRDS(dom_net_all, paste0(output_location, "domestic_migration_net.rds"))

saveRDS(aggregated_flows[[3]], paste0(output_location, "national_domestic_migration_flows.rds"))
saveRDS(aggregated_flows[[1]], paste0(output_location, "regional_domestic_migration_flows.rds"))

rm(list=ls())
