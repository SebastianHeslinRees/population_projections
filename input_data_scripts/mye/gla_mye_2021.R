#revised GLA estimates of population & international flows
#data created in a separate process
#this script calls on the processed data
#data created by BC January 2023

library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(popmodules)

message("2021 GLA MYE series")
dir.create("input_data/mye/2021/", showWarnings = FALSE)

gla_series <- readRDS("Q:/Teams/D&PA/Demography/MYE/gla_revised_mye_series.rds")
england_codes <- c("E06", "E07", "E08", "E09")#, "W06", "N92", "S92")

# 2002 - 2011
previous_data <- "input_data/mye/2020/"
past_popn <- readRDS(paste0(previous_data, "population_ons.rds")) %>% filter(year < 2011)
past_births <- readRDS(paste0(previous_data, "births_ons.rds")) %>% filter(year < 2012)
past_deaths <- readRDS(paste0(previous_data, "deaths_ons.rds")) %>% filter(year < 2012)
past_int_in <- readRDS(paste0(previous_data, "int_in_ons.rds")) %>% filter(year < 2012)
past_int_out <- readRDS(paste0(previous_data, "int_out_ons.rds")) %>% filter(year < 2012)
#past_int_net <- readRDS(paste0(previous_data, "int_net_ons.rds")) %>% filter(year < 2012)
past_dom_in <- readRDS(paste0(previous_data, "dom_in_ons.rds")) %>% filter(year < 2012)
past_dom_out <- readRDS(paste0(previous_data, "dom_out_ons.rds")) %>% filter(year < 2012)
#past_dom_net <- readRDS(paste0(previous_data, "dom_net_ons.rds")) %>% filter(year < 2012)

# N Ireland, Scotland, Wales
n_s_w <- function(x){
  x %>% filter(substr(gss_code,1,1) %in% c("N","S","W"))
}

other_popn <- readRDS(paste0(previous_data, "population_ons.rds")) %>% n_s_w()
other_births <- readRDS(paste0(previous_data, "births_ons.rds")) %>% n_s_w()
other_deaths <- readRDS(paste0(previous_data, "deaths_ons.rds")) %>% n_s_w()
other_int_in <- readRDS(paste0(previous_data, "int_in_ons.rds")) %>% n_s_w()
other_int_out <- readRDS(paste0(previous_data, "int_out_ons.rds")) %>% n_s_w()
other_dom_in <- readRDS(paste0(previous_data, "dom_in_ons.rds")) %>% n_s_w()
other_dom_out <- readRDS(paste0(previous_data, "dom_out_ons.rds")) %>% n_s_w()

#-------------------------------------------------------------------------------
# Scotland

scotland_2021_pop <- fread("Q:/Teams/D&PA/Data/population_estimates/nrs_nisra_estimates/scotland/mye_2021/scotalnd_sya_population_2021.csv",
                           header = TRUE) %>% 
  data.frame() %>% 
  pivot_longer(cols = starts_with("X"), names_to = "age", values_to = "popn") %>% 
  mutate(age = str_replace_all(age, "X", ""),
         age = as.numeric(age),
         year = 2021) %>% 
  filter(Sex != "persons",
         Area.code == "S92000003") %>% 
  rename(sex = Sex,
         gss_code = Area.code) %>%
  select(names(past_popn))

scotland_births_total_2021 <- 46734	
scotland_deaths_total_2021 <- 61280
scotland_dom_in_total_2021 <- 56200
scotland_int_in_total_2021 <- 41000
scotland_dom_out_total_2021 <- 47300
scotland_int_out_total_2021 <- 22100

scotland_births_2021 <- data.frame(gss_code = "S92000003",
                                   age = 0,
                                   sex = c("female","male"),
                                   year = 2021,
                                   births = c(scotland_births_total_2021 * (100/205),
                                              scotland_births_total_2021 * (105/205))) %>% 
  complete_popn_dataframe(col_data = "births") %>% 
  select(names(past_births))

scotland_deaths_2021 <- filter(other_deaths, year == 2020, gss_code == "S92000003") %>% 
  mutate(share = deaths/sum(deaths),
         deaths = share * scotland_deaths_total_2021) %>% 
  mutate(year = 2021) %>% 
  select(names(past_deaths))

scotland_int_in_2021 <- filter(other_int_in, year == 2020, gss_code == "S92000003") %>% 
  mutate(share = int_in/sum(int_in),
         int_in = share * scotland_int_in_total_2021) %>% 
  mutate(year = 2021) %>% 
  select(names(past_int_in))

scotland_int_out_2021 <- filter(other_int_out, year == 2020, gss_code == "S92000003") %>% 
  mutate(share = int_out/sum(int_out),
         int_out = share * scotland_int_out_total_2021) %>% 
  mutate(year = 2021) %>% 
  select(names(past_int_out))

scotland_dom_in_2021 <- filter(other_dom_in, year == 2020, gss_code == "S92000003") %>% 
  mutate(share = dom_in/sum(dom_in),
         dom_in = share * scotland_dom_in_total_2021) %>% 
  mutate(year = 2021) %>% 
  select(names(past_dom_in))

scotland_dom_out_2021 <- filter(other_dom_out, year == 2020, gss_code == "S92000003") %>% 
  mutate(share = dom_out/sum(dom_out),
         dom_out = share * scotland_dom_out_total_2021) %>% 
  mutate(year = 2021) %>% 
  select(names(past_dom_out))


#-----------------

# 2021: N Ireland Wales
approx_est <- function(data, codes = c("W","N")){
  
  data_1 <- filter(data, substr(gss_code,1,1) %in% codes)
  
  data_2 <- data_1 %>%
    filter(year == 2019) %>% 
    select(-year) %>% 
    rename(value_1 = last(names(data)))
  
  data_3 <- data_1 %>%
    filter(year == 2020) %>% 
    select(-year)%>% 
    rename(value_2 = last(names(data)))
  
  data_4 <- left_join(data_2, data_3, by = c("gss_code", "sex", "age")) %>% 
    mutate(value_3 = value_2 + (value_2-value_1)) %>% 
    rename(!!last(names(data)) := value_3) %>% 
    mutate(year = 2021) %>% 
    select(names(data)) %>% 
    bind_rows(data)
  
  return(data_4)
  
}

other_popn <- approx_est(other_popn)
other_births <- approx_est(other_births)
other_deaths <- approx_est(other_deaths)
other_int_in <- approx_est(other_int_in)
other_int_out <- approx_est(other_int_out)
other_dom_in <- approx_est(other_dom_in)
other_dom_out <- approx_est(other_dom_out)

#-------------------------------------------------------------------------------

# split into individual components files
gla_popn <- filter(gla_series, component == "population") %>% 
  select(gss_code, year, sex, age, popn = value) %>% 
  data.frame()  %>% 
  bind_rows(past_popn)%>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_popn, scotland_2021_pop) %>% 
  check_negative_values("popn")

gla_births <- filter(gla_series, component == "births") %>% 
  select(gss_code, year, sex, age, births = value) %>% 
  data.frame() %>% 
  bind_rows(past_births)%>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_births, scotland_births_2021) %>% 
  check_negative_values("births") %>% 
  complete_popn_dataframe(col_data = "births")

gla_deaths <- filter(gla_series, component == "deaths") %>% 
  select(gss_code, year, sex, age, deaths = value) %>% 
  data.frame() %>% 
  bind_rows(past_deaths)%>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_deaths, scotland_deaths_2021) %>% 
  check_negative_values("deaths")

gla_int_in <- filter(gla_series, component == "international_in") %>% 
  select(gss_code, year, sex, age, int_in = value) %>% 
  data.frame() %>% 
  bind_rows(past_int_in) %>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_int_in, scotland_int_in_2021) %>% 
  check_negative_values("int_in") 

gla_int_out <- filter(gla_series, component == "international_out") %>% 
  select(gss_code, year, sex, age, int_out = value) %>% 
  data.frame() %>% 
  bind_rows(past_int_out)%>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_int_out, scotland_int_out_2021) %>% 
  check_negative_values("int_out")

gla_int_net <- gla_int_out %>% 
  mutate(int_in = int_out *-1) %>% 
  select(-int_out) %>% 
  bind_rows(gla_int_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(int_net = sum(int_in), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------
# Domestic
# TODO This needs to be rebuilt from the O/D matrix

dom_matrix <- readRDS("input_data/domestic_migration/2020/domestic_migration_flows_ons_(2021_geog).rds")
region_lookup <- readRDS("input_data/lookup/district_to_region_(2021 geog).rds")

regional_dom <- aggregate_regional_flows(dom_matrix,
                                         region_lookup = region_lookup,
                                         flow_col = "value",
                                         inner_outer_lookup =  "input_data/lookup/inner_and_outer_london.rds")

region_dom_in <- regional_dom[[1]] %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) == "E")

region_dom_out <- regional_dom[[1]] %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) == "E")

national_dom_in <- regional_dom[[2]] %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) %in% c("E","W"))

national_dom_out <- regional_dom[[2]] %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) %in% c("E","W"))

sub_reg_dom_in <- regional_dom[[3]] %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,3)=="E13") 

sub_reg_dom_out <- regional_dom[[3]] %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,3)=="E13")

gla_dom_in <- filter(gla_series, component == "internal_in") %>% 
  select(gss_code, year, sex, age, dom_in = value) %>% 
  data.frame()%>% 
  bind_rows(past_dom_in)%>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_dom_in, scotland_dom_in_2021,
            region_dom_in, national_dom_in, sub_reg_dom_in) %>% 
  check_negative_values("dom_in") %>% 
  data.frame()

gla_dom_out <- filter(gla_series, component == "internal_out") %>% 
  select(gss_code, year, sex, age, dom_out = value) %>% 
  data.frame() %>% 
  bind_rows(past_dom_out) %>% 
  filter(substr(gss_code,1,3) %in% england_codes) %>% 
  bind_rows(other_dom_out, scotland_dom_out_2021,
            region_dom_out, national_dom_out, sub_reg_dom_out) %>% 
  check_negative_values("dom_out") %>% 
  data.frame()

#-------------------------------------------------------------------------------

sum(is.na(gla_popn))
sum(is.na(gla_births))
sum(is.na(gla_deaths))
sum(is.na(gla_int_in))
sum(is.na(gla_int_out))
sum(is.na(gla_int_net))
sum(is.na(gla_dom_in))
sum(is.na(gla_dom_out))

#save
saveRDS(gla_popn, "input_data/mye/2021/population_gla.rds")
saveRDS(gla_births, "input_data/mye/2021/births_gla.rds")
saveRDS(gla_deaths, "input_data/mye/2021/deaths_gla.rds")
saveRDS(gla_int_in, "input_data/mye/2021/int_in_gla.rds")
saveRDS(gla_int_out, "input_data/mye/2021/int_out_gla.rds")
saveRDS(gla_int_net, "input_data/mye/2021/int_net_gla.rds")
saveRDS(gla_dom_in, "input_data/mye/2021/dom_in_gla.rds")
saveRDS(gla_dom_out, "input_data/mye/2021/dom_out_gla.rds")

rm(list=ls())
