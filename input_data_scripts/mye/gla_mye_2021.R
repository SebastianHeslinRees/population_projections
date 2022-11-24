#revised GLA estimates of population & international flows
#data created in a separate process
#this script calls on the processed data
#data created by ML September 2021

message("2021 GLA MYE series")
dir.create("input_data/mye/2021/", showWarnings = FALSE)

gla_series <- readRDS("Q:/Teams/D&PA/Demography/MYE/gla_revised_mye_series.rds")
acceptable_codes <- c("E06", "E07", "E08", "E09")#, "W06", "N92", "S92")

# 2002 - 2011
previous_data <- "input_data/mye/2020/"
past_popn <- readRDS(paste0(previous_data, "population_ons.rds")) %>% filter(year < 2011)
past_births <- readRDS(paste0(previous_data, "births_ons.rds")) %>% filter(year < 2012)
past_deaths <- readRDS(paste0(previous_data, "deaths_ons.rds")) %>% filter(year < 2012)
past_int_in <- readRDS(paste0(previous_data, "int_in_ons.rds")) %>% filter(year < 2012)
past_int_out <- readRDS(paste0(previous_data, "int_out_ons.rds")) %>% filter(year < 2012)
past_int_net <- readRDS(paste0(previous_data, "int_net_ons.rds")) %>% filter(year < 2012)

#-------------------------------------------------------------------------------

# split into individual components files
gla_popn <- filter(gla_series, component == "population") %>% 
  select(gss_code, year, sex, age, popn = value) %>% 
  data.frame() %>% 
  check_negative_values("popn") %>% 
  bind_rows(past_popn)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes) 

gla_births <- filter(gla_series, component == "births") %>% 
  select(gss_code, year, sex, age, births = value) %>% 
  data.frame()  %>% 
  check_negative_values("births") %>% 
  bind_rows(past_births)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes) 

gla_deaths <- filter(gla_series, component == "deaths") %>% 
  select(gss_code, year, sex, age, deaths = value) %>% 
  data.frame() %>% 
  check_negative_values("deaths") %>% 
  bind_rows(past_deaths)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes) 

gla_int_in <- filter(gla_series, component == "international_in") %>% 
  select(gss_code, year, sex, age, int_in = value) %>% 
  data.frame() %>% 
  check_negative_values("int_in") %>% 
  bind_rows(past_int_in)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes) 

gla_int_out <- filter(gla_series, component == "international_out") %>% 
  select(gss_code, year, sex, age, int_out = value) %>% 
  data.frame() %>% 
  check_negative_values("int_out") %>% 
  bind_rows(past_int_out)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes) 

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

past_dom_in <- readRDS(paste0(previous_data, "dom_in_ons.rds")) %>% filter(year < 2012)
past_dom_out <- readRDS(paste0(previous_data, "dom_out_ons.rds")) %>% filter(year < 2012)
#past_dom_net <- readRDS(paste0(previous_data, "dom_net_ons.rds")) %>% filter(year < 2012)

gla_dom_in <- filter(gla_series, component == "internal_in") %>% 
  select(gss_code, year, sex, age, dom_in = value) %>% 
  data.frame() %>% 
  check_negative_values("dom_in") %>% 
  bind_rows(past_dom_in)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes) 

gla_dom_out <- filter(gla_series, component == "internal_out") %>% 
  select(gss_code, year, sex, age, dom_out = value) %>% 
  data.frame() %>% 
  check_negative_values("dom_out") %>% 
  bind_rows(past_dom_out)%>% 
  filter(substr(gss_code,1,3) %in% acceptable_codes)

#-------------------------------------------------------------------------------

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
