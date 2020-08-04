library(data.table)
library(dplyr)

#The 2019 ONS series only has data for England & Wales
#2019 data for northern_ireland must be modeled from component totals
#earlier data for northern_ireland are bound on from the 2018 file

output_location <- "input_data/mye/2019/"
data_location <- "Q:/Teams/D&PA/Data/population_estimates/nrs_nisra_estimates/northern_ireland"

fetch_2018_data <- function(x, component){
  readRDS(paste0("input_data/mye/2018/",component,".rds")) %>%
    filter(gss_code == "N92000002") %>%
    data.frame() %>%
    select(names(x)) %>%
    rbind(x) %>%
    arrange(year, gss_code, sex, age)
}

#population
ni_popn <- fread(paste0(data_location, "/2019_mye_population_northern_ireland.csv")) %>%
  data.frame() %>%
  fetch_2018_data("population_ons")

ons_popn <- readRDS(paste0(output_location, "population_ons.rds")) %>%
  rbind(ni_popn)

#total components
ni_components <- fread(paste0(data_location, "/2019_mye_components_northern_ireland.csv")) %>% 
  data.frame()

#births
ni_births <- filter(ni_components, component == "births") %>% 
  mutate(male = value*(105/205), female = value*(100/205),
         age = 0) %>%
  select(-value, -component) %>% 
  tidyr::pivot_longer(values_to = "births", names_to="sex", cols=c("male","female")) %>% 
  data.frame() %>%
  tidyr::complete(year, gss_code, sex, age = 0:90, fill = list(births = 0)) %>% 
  fetch_2018_data("births_ons")

ons_births <- readRDS(paste0(output_location, "births_ons.rds")) %>%
  rbind(ni_births)

#deaths
ni_deaths_total <- filter(ni_components, component == "deaths") %>%
  select(-component)

ons_deaths <- readRDS(paste0(output_location, "deaths_ons.rds"))

ni_deaths <- ons_deaths %>% 
  filter(substr(gss_code,1,1)=="E", year == 2019) %>% 
  group_by(year, sex, age) %>% 
  summarise(eng_deaths = sum(deaths)) %>% 
  data.frame() %>% 
  left_join(ni_deaths_total, by="year") %>% 
  mutate(deaths = (eng_deaths/sum(eng_deaths))*value) %>% 
  select(names(ons_deaths)) %>%
  fetch_2018_data("deaths_ons")

ons_deaths <- rbind(ons_deaths, ni_deaths)

#international in
ni_int_in_total <- filter(ni_components, component == "int_in") %>%
  select(-component)

ons_int_in <- readRDS(paste0(output_location, "int_in_ons.rds"))

ni_int_in <- ons_int_in %>% 
  filter(substr(gss_code,1,1)=="E", year == 2019) %>% 
  group_by(year, sex, age) %>% 
  summarise(eng_int_in = sum(int_in)) %>% 
  data.frame() %>% 
  left_join(ni_int_in_total, by="year") %>% 
  mutate(int_in = (eng_int_in/sum(eng_int_in))*value) %>% 
  select(names(ons_int_in)) %>%
  tidyr::complete(year, gss_code, sex, age = 0:90, fill = list(int_in = 0)) %>%
  fetch_2018_data("international_in_ons")

ons_int_in <- rbind(ons_int_in, ni_int_in)

#international out
ni_int_out_total <- filter(ni_components, component == "int_out")%>%
  select(-component)

ons_int_out <- readRDS(paste0(output_location, "int_out_ons.rds"))

ni_int_out <- ons_int_out %>% 
  filter(substr(gss_code,1,1)=="E", year == 2019) %>% 
  group_by(year, sex, age) %>% 
  summarise(eng_int_out = sum(int_out)) %>% 
  data.frame() %>% 
  left_join(ni_int_out_total, by="year") %>% 
  mutate(int_out = (eng_int_out/sum(eng_int_out))*value) %>% 
  select(names(ons_int_out)) %>%
  tidyr::complete(year, gss_code, sex, age = 0:90, fill = list(int_out = 0)) %>%
  fetch_2018_data("international_out_ons")

ons_int_out <- rbind(ons_int_out, ni_int_out)

#international net
ni_int_net <- mutate(ni_int_out, int_net = int_out*-1) %>%
  select(-int_out) %>% 
  rbind(rename(ni_int_in, int_net = int_in)) %>%
  filter(year == 2019) %>% 
  group_by(gss_code,year,sex,age) %>% 
  summarise(int_net = sum(int_net)) %>% 
  data.frame() %>%
  fetch_2018_data("international_net_ons")

ons_int_net <- readRDS(paste0(output_location, "int_net_ons.rds")) %>% 
  rbind(ni_int_net)

#validate data
validate_population(ons_popn)
validate_population(ons_births)
validate_population(ons_deaths)
validate_population(ons_int_in)
validate_population(ons_int_out)
validate_population(ons_int_net)
dir.create(output_location, showWarnings = FALSE, recursive = TRUE)
#Bind to ONS data and save
saveRDS(ons_popn, paste0(output_location, "population_ons.rds"))
saveRDS(ons_births, paste0(output_location, "births_ons.rds"))
saveRDS(ons_deaths, paste0(output_location, "deaths_ons.rds"))
saveRDS(ons_int_in, paste0(output_location, "int_in_ons.rds"))
saveRDS(ons_int_out, paste0(output_location, "int_out_ons.rds"))
saveRDS(ons_int_net, paste0(output_location, "int_net_ons.rds"))

rm(list=ls())
