library(data.table)
library(dplyr)

#The 2019 ONS series only has data for England & Wales
#2019 data for scotland must be modeled from component totals
#earlier data for scotland are bound on from the 2018 file

message('2019 Scotland MYE')

output_location <- "input_data/mye/2019/"
data_location <- "Q:/Teams/D&PA/Data/population_estimates/nrs_nisra_estimates/scotland"

fetch_2018_data <- function(x, component){
  readRDS(paste0("input_data/mye/2018/",component,".rds")) %>% 
    filter(gss_code == "S92000003") %>% 
    data.frame() %>%
    select(names(x)) %>% 
    rbind(x) %>%
    arrange(year, gss_code, sex, age)
}

#population
sc_popn <- fread(paste0(data_location, "/2019_mye_population_scotland.csv")) %>%
  tidyr::pivot_longer(values_to = "popn", names_to="sex", cols=c("male","female")) %>% 
  data.frame() %>%
  fetch_2018_data("population_ons")

ons_popn <- readRDS(paste0(output_location, "population_ons.rds")) %>%
  rbind(sc_popn)

#total components
sc_components <- fread(paste0(data_location, "/2019_mye_components_scotland.csv")) %>% 
  data.frame()

#births
sc_births <- filter(sc_components, component == "births") %>% 
  mutate(male = value*(105/205), female = value*(100/205),
         age = 0) %>%
  select(-value, -component) %>% 
  tidyr::pivot_longer(values_to = "births", names_to="sex", cols=c("male","female")) %>% 
  data.frame() %>%
  tidyr::complete(year, gss_code, sex, age = 0:90, fill = list(births = 0)) %>% 
  fetch_2018_data("births_ons")

ons_births <- readRDS(paste0(output_location, "births_ons.rds")) %>%
  rbind(sc_births)

#deaths
sc_deaths_total <- filter(sc_components, component == "deaths") %>%
  select(-component)

ons_deaths <- readRDS(paste0(output_location, "deaths_ons.rds"))

sc_deaths <- ons_deaths %>% 
  filter(substr(gss_code,1,1)=="E", year == 2019) %>% 
  group_by(year, sex, age) %>% 
  summarise(eng_deaths = sum(deaths), .groups = 'drop_last') %>% 
  data.frame() %>% 
  left_join(sc_deaths_total, by="year") %>% 
  mutate(deaths = (eng_deaths/sum(eng_deaths))*value) %>% 
  select(names(ons_deaths)) %>%
  fetch_2018_data("deaths_ons")

ons_deaths <- rbind(ons_deaths, sc_deaths)

#international in
sc_int_in_total <- filter(sc_components, component == "int_in") %>%
  select(-component)

ons_int_in <- readRDS(paste0(output_location, "int_in_ons.rds"))

sc_int_in <- ons_int_in %>% 
  filter(substr(gss_code,1,1)=="E", year == 2019) %>% 
  group_by(year, sex, age) %>% 
  summarise(eng_int_in = sum(int_in), .groups = 'drop_last') %>% 
  data.frame() %>% 
  left_join(sc_int_in_total, by="year") %>% 
  mutate(int_in = (eng_int_in/sum(eng_int_in))*value) %>% 
  select(names(ons_int_in)) %>%
  tidyr::complete(year, gss_code, sex, age = 0:90, fill = list(int_in = 0)) %>%
  fetch_2018_data("international_in_ons")

ons_int_in <- rbind(ons_int_in, sc_int_in)

#international out
sc_int_out_total <- filter(sc_components, component == "int_out")%>%
  select(-component)

ons_int_out <- readRDS(paste0(output_location, "int_out_ons.rds"))

sc_int_out <- ons_int_out %>% 
  filter(substr(gss_code,1,1)=="E", year == 2019) %>% 
  group_by(year, sex, age) %>% 
  summarise(eng_int_out = sum(int_out), .groups = 'drop_last') %>% 
  data.frame() %>% 
  left_join(sc_int_out_total, by="year") %>% 
  mutate(int_out = (eng_int_out/sum(eng_int_out))*value) %>% 
  select(names(ons_int_out)) %>%
  tidyr::complete(year, gss_code, sex, age = 0:90, fill = list(int_out = 0)) %>%
  fetch_2018_data("international_out_ons")

ons_int_out <- rbind(ons_int_out, sc_int_out)

#international net
sc_int_net <- mutate(sc_int_out, int_net = int_out*-1) %>%
  select(-int_out) %>% 
  rbind(rename(sc_int_in, int_net = int_in)) %>%
  filter(year == 2019) %>% 
  group_by(gss_code,year,sex,age) %>% 
  summarise(int_net = sum(int_net), .groups = 'drop_last') %>% 
  data.frame() %>%
  fetch_2018_data("international_net_ons")

ons_int_net <- readRDS(paste0(output_location, "int_net_ons.rds")) %>% 
  rbind(sc_int_net)

#validate data
validate_population(ons_popn, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(ons_births, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(ons_deaths, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(ons_int_in, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(ons_int_out, test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
validate_population(ons_int_net, test_complete = TRUE, test_unique = TRUE)
dir.create(output_location, showWarnings = FALSE, recursive = TRUE)
#Bind to ONS data and save
saveRDS(ons_popn, paste0(output_location, "population_ons.rds"))
saveRDS(ons_births, paste0(output_location, "births_ons.rds"))
saveRDS(ons_deaths, paste0(output_location, "deaths_ons.rds"))
saveRDS(ons_int_in, paste0(output_location, "int_in_ons.rds"))
saveRDS(ons_int_out, paste0(output_location, "int_out_ons.rds"))
saveRDS(ons_int_net, paste0(output_location, "int_net_ons.rds"))

rm(list=ls())
