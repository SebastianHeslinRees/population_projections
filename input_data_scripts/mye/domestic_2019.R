library(dplyr)
devtools::load_all("model_code/popmodules")

#read in all the other components
ons_popn <- readRDS("input_data/mye/2019/temp_ons_popn.rds")

births <- readRDS("input_data/mye/2019/temp_births.rds") %>%
  filter(year == 2019)

deaths <- readRDS("input_data/mye/2019/temp_deaths.rds") %>%
  filter(year == 2019)

ons_international_in <- readRDS("input_data/mye/2019/temp_ons_international_in.rds") %>%
  filter(year == 2019)

ons_international_out <- readRDS("input_data/mye/2019/temp_ons_international_out.rds") %>%
  filter(year == 2019)
  
popn_2018 <- ons_popn %>%
  filter(year == 2018)

popn_2019 <- ons_popn %>%
  filter(year == 2019)

domestic_net <- construct_popn_from_components(start_population = popn_2019,
                                               addition_data = list(deaths, ons_international_out),
                                               subtraction_data = list(births, ons_international_in, popn_2018),
                                               col_aggregation = c("gss_code", "sex", "age")) %>%
  mutate(year = 2019, age = as.numeric(age)) %>%
  arrange(year, gss_code, sex, age) %>%
  select(year, gss_code, sex, age, dom_net = popn) %>%
  recode_gss_codes(data_cols = "dom_net", recode_to_year = 2020)

#calculate net domestic as a residual
domestic_migration_net <- readRDS("input_data/domestic_migration/2018/domestic_migration_net.rds") %>%
  select(-dom_in, -dom_out) %>% 
  recode_gss_codes(data_cols = "dom_net", recode_to_year = 2020) %>%
  select(names(domestic_net)) %>% 
  rbind(domestic_net)

#compare 2019 net to other years to find a match
#spoiler: its 2004
london_net <- filter(domestic_migration_net, substr(gss_code,1,3)=="E09") %>% 
  group_by(year) %>%
  summarise(dom_net = sum(dom_net)) %>% 
  ungroup()

domestic_migration_flows_ons <- readRDS("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds") %>%
  recode_gss_codes(data_cols = "value", col_geog = "gss_in", recode_to_year = 2020) %>%
  recode_gss_codes(data_cols = "value", col_geog = "gss_out", recode_to_year = 2020) %>%
  filter(gss_out != gss_in)

#Use 2004 flows as 2019 flows (they're not identical but it doesn't matter)
#Make all of the domestic components
flows_2004 <- filter(domestic_migration_flows_ons, year == 2004) %>%
  mutate(year = 2019)

dom_flows <- rbind(domestic_migration_flows_ons, flows_2004) 

regional <- readRDS("input_data/domestic_migration/2018/regional_domestic_migration_flows_ons.rds")
regional <- filter(regional, year == 2004) %>%
  mutate(year = 2019) %>% 
  rbind(regional)

dom_in <- dom_flows %>%
  rbind(regional) %>% 
  group_by(year, gss_in, sex, age) %>%
  summarise(dom_in = sum(value)) %>%
  data.frame() %>% 
  rename(gss_code = gss_in)

dom_out <- dom_flows %>%
  rbind(regional) %>% 
  group_by(year, gss_out, sex, age) %>%
  summarise(dom_out = sum(value)) %>%
  data.frame() %>% 
  rename(gss_code = gss_out)

dom_net <- full_join(dom_in, dom_out, by=c("year","gss_code","sex","age")) %>%
  tidyr::replace_na(list(dom_in = 0, dom_out = 0)) %>%
  mutate(dom_net = dom_in - dom_out)

dom_dir <- "input_data/domestic_migration/2019"
dir.create(dom_dir, showWarnings = FALSE)
saveRDS(dom_net, paste0(dom_dir, "/temp_domestic_migration_net.rds"))
saveRDS(dom_in, paste0(dom_dir, "/temp_domestic_migration_in.rds"))
saveRDS(dom_out, paste0(dom_dir, "/temp_domestic_migration_out.rds"))
saveRDS(dom_flows, paste0(dom_dir, "/temp_domestic_flows.rds"))

rm(list=ls())
