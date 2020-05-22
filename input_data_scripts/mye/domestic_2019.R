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
  recode_gss_codes(col_aggregation = c("gss_code","year","sex","age"), recode_to_year = 2020)

#calculate net domestic as a residual
domestic_migration_net <- readRDS("input_data/domestic_migration/2018/domestic_migration_net.rds") %>%
  select(-dom_in, -dom_out) %>% 
  recode_gss_codes(col_aggregation = c("gss_code","year","sex","age"), recode_to_year = 2020) %>%
  select(names(domestic_net)) %>% 
  rbind(domestic_net)

#compare 2019 net to other years to find a match
#spoiler: its 2004
london_net <- filter(domestic_migration_net, substr(gss_code,1,3)=="E09") %>% 
  group_by(year) %>%
  summarise(dom_net = sum(dom_net)) %>% 
  ungroup()

domestic_migration_flows_ons <- readRDS("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds") %>%
  recode_gss_codes(col_aggregation = c("gss_out","gss_in","year","sex","age"),
                   col_geog = "gss_in", recode_to_year = 2020) %>%
  recode_gss_codes(col_aggregation = c("gss_out","gss_in","year","sex","age"),
                   col_geog = "gss_out", recode_to_year = 2020) %>%
  filter(gss_out != gss_in)

#Use 2004 flows as 2019 flows (they're not identical but it doesn't matter)
#Make all of the domestic components
flows_2004 <- filter(domestic_migration_flows_ons, year == 2004) %>%
  mutate(year = 2019)

dom_flows <- rbind(domestic_migration_flows_ons, flows_2004) 

domestic_migration_net <- readRDS("input_data/domestic_migration/2018/domestic_migration_net.rds") %>%
  recode_gss_codes(col_aggregation = c("gss_code","year","sex","age"), recode_to_year = 2020)

net_2004 <- filter(domestic_migration_net, year == 2004) %>%
  mutate(year = 2019)

domestic_migration_net <- rbind(domestic_migration_net, net_2004)

domestic_migration_in <- domestic_migration_net %>%
  select(year, gss_code, sex, age, dom_in)

domestic_migration_out <- domestic_migration_net %>%
  select(year, gss_code, sex, age, dom_out)

dom_dir <- "input_data/domestic_migration/2019"
dir.create(dom_dir, showWarnings = FALSE)
saveRDS(domestic_migration_net, paste0(dom_dir, "/temp_domestic_migration_net.rds"))
saveRDS(domestic_migration_in, paste0(dom_dir, "/temp_domestic_migration_in.rds"))
saveRDS(domestic_migration_out, paste0(dom_dir, "/temp_domestic_migration_out.rds"))
saveRDS(dom_flows, paste0(dom_dir, "/temp_domestic_flows.rds"))

rm(list=ls())
