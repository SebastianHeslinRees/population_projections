library(dplyr)

new_data_folder <- "Q:/Teams/D&PA/Demography/Projections/population_projections/input_data/mye/2018/"
existing_data_folder <- "input_data/mye/2018/"
out_new_data_folder <- "input_data/mye/2018"

####Population####

#GLA revised data
gla_popn <- readRDS(paste0(new_data_folder,"adj_mye_popn_2001_2018_ldn.rds")) %>%
  rename(gss_code = lad11cd,
         year = mye_year) %>%
  select(-source)

#Existing data
ons_popn <- readRDS(paste0(existing_data_folder,"population_ons.rds"))

#Replace ONS estimates with GLA estimates for London
switch <- filter(ons_popn, substr(gss_code,1,3)=="E09") %>%
  left_join(gla_popn, by=c("gss_code","year","sex","age")) %>%
  select(-popn) %>%
  rename(popn = count)

final_popn <- filter(ons_popn, substr(gss_code,1,3)!="E09") %>%
  rbind(switch)


####International####

gla_international <- readRDS(paste0(new_data_folder,"adj_mye_international_mig_2001_2018_ldn.rds")) %>%
  rename(gss_code = lad11cd,
         year = mye_year) %>%
  select(-source)

gla_int_in <- select(gla_international, gss_code, year, sex, age, int_in)
gla_int_out <- select(gla_international, gss_code, year, sex, age, int_out)
gla_int_net <- select(gla_international, gss_code, year, sex, age, int_net)

#Existing data
ons_int_in <- readRDS(paste0(existing_data_folder,"international_in_ons.rds"))
ons_int_out <- readRDS(paste0(existing_data_folder,"international_out_ons.rds"))
ons_int_net <- readRDS(paste0(existing_data_folder,"international_net_ons.rds"))


#Replace ONS estimates with GLA estimates for London
switch_in <- filter(ons_int_in, substr(gss_code,1,3)=="E09") %>%
  select(-int_in) %>%
  left_join(gla_int_in, by=c("gss_code","year","sex","age"))

switch_out <- filter(ons_int_out, substr(gss_code,1,3)=="E09") %>%
  select(-int_out) %>%
  left_join(gla_int_out, by=c("gss_code","year","sex","age"))

switch_net <- filter(ons_int_net, substr(gss_code,1,3)=="E09") %>%
  select(-int_net) %>%
  left_join(gla_int_net, by=c("gss_code","year","sex","age"))



final_int_in <- filter(ons_int_in, substr(gss_code,1,3)!="E09") %>%
  rbind(switch_in)

final_int_out <- filter(ons_int_out, substr(gss_code,1,3)!="E09") %>%
  rbind(switch_out)

final_int_net <- filter(ons_int_net, substr(gss_code,1,3)!="E09") %>%
  rbind(switch_net)


#Write out
datestamp <- Sys.Date()
saveRDS(final_popn, file = paste0("input_data/mye/2018/population_gla_", datestamp, ".rds"))
saveRDS(final_int_in, file = paste0("input_data/mye/2018/international_in_gla_", datestamp, ".rds"))
saveRDS(final_int_out, file = paste0("input_data/mye/2018/international_out_gla_", datestamp, ".rds"))
saveRDS(final_int_net, file = paste0("input_data/mye/2018/international_net_gla_", datestamp, ".rds"))

rm(list=ls())
