#ONS Household Rates
library(dplyr)
library(data.table)
library(popmodules)

read_hh_data_files <- function(file, file_location, yr){
  
  x <- readRDS(paste0(file_location, file)) %>%
    filter(year == yr) %>%
    mutate(sex = case_when(sex == "F" ~ "female",
                           sex == "M" ~ "male"))
}

file_location <- "Q:/Teams/D&PA/Demography/Projections/R Models/ONS Household Projections/GLA implementation/input"

rates_2001 <- read_hh_data_files("/hh_rep_rates.rds", file_location, 2001) %>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","age_group"),
                     fun = list(mean))

rates_2011 <- read_hh_data_files("/hh_rep_rates.rds", file_location, 2011) %>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","age_group"),
                     fun = list(mean))

households_2001 <- read_hh_data_files("/households.rds", file_location, 2001)%>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","age_group"))

households_2011 <- read_hh_data_files("/households.rds", file_location, 2011)%>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","age_group"))

ce <- read_hh_data_files("/communal_pop.rds", file_location, 2011)%>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","age_group"))

rates <- vector("list", 2041)
rates[[2001]] <- rates_2001
rates[[2011]] <- rates_2011

#16 to 19-year-olds
#quinary age groups from 20-24 years to 85-89
#90 years and over.

#Project rates to 2021
#       Modified 2-point exponential
for(i in 2001:2021){
  
  c <- 2011#ifelse(i < 2011, 2001, 2011) #most recent census
  d <- 2001#ifelse(i < 2011, 2011, 2001) #furthest away census
  
  yc <- rates[[c]] %>% select(-year)
  yd <- rates[[d]] %>% select(-year)
  
  rates[[i]] <- left_join(yc, yd, by = c("gss_code", "sex", "age_group")) %>%
    setnames(c("gss_code", "sex", "age_group", "yc", "yd")) %>%
    mutate(k = ifelse(yc >= yd, 1, 0),
           a = yd -k,
           b = (yc-k)/(yd-k),
           x = (i-d)/(c-d),
           y = k + (a*(b^x)),
           year = i) %>%
    select(gss_code, year, sex, age_group, HRR = y)
}

#Hold rates constant 2022-2041
for(i in 2022:2041){
  rates[[i]] <- rates[[2021]] %>%
    mutate(year = i)
}

rates <- data.table::rbindlist(rates)

saveRDS(rates, "input_data/household_model/ons_household_representative_rates.rds")
saveRDS(ce, "input_data/household_model/ons_communal_establishment_population.rds")
rm(list=ls())
#--------------------------------------------------------

#DCLG Data
data_location <- "Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model - original/Inputs/"

stage1_data <- readRDS(paste0(data_location,"2014 DCLG stage 1 data.rds")) %>%
  setnames(c("gss_code","year","sex","household_type","age_group","households",
             "household_population","institutional_population","total_population","hh_rep_rates"))%>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male"))

stage_1_totals <- select(stage1_data, -hh_rep_rates) %>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","household_type","age_group"))

stage_1_rates <- select(stage1_data, -households, -household_population,
                        -institutional_population, -total_population) %>%
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","household_type","age_group"),
                     fun = list(mean))

stage1_data <- left_join(stage_1_totals, stage_1_rates, by = c("gss_code","year","sex","household_type","age_group"))

stage2_data <- readRDS(paste0(data_location,"2014 DCLG Stage 2 headship rates.rds")) %>%
  rename(rate = DCLG.rate) %>%
  select(-district) %>%
  recode_gss_to_2011(col_aggregation = c("gss_code", "year", "household_type", "age_group"), fun = list(mean))

saveRDS(stage1_data, "input_data/household_model/dclg_stage1_data_2014.rds")
saveRDS(stage2_data, "input_data/household_model/dclg_headship_rates_2014.rds")

#-------------------------------------------------------
#Lookup

region_codes <- data.table::fread("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/region to region gss code.csv")

district_to_region <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/district to region.rds") %>%
  left_join(region_codes, by="region") %>%
  select(gss_code, region_gss_code) %>%
  rbind(data.frame(gss_code = c("E07000100","E07000104"), region_gss_code = rep("E12000006")))

saveRDS(district_to_region, "input_data/household_model/district_to_region.rds")
rm(list=ls())


