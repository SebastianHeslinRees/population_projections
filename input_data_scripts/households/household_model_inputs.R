#ONS Household Rates
library(dplyr)
library(data.table)
library(popmodules)

read_hh_rates_files <- function(file, file_location, yr){
  
  x <- readRDS(paste0(file_location, file)) %>%
    filter(year == yr) %>%
    mutate(sex = case_when(sex == "F" ~ "female",
                           sex == "M" ~ "male"))
}

file_location <- "Q:/Teams/D&PA/Data/household_projections/ONS_data/2016_based/model_inputs"

rates_2001 <- read_hh_rates_files("/hh_rep_rates.rds", file_location, 2001) %>%
  recode_gss_codes(data_cols = "HRR",
                   fun = list(mean),
                   recode_gla_codes = TRUE)

rates_2011 <- read_hh_rates_files("/hh_rep_rates.rds", file_location, 2011) %>%
  recode_gss_codes(data_cols = "HRR",
                   fun = list(mean),
                   recode_gla_codes = TRUE)

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

rates <- data.table::rbindlist(rates) %>%
  data.frame()

#Split out 85_89 & 90+ from 85+ data using ONS rounded rates
ons_data_location <- "Q:/Teams/D&PA/Data/household_projections/ONS_data/2016_based/csv/"
merged_lookup <- readRDS("input_data/lookup/2011_merged_la_to_la.rds") %>%
  select(-merged_name)

rounded_rates <- rbind(data.table::fread(paste0(ons_data_location, "rounded_hh_rep_rates_female.csv"), header = T),
                       data.table::fread(paste0(ons_data_location, "rounded_hh_rep_rates_male.csv"), header = T)) %>%
  tibble() %>%
  filter(age_group %in% c("85_89","90+")) %>%
  tidyr::pivot_longer(5:45, names_to = "year", values_to = "HRR") %>%
  mutate(year = as.numeric(year),
         sex = case_when(sex == "Female" ~ "female",
                         sex == "Male" ~ "male")) %>%
  select(-district) %>%
  rename(merged_gss_code = gss_code) %>%
  left_join(merged_lookup, by="merged_gss_code") %>%
  mutate(gss_code = ifelse(is.na(gss_code),merged_gss_code,gss_code)) %>%
  select(-merged_gss_code) %>%
  popmodules::recode_gss_codes(data_cols = "HRR",
                               fun=list(mean),
                               recode_gla_codes = TRUE) %>%
  filter(year %in% unique(rates$year) & gss_code %in% unique(rates$gss_code))

rates <- filter(rates, age_group != "85_over") %>%
  rbind(rounded_rates)

# split city into city and westminster into two GSS codes with the same rates
rates <- filter(rates, gss_code=="E09000001") %>%
  mutate(gss_code = "E09000033") %>%
  rbind(rates)

# same for cornwall and scilly
rates <- filter(rates, gss_code=="E06000053") %>%
  mutate(gss_code = "E06000052") %>%
  rbind(rates)

rm(list=setdiff(ls(),c("rates","merged_lookup","ons_data_location")))


ce <- rbind(data.table::fread(paste0(ons_data_location, "ce_population_female.csv"), header = T),
            data.table::fread(paste0(ons_data_location, "ce_population_male.csv"), header = T)) %>%
  tibble() %>%
  tidyr::pivot_longer(5:45, names_to = "year", values_to = "ce_pop") %>%
  mutate(year = as.numeric(year),
         sex = case_when(sex == "Female" ~ "female",
                         sex == "Male" ~ "male")) %>%
  select(-district) %>%
  popmodules::recode_gss_codes(data_cols = "ce_pop",
                               recode_gla_codes = TRUE)

hh_pop <- rbind(data.table::fread(paste0(ons_data_location, "hh_population_female.csv"), header = T),
                data.table::fread(paste0(ons_data_location, "hh_population_male.csv"), header = T)) %>%
  tibble() %>%
  tidyr::pivot_longer(5:45, names_to = "year", values_to = "hh_pop") %>%
  mutate(year = as.numeric(year),
         sex = case_when(sex == "Female" ~ "female",
                         sex == "Male" ~ "male")) %>%
  select(-district) %>%
  popmodules::recode_gss_codes(data_cols = "hh_pop",
                               recode_gla_codes = TRUE)

ce <- left_join(ce, hh_pop, by = c("gss_code", "age_group", "sex", "year")) %>%
  mutate(ce_rate = ifelse(age_group %in% c("75_79","80_84","85_89","90+"), ce_pop/(ce_pop+hh_pop), NA)) %>%
  select(-hh_pop)


#Stage 2
stage_2_inputs <- data.table::fread(paste0(ons_data_location,"s2_household_representative_rate.csv"),header = TRUE) %>%
  tibble() %>%
  tidyr::pivot_longer(5:45, names_to = "year", values_to = "rate") %>%
  select(-AREA) %>%
  setnames(c("merged_gss_code","age_group","household_type","year","rate")) %>%
  left_join(merged_lookup, by="merged_gss_code") %>%
  mutate(gss_code = ifelse(is.na(gss_code),merged_gss_code,gss_code)) %>%
  select(-merged_gss_code) %>%
  popmodules::recode_gss_codes(data_cols = "rate",
                               fun = list(mean),
                               recode_gla_codes = TRUE) %>%
  mutate(year = as.numeric(year))

pop_under_16 <-  expand.grid(gss_code = unique(stage_2_inputs$gss_code),
                             year = unique(stage_2_inputs$year),
                             household_type = unique(stage_2_inputs$household_type),
                             age_group = "0_15",
                             rate = 0,
                             stringsAsFactors = FALSE)

stage_2_inputs <- rbind(stage_2_inputs, pop_under_16)

dir.create("input_data/household_model", showWarnings = FALSE)
saveRDS(rates, "input_data/household_model/ons_household_representative_rates.rds")
saveRDS(ce, "input_data/household_model/ons_communal_establishment_population.rds")
saveRDS(stage_2_inputs, "input_data/household_model/ons_headship_rates_2016.rds")
rm(list=ls())
#--------------------------------------------------------

#DCLG Data
data_location <-  "Q:/Teams/D&PA/Data/household_projections/DCLG_data/model_inputs/"

stage1_data <- readRDS(paste0(data_location,"2014 DCLG stage 1 data.rds")) %>%
  setnames(c("gss_code","year","sex","household_type","age_group","households",
             "household_population","institutional_population","total_population","hh_rep_rates"))%>%
  mutate(sex = case_when(sex == "F" ~ "female",
                         sex == "M" ~ "male"))

stage_1_totals <- select(stage1_data, -hh_rep_rates) %>%
  recode_gss_codes(data_cols = c("households","household_population",
                                 "institutional_population","total_population"),
                   recode_gla_codes = TRUE)

stage_1_rates <- select(stage1_data, -households, -household_population,
                        -institutional_population, -total_population) %>%
  recode_gss_codes(data_cols = "hh_rep_rates",
                   fun = list(mean),
                   recode_gla_codes = TRUE)

stage1_data <- left_join(stage_1_totals, stage_1_rates, by = c("gss_code","year","sex","household_type","age_group"))

stage2_data <- readRDS(paste0(data_location,"2014 DCLG Stage 2 headship rates.rds")) %>%
  rename(rate = DCLG.rate) %>%
  select(-district) %>%
  popmodules::recode_gss_codes(data_cols = "rate", fun = list(mean), recode_gla_codes = TRUE)

saveRDS(stage1_data, "input_data/household_model/dclg_stage1_data_2014.rds")
saveRDS(stage2_data, "input_data/household_model/dclg_headship_rates_2014.rds")

rm(list=ls())


