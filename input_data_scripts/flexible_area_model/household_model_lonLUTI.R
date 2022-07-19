library(tidyr)
library(data.table)
library(dplyr)

# This is broadly the ONS LA-level household model (stage 1) with LSOA data
# It uses census data from 2001 and 2011
# The age groups for the 4 table it uses are different so there are some shenanigans

message("lonLUTI Household model")

input_data_dir <- "input_data/flexible_area_model/"
data_dir_Q_drive <- "Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/"

# Read in data
hh_pop_2011 <- fread(paste0(data_dir_Q_drive, "HH_pop_Age_Sex_LSOA_2011.csv"), header = TRUE) %>% data.frame()
hh_pop_2001 <- fread(paste0(data_dir_Q_drive, "HH_pop_Age_Sex_LSOA_2001.csv"), header = TRUE) %>% data.frame()

hrp_2011 <- fread(paste0(data_dir_Q_drive,"HRP_Age_Sex_LSOA_2011.csv"), header = TRUE) %>% data.frame()
hrp_2001 <- fread(paste0(data_dir_Q_drive,"HRP_Age_Sex_LSOA_2001.csv"), header = TRUE) %>% data.frame()

#-------------------------------------------------------------------------------

# Lookups
lsoa_lookup <- readRDS(paste0(input_data_dir, "lookups/lsoa_2001_to_lsoa_2011_lookup.rds"))
lsoa_lonLUTI_lookup <- readRDS(paste0(input_data_dir, "lookups/lsoa_to_LonLUTI3_lookup.rds"))

#-------------------------------------------------------------------------------

# 2011 Household population

hh_pop_2011 <- pivot_longer(hh_pop_2011, cols = starts_with("X"),
                            names_to = "age", values_to = "hh_pop") %>% 
  data.frame() %>% 
  mutate(age_group = case_when(age == "X0_4" ~ "0_17",
                               age == "X5_7" ~ "0_17",
                               age == "X8_9" ~ "0_17",
                               age == "X10_14" ~ "0_17",
                               age == "X15" ~ "0_17",
                               age == "X16_17" ~ "0_17",
                               age == "X18_19" ~ "18_24",
                               age == "X20_24" ~ "18_24",
                               age == "X25_29" ~ "25_34",
                               age == "X30_34" ~ "25_34",
                               age == "X35_39" ~ "35_49",  
                               age == "X40_44" ~ "35_49",
                               age == "X45_49" ~ "35_49",
                               age == "X50_54" ~ "50_64",
                               age == "X55_59" ~ "50_64",
                               age == "X60_64" ~ "50_64",
                               age == "X65_69" ~ "65_plus",
                               age == "X70_74" ~ "65_plus",
                               age == "X75_79" ~ "65_plus",
                               age == "X80_84" ~ "65_plus",
                               age == "X85_over" ~ "65_plus",
                               TRUE ~ "NA")) %>% 
  group_by(gss_code, sex, age_group) %>% 
  summarise(hh_pop = sum(hh_pop), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

# 2001 Household population
# Note the age groupings here mean that 18-19 year olds end up in the 0-17 group

hh_pop_2001 <- pivot_longer(hh_pop_2001, cols = starts_with("X"),
                            names_to = "age", values_to = "hh_pop") %>% 
  data.frame() %>% 
  mutate(age_group = case_when(age == "X0_4" ~ "0_17",
                               age == "X5_9" ~ "0_17",
                               age == "X10_14" ~ "0_17",
                               age == "X15_19" ~ "0_17",
                               age == "X20_24" ~ "18_24",
                               age == "X25_29" ~ "25_34",
                               age == "X30_34" ~ "25_34",
                               age == "X35_39" ~ "35_49",  
                               age == "X40_44" ~ "35_49",
                               age == "X45_49" ~ "35_49",
                               age == "X50_54" ~ "50_64",
                               age == "X55_59" ~ "50_64",
                               age == "X60_64" ~ "50_64",
                               age == "X65_69" ~ "65_plus",
                               age == "X70_74" ~ "65_plus",
                               age == "X75_79" ~ "65_plus",
                               age == "X80_84" ~ "65_plus",
                               age == "X85_89" ~ "65_plus",
                               age == "X90_over" ~ "65_plus",
                               TRUE ~ "NA"))  %>% 
  group_by(gss_code, sex, age_group) %>% 
  summarise(hh_pop = sum(hh_pop), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

# 2011 Household Representative person population
#Assume all under 24s are actually 18-24 when applying denominators

hrp_2011 <- pivot_longer(hrp_2011, cols = starts_with("X"),
                         names_to = "age", values_to = "hrp") %>% 
  mutate(age_group = substr(age,2,8), 
         age_group = ifelse(age_group == "0_24", "18_24", age_group)) %>% 
  data.frame() %>% 
  select(-age, -gss_name)

#-------------------------------------------------------------------------------

# 2001 Household Representative person population
# Assume all under 24s are actually 18-24 when applying denominators

hrp_2001 <- pivot_longer(hrp_2001, cols = starts_with("X"),
                         names_to = "age", values_to = "hrp")%>% 
  data.frame() %>% 
  mutate(age_group = case_when(age == "X0_19" ~ "18_24",
                               age == "X20_24" ~ "18_24",
                               age == "X25_29" ~ "25_34",
                               age == "X30_34" ~ "25_34",
                               age == "X35_39" ~ "35_49",  
                               age == "X40_44" ~ "35_49",
                               age == "X45_49" ~ "35_49",
                               age == "X50_54" ~ "50_64",
                               age == "X55_59" ~ "50_64",
                               age == "X60_64" ~ "50_64",
                               age == "X65_69" ~ "65_plus",
                               age == "X70_74" ~ "65_plus",
                               age == "X75_79" ~ "65_plus",
                               age == "X80_84" ~ "65_plus",
                               age == "X85_89" ~ "65_plus",
                               age == "X90_over" ~ "65_plus",
                               TRUE ~ "NA")) %>% 
  group_by(gss_code, sex, age_group) %>% 
  summarise(hrp = sum(hrp), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

# A function to recode the 2001 LSOAs to 2011 LSOAs using a lookup

recode <- function(x, lookup){
  
  U <- filter(lookup, CHGIND == "U")
  S <- filter(lookup, CHGIND == "S") %>% 
    group_by(LSOA01CD) %>% 
    mutate(n = n()) %>% 
    data.frame() %>% 
    select(LSOA01CD, LSOA11CD, n)
  
  M <- filter(lookup, CHGIND == "M")
  X <- filter(lookup, CHGIND == "X")
  
  changes <- rbind(M, X) %>% 
    select(gss_code = LSOA01CD, LSOA11CD)
  
  value_col <- last(names(x))
  
  unchanged <- filter(x, gss_code %in% U$LSOA01CD)
  changed <- filter(x, gss_code %in% changes$gss_code) %>% 
    left_join(changes, by="gss_code") %>% 
    select(-gss_code) %>% 
    rename(gss_code = LSOA11CD) %>% 
    group_by(gss_code, sex, age_group) %>% 
    summarise(!!value_col := sum(!!sym(value_col)),
              .groups = 'drop_last') %>% 
    data.frame()
  
  splits <- filter(x, gss_code %in% S$LSOA01CD) %>% 
    full_join(S, by=c(gss_code = "LSOA01CD")) %>% 
    mutate(!!value_col := !!sym(value_col)/n) %>% 
    select(-gss_code, -n) %>% 
    rename(gss_code = LSOA11CD)
  
  rbind(unchanged, changed, splits)
  
}

#-------------------------------------------------------------------------------

# Standardise the data

hrp_2001 <- recode(hrp_2001, lsoa_lookup)
hh_pop_2001 <- recode(hh_pop_2001, lsoa_lookup)

codes_not_in_lookup <- c("E01032522","E01032540","E01032608","E01033730") # TODO Look into but for now ignore - all outside London 
hrp_2011 <- filter(hrp_2011, !gss_code %in% codes_not_in_lookup)
hh_pop_2011 <- filter(hh_pop_2011, !gss_code %in% codes_not_in_lookup)

#-------------------------------------------------------------------------------

# ONS model

lonLUTI_rates_2001 <- left_join(hh_pop_2001, hrp_2001, by = c("gss_code", "sex", "age_group")) %>% 
  filter(gss_code != "E01019077") %>%  #Isles of Scilly
  left_join(lsoa_lonLUTI_lookup, by=c("gss_code"="gss_code_lsoa")) %>% 
  group_by(LonLUTI3, sex, age_group) %>%
  summarise(hrp = sum(hrp*lsoa_share),
            hh_pop = sum(hh_pop*lsoa_share),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(hh_rep_rate = ifelse(hh_pop == 0, 0, hrp/hh_pop),
         hh_rep_rate = ifelse(is.na(hh_rep_rate), 0, hh_rep_rate),
         year = 2001) %>% 
  select(-hrp, -hh_pop)

lonLUTI_rates_2011 <- left_join(hh_pop_2011, hrp_2011, by = c("gss_code", "sex", "age_group")) %>%
  filter(gss_code != "E01019077") %>%  #Isles of Scilly
  left_join(lsoa_lonLUTI_lookup, by=c("gss_code"="gss_code_lsoa")) %>% 
  mutate(hrp = ifelse(age_group == "0_17", 0, hrp)) %>% 
  group_by(LonLUTI3, sex, age_group) %>%
  summarise(hrp = sum(hrp*lsoa_share),
            hh_pop = sum(hh_pop*lsoa_share),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(hh_rep_rate = ifelse(hh_pop == 0, 0, hrp/hh_pop),
         year = 2011) %>% 
  select(-hrp, -hh_pop)

lonLUTI_rates <- vector("list", 2041)
lonLUTI_rates[[2001]] <- lonLUTI_rates_2001 %>% filter(!is.na(LonLUTI3))
lonLUTI_rates[[2011]] <- lonLUTI_rates_2011 %>% filter(!is.na(LonLUTI3))

#-------------------------------------------------------------------------------

# Project rates to 2021
# Modified 2-point exponential
for(i in 2001:2021){
  
  c <- 2011#ifelse(i < 2011, 2001, 2011) #most recent census
  d <- 2001#ifelse(i < 2011, 2011, 2001) #furthest away census
  
  yc <- lonLUTI_rates[[c]] %>% select(-year)
  yd <- lonLUTI_rates[[d]] %>% select(-year)
  
  lonLUTI_rates[[i]] <- left_join(yc, yd, by = c("LonLUTI3", "sex", "age_group")) %>%
    setnames(c("LonLUTI3", "sex", "age_group", "yc", "yd")) %>%
    mutate(k = ifelse(yc >= yd, 1, 0),
           a = yd -k,
           b = (yc-k)/(yd-k),
           x = (i-d)/(c-d),
           y = k + (a*(b^x)),
           year = i) %>%
    select(LonLUTI3, year, sex, age_group, hh_rep_rate = y)
}
rm(yc, yd, c, d, i)

#Hold rates constant 2022-2041
for(i in 2022:2041){
  lonLUTI_rates[[i]] <- lonLUTI_rates[[2021]] %>%
    mutate(year = i)
}

lonLUTI_rates <- data.table::rbindlist(lonLUTI_rates) %>%
  data.frame()

sum(is.na(lonLUTI_rates))

#-------------------------------------------------------------------------------

saveRDS(lonLUTI_rates, paste0(input_data_dir, "processed/hh_rep_rate_lonLUTI.rds"))


