library(tidyr)
library(data.table)

data_dir <- "input_data/new_ward_model/"
hh_pop_2011 <- fread(paste0(data_dir, "HH_pop_Age_Sex_LSOA_2011.csv"), header = TRUE) %>% data.frame()
hh_pop_2001 <- fread(paste0(data_dir, "HH_pop_Age_Sex_LSOA_2001.csv"), header = TRUE) %>% data.frame()

hrp_2011 <- fread(paste0(data_dir,"HRP_Age_Sex_LSOA_2011.csv"), header = TRUE) %>% data.frame()
hrp_2001 <- fread(paste0(data_dir,"HRP_Age_Sex_LSOA_2001.csv"), header = TRUE) %>% data.frame()

hh_pop_2011 <- pivot_longer(hh_pop_2011, cols = starts_with("X"),
                            names_to = "age", values_to = "hh_pop") %>% 
  data.frame() %>% 
  mutate(age_group = case_when(age == "X0_4" ~ "0_24",
                               age == "X5_7" ~ "0_24",
                               age == "X8_9" ~ "0_24",
                               age == "X10_14" ~ "0_24",
                               age == "X15" ~ "0_24",
                               age == "X16_17" ~ "0_24",
                               age == "X18_19" ~ "0_24",
                               age == "X20_24" ~ "0_24",
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

hh_pop_2001 <- pivot_longer(hh_pop_2001, cols = starts_with("X"),
                            names_to = "age", values_to = "hh_pop") %>% 
  data.frame() %>% 
  mutate(age_group = case_when(age == "X0_4" ~ "0_24",
                               age == "X5_9" ~ "0_24",
                               age == "X10_14" ~ "0_24",
                               age == "X15_19" ~ "0_24",
                               age == "X20_24" ~ "0_24",
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

hrp_2011 <- pivot_longer(hrp_2011, cols = starts_with("X"),
                         names_to = "age", values_to = "hrp") %>% 
  mutate(age_group = substr(age,2,8)) %>% 
  data.frame() %>% 
  select(-age, -gss_name)

hrp_2001 <- pivot_longer(hrp_2001, cols = starts_with("X"),
                         names_to = "age", values_to = "hrp")%>% 
  data.frame() %>% 
  mutate(age_group = case_when(age == "X0_19" ~ "0_24",
                               age == "X20_24" ~ "0_24",
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

lsoa_lookup <- fread(paste0(data_dir,"lsoa_2001_to_lsoa_2011.csv"),
                header = TRUE) %>% data.frame()

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

#Standardise

hrp_2001 <- recode(hrp_2001, lsoa_lookup)
hh_pop_2001 <- recode(hh_pop_2001, lsoa_lookup)


codes_not_in_lookup <- c("E01032522","E01032540","E01032608","E01033730")
hrp_2011 <- filter(hrp_2011, !gss_code %in% codes_not_in_lookup)
hh_pop_2011 <- filter(hh_pop_2011, !gss_code %in% codes_not_in_lookup)

#-------------------------------------------------------------------------------

lsoa_rates_2001 <- left_join(hh_pop_2001, hrp_2001, by = c("gss_code", "sex", "age_group")) %>% 
  mutate(HHR = ifelse(hh_pop == 0, 0, hrp/hh_pop),
         year = 2001) %>% 
  select(-hrp, -hh_pop)

lsoa_rates_2011 <- left_join(hh_pop_2011, hrp_2011, by = c("gss_code", "sex", "age_group")) %>% 
  mutate(HHR = ifelse(hh_pop == 0, 0, hrp/hh_pop),
         year = 2011) %>% 
  select(-hrp, -hh_pop)

#-------------------------------------------------------------------------------

lsoa_ward_lookup <- readRDS("input_data/lookup/2011_lsoa_to_ward.rds")

ward_rates_2001 <- left_join(hh_pop_2001, hrp_2001, by = c("gss_code", "sex", "age_group")) %>% 
  left_join(lsoa_ward_lookup, by=c("gss_code"="gss_code_lsoa")) %>% 
  group_by(gss_code_ward, sex, age_group) %>%
  summarise(hrp = sum(hrp),
            hh_pop = sum(hh_pop),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(HHR = ifelse(hh_pop == 0, 0, hrp/hh_pop),
         year = 2001) %>% 
  select(-hrp, -hh_pop)

ward_rates_2011 <- left_join(hh_pop_2011, hrp_2011, by = c("gss_code", "sex", "age_group")) %>% 
  left_join(lsoa_ward_lookup, by=c("gss_code"="gss_code_lsoa")) %>% 
  group_by(gss_code_ward, sex, age_group) %>%
  summarise(hrp = sum(hrp),
            hh_pop = sum(hh_pop),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(HHR = ifelse(hh_pop == 0, 0, hrp/hh_pop),
         year = 2001) %>% 
  select(-hrp, -hh_pop)

#-------------------------------------------------------------------------------

lsoa_rates <- vector("list", 2041)
lsoa_rates[[2001]] <- lsoa_rates_2001
lsoa_rates[[2011]] <- lsoa_rates_2011

# Project rates to 2021
# Modified 2-point exponential
for(i in 2001:2021){
  
  c <- 2011#ifelse(i < 2011, 2001, 2011) #most recent census
  d <- 2001#ifelse(i < 2011, 2011, 2001) #furthest away census
  
  yc <- lsoa_rates[[c]] %>% select(-year)
  yd <- lsoa_rates[[d]] %>% select(-year)
  
  lsoa_rates[[i]] <- left_join(yc, yd, by = c("gss_code", "sex", "age_group")) %>%
    setnames(c("gss_code", "sex", "age_group", "yc", "yd")) %>%
    mutate(k = ifelse(yc >= yd, 1, 0),
           a = yd -k,
           b = (yc-k)/(yd-k),
           x = (i-d)/(c-d),
           y = k + (a*(b^x)),
           year = i) %>%
    select(gss_code, year, sex, age_group, HRR = y)
}
rm(yc, yd, c, d, i)

#Hold rates constant 2022-2041
for(i in 2022:2041){
  lsoa_rates[[i]] <- lsoa_rates[[2021]] %>%
    mutate(year = i)
}

lsoa_rates <- data.table::rbindlist(lsoa_rates) %>%
  data.frame()

#-------------------------------------------------------------------------------

ward_rates <- vector("list", 2041)
ward_rates[[2001]] <- ward_rates_2001
ward_rates[[2011]] <- ward_rates_2011

# Project rates to 2021
# Modified 2-point exponential
for(i in 2001:2021){
  
  c <- 2011#ifelse(i < 2011, 2001, 2011) #most recent census
  d <- 2001#ifelse(i < 2011, 2011, 2001) #furthest away census
  
  yc <- ward_rates[[c]] %>% select(-year)
  yd <- ward_rates[[d]] %>% select(-year)
  
  ward_rates[[i]] <- left_join(yc, yd, by = c("gss_code_ward", "sex", "age_group")) %>%
    setnames(c("gss_code_ward", "sex", "age_group", "yc", "yd")) %>%
    mutate(k = ifelse(yc >= yd, 1, 0),
           a = yd -k,
           b = (yc-k)/(yd-k),
           x = (i-d)/(c-d),
           y = k + (a*(b^x)),
           year = i) %>%
    select(gss_code_ward, year, sex, age_group, HRR = y)
}
rm(yc, yd, c, d, i)

#Hold rates constant 2022-2041
for(i in 2022:2041){
  ward_rates[[i]] <- ward_rates[[2021]] %>%
    mutate(year = i)
}

ward_rates <- data.table::rbindlist(ward_rates) %>%
  data.frame()
#LOOK INTO MISSING VALUES IN GSS_CODE_WARD COLUMN

