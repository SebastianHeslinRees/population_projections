#Process 2020-based ward projections for the ward profiles
library(dplyr)
library(data.table)

#Projections
locations <- c("outputs/housing_led/2020/Identified_Capacity_21-09-22_1121/ward/",
               "outputs/housing_led/2020/Past_Delivery_21-09-22_1121/ward/",
               "outputs/housing_led/2020/Housing_Targets_21-09-22_1121/ward/")

scenarios <- c("Identified Capacity", "Past Delivery", "Housing Targets")

#read data
components <- c("population","births","deaths","migration")
scenario_1 <- list()
scenario_2 <- list()
scenario_3 <- list()

#-------------------------------------------------------------------------------

for(i in components){
  message(i)
  
  scenario_1[[i]] <- readRDS(paste0(locations[1],i,"_ward.rds")) %>% 
    rename(value = last(names(.)))   %>% 
    mutate(variant = scenarios[1],
           component = i)
  
  scenario_2[[i]] <- readRDS(paste0(locations[2],i,"_ward.rds"))  %>% 
    rename(value = last(names(.)))   %>% 
    mutate(variant = scenarios[2],
           component = i)
  
  scenario_3[[i]] <- readRDS(paste0(locations[3],i,"_ward.rds"))  %>% 
    rename(value = last(names(.)))   %>% 
    mutate(variant = scenarios[3],
           component = i)
}

all_data <-  rbindlist(list(rbindlist(scenario_1),
                            rbindlist(scenario_2),
                            rbindlist(scenario_3))) %>%
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  filter(year <= 2041) %>% 
  dtplyr::lazy_dt()

#-------------------------------------------------------------------------------

rm(list= setdiff(ls(), c("all_data","locations","scenarios")))

ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>% 
  left_join(readRDS("input_data/lookup/gss_code_to_name.rds"), by="gss_code")

#-------------------------------------------------------------------------------
#Development
message('development & ahs')

assumed_dev <- list()
borough_hh <- list()
borough_hh_pop <- list()
b_ahs <- list()

for(i in 1:3){
  assumed_dev[[i]] <- readRDS(paste0(locations[i],"total_stock_ward.rds")) %>% 
    mutate(variant = scenarios[i])
  
  borough_hh[[i]] <- readRDS(paste0(
    substr(locations[i],1,nchar(locations[i])-5),
    "household_trajectory.rds")) %>% 
    mutate(variant = scenarios[i])
  
  b_ahs[[i]] <- readRDS(paste0(
    substr(locations[i],1,nchar(locations[i])-5),
    "ahs.rds")) %>%
    mutate(variant = scenarios[i])
  
  borough_hh_pop[[i]] <- readRDS(paste0(
    substr(locations[i],1,nchar(locations[i])-5),
    "household_population.rds")) %>% 
    mutate(variant = scenarios[i])
}

assumed_dev <- rbindlist(assumed_dev) %>% 
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt()

borough_hh <- rbindlist(borough_hh) %>% 
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt()

#HH to Dwellings ratio
borough_dwellings <- assumed_dev %>% 
  group_by(gss_code, year, variant) %>% 
  summarise(dwellings = sum(units))

ratio <- left_join(borough_hh, borough_dwellings, by=c("gss_code","year","variant")) %>% 
  mutate(ratio = dwellings/households) %>% 
  select(gss_code, year, variant, ratio)

#-------------------------------------------------------------------------------
#AHS
communal_est <- readRDS("input_data/small_area_model/ward_data/ward_communal_establishment_population.rds") %>% 
  dtplyr::lazy_dt()

household_popn <- all_data %>%
  filter(component == "population") %>%
  left_join(communal_est, by=c("gss_code_ward","sex","age")) %>% 
  group_by(gss_code, gss_code_ward, year, variant) %>% 
  summarise(hh_pop = sum(value)-sum(ce_popn)) %>% 
  group_by(gss_code, year, variant) %>% 
  mutate(b_hh_pop = sum(hh_pop)) %>% 
  left_join(rbindlist(borough_hh_pop), by = c("gss_code", "year", "variant")) %>% 
  mutate(scaling = household_popn/b_hh_pop,
         hh_pop = ifelse(is.na(household_popn), hh_pop, hh_pop * scaling)) %>% 
  select(gss_code_ward, year, variant, hh_pop)

ahs <- household_popn %>% 
  left_join(assumed_dev, by=c("gss_code_ward","year","variant")) %>%
  left_join(ratio, by = c("gss_code", "year", "variant")) %>% 
  mutate(hh = units/ratio) %>% 
  mutate(ahs = hh_pop/hh) %>% 
  filter(year <= 2041 & !is.na(ahs)) %>%
  data.frame() 

borough_ahs <- ahs %>% 
  dtplyr::lazy_dt() %>% 
  group_by(gss_code, year, variant) %>% 
  summarise(hh_pop = sum(hh_pop),
            hh = sum(hh)) %>% 
  mutate(ahs = hh_pop/hh) %>% 
  data.frame()

checks <- borough_ahs %>% 
  left_join(rbindlist(b_ahs), by=c("gss_code", "year", "variant")) %>% 
  left_join(rbindlist(borough_hh_pop), by = c("gss_code", "year", "variant")) %>% 
  left_join(data.frame(borough_hh), by = c("gss_code", "year", "variant")) %>% 
  rename(input_hh_pop = household_popn,
         input_ahs = ahs.y,
         input_hh = households) %>% 
  select(gss_code, year, variant, hh, input_hh, hh_pop, input_hh_pop, ahs = ahs.x, input_ahs)

london_ahs <- ahs %>% 
  dtplyr::lazy_dt() %>% 
  group_by(year, variant) %>% 
  summarise(hh_pop = sum(hh_pop),
            units = sum(units)) %>% 
  mutate(ahs = hh_pop/units) %>% 
  data.frame()

#-------------------------------------------------------------------------------
#Ward areas (hectares)

ward_areas <- foreign::read.dbf("W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2011/ESRI/London/London_Ward.dbf") %>% 
  select(gss_code = GSS_CODE, hectares = HECTARES)

#-------------------------------------------------------------------------------

output_list <- list()

#London growth
london_population <- all_data %>% 
  filter(substr(gss_code,1,3) == "E09",
         component == "population") %>% 
  group_by(year, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame()

london_2020 <- unique(filter(london_population, year == 2020)$value)
london_2011 <- unique(filter(london_population, year == 2011)$value)
london_growth <- london_2020 - london_2011

#-------------------------------------------------------------------------------

#London variables

output_list$london$london_growth_percent <- (london_growth / london_2011)*100
output_list$london$london_ahs <- unique(filter(london_ahs, year == 2020)$ahs)

#-------------------------------------------------------------------------------

a <- list()

a$total_population <- all_data %>% 
  filter(component == "population") %>% 
  group_by(gss_code_ward, year, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame() %>% 
  dtplyr::lazy_dt()

a$total_births <- all_data %>% 
  filter(component == "births") %>% 
  group_by(gss_code_ward, year, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame() %>% 
  dtplyr::lazy_dt()

a$total_deaths <- all_data %>% 
  filter(component == "deaths") %>% 
  group_by(gss_code_ward, year, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame() %>% 
  dtplyr::lazy_dt()

a$total_migration <- all_data %>% 
  filter(component == "migration") %>% 
  group_by(gss_code_ward, year, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame() %>% 
  dtplyr::lazy_dt()

a$age_groups <- all_data %>% 
  filter(component == "population",
         year %in% c(2020, 2030)) %>% 
  mutate(age_group = cut(age,
                         c(-1,3,10,15,17,64,Inf),
                         c("3 and under","4 to 10","11 to 15",
                           "16 to 17", "18 to 64", "65 and over"))) %>%
  group_by(gss_code_ward, year, age_group, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame() %>% 
  dtplyr::lazy_dt()

a$age_structure <- all_data %>% 
  filter(component == "population",
         year %in% c(2011, 2020, 2030)) %>% 
  group_by(gss_code_ward, year, age, variant) %>% 
  summarise(value = sum(value)) %>% 
  data.frame() %>% 
  mutate(year = as.character(year)) %>% 
  dtplyr::lazy_dt()

#-------------------------------------------------------------------------------

all_ward_codes <- unique(data.frame(all_data)$gss_code_ward)
last_borough <- "x"
i <- 0

#Loop from here
for(ward_code in all_ward_codes){
  i <- i+1
  print(paste0(i, ": ", ward_code))
  x <- list()
  x$ward_code <- ward_code
  borough_code <- filter(ward_to_district, gss_code_ward == ward_code)$gss_code
  new_borough <- ifelse(borough_code == last_borough, FALSE, TRUE)
  
#-------------------------------------------------------------------------------
  
  #Break it up
  x$total_population <- a$total_population %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$total_births <- a$total_births %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame() 
  
  x$total_deaths <- a$total_deaths %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$total_migration <- a$total_migration %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$population_change <- x$total_population %>% 
    arrange(variant, year) %>% 
    mutate(change = value - lag(value),
           percentage = (change / lag(value))*100) %>% 
    filter(year != min(year)) %>% 
    select(-value) %>% 
    data.frame()
  
  x$age_groups <- a$age_groups %>% 
    filter(gss_code_ward == ward_code) %>% 
    select(-gss_code_ward) %>% 
    data.frame()
  
  x$age_structure <- a$age_structure %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$ward_ahs <- ahs %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$ward_name <- filter(ward_to_district, gss_code_ward == ward_code)$ward_name
  x$popn_2020 <- unique(filter(x$total_population, year == 2020)$value)
  x$popn_2011 <- unique(filter(x$total_population, year == 2011)$value)
  x$popn_2030_s1 <- filter(x$total_population,
                           year == 2030, variant == scenarios[1])$value
  x$popn_2030_s2 <- filter(x$total_population,
                           year == 2030, variant == scenarios[2])$value
  x$popn_2030_s3 <- filter(x$total_population,
                           year == 2030, variant == scenarios[3])$value
  x$growth <- x$popn_2020 - x$popn_2011
  x$growth_percent <- (x$growth / x$popn_2011)*100
  
  if(new_borough){
    
    borough_name <-filter(ward_to_district, gss_code_ward == ward_code)$gss_name
    
    borough_popn <- filter(all_data, gss_code == borough_code,
                           component == "population") %>% 
      group_by(year, variant) %>% 
      summarise(value = sum(value)) %>% 
      data.frame()
    
    borough_2020 <- unique(filter(borough_popn, year == 2020)$value)
    borough_2011 <- unique(filter(borough_popn, year == 2011)$value)
    
  }
  x$borough_growth <- borough_2020 - borough_2011
  x$borough_growth_percent <- (x$borough_growth / borough_2011)*100
  
  x$future_growth_s1 <- x$popn_2030_s1 - x$popn_2020
  x$future_growth_s2 <- x$popn_2030_s2 - x$popn_2020
  x$future_growth_s3 <- x$popn_2030_s3 - x$popn_2020
  
  future <- c(x$future_growth_s1, x$future_growth_s2, x$future_growth_s3)
  
  x$future_growth_min <- min(future)
  x$future_growth_max <- max(future)
  x$ward_code <- ward_code
  x$borough_name <- borough_name
  x$borough_ahs <- filter(borough_ahs, gss_code == borough_code, year == 2020)$ahs %>% unique()
  x$ward_area <- filter(ward_areas, gss_code == ward_code)$hectares
  
  last_borough <- borough_code
  
#-------------------------------------------------------------------------------
  output_list[[ward_code]] <- x
}

dir.create('outputs/markdown', showWarnings = FALSE)
saveRDS(output_list, 'outputs/markdown/ward_profile_data.rds')
