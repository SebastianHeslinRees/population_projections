#Process 2021-based ward projections for the ward profiles
library(dplyr)
library(data.table)

#Projections
locations <- c("outputs/flexible_area_model/2021_based/2021_Identified_Capacity_10yr/",
               "outputs/flexible_area_model/2021_based/2021_Past_Delivery_10yr/",
               "outputs/flexible_area_model/2021_based/2021_Housing_Targets_10yr/")

scenarios <- c("Identified Capacity", "Past Delivery", "Housing Targets")

#read data
components <- c("population",
                "births",
                "deaths",
                "net_migration",
                "in_migration",
                "out_migration")

scenario_data <- list()
scenario_data[[1]] <- list() 
scenario_data[[2]] <- list()
scenario_data[[3]] <- list()

#-------------------------------------------------------------------------------

for(i in components){
  
  for(j in 1:3){
    
    message(paste(i, scenarios[j], sep = " - "))
    
    scenario_data[[i]][[j]] <- readRDS(paste0(locations[j],i,".rds")) %>% 
      rename(value = last(names(.)))   %>% 
      mutate(variant = scenarios[j],
             component = i)
  }
  scenario_data[[i]] <- rbindlist(scenario_data[[i]])
}

all_data <-  rbindlist(scenario_data) %>%
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt() %>% 
  filter(year <= 2041) %>% 
  mutate(component = stringr::str_replace_all(component, "_", " "))

#-------------------------------------------------------------------------------

rm(list= setdiff(ls(), c("all_data","locations","scenarios")))

ward_to_district <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds")

#-------------------------------------------------------------------------------
#Development
message('development & ahs')

assumed_dev <- list()
borough_hh <- list()
borough_hh_pop <- list()
b_ahs <- list()
ward_hh <- list()
ahs <- list()

for(i in 1:3){
  assumed_dev[[i]] <- readRDS(paste0(locations[i],"dwelling_stock.rds")) %>% 
    mutate(variant = scenarios[i])
  
  borough_hh[[i]] <- readRDS(paste0(locations[i], "borough_data.households_detail.rds")) %>% 
    mutate(variant = scenarios[i])
  
  ward_hh[[i]] <- readRDS(paste0(locations[i], "households_detail.rds")) %>% 
    mutate(variant = scenarios[i]) 
  
  ahs[[i]] <- readRDS(paste0(locations[i], "ahs_detail.rds")) %>% 
    mutate(variant = scenarios[i])%>% 
    select(gss_code, la_name, gss_code_ward, ward_name, year, variant, ahs = actual_ahs) %>% 
    filter(year >= 2011)
}

assumed_dev <- rbindlist(assumed_dev) %>% 
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt()

borough_hh_data <- rbindlist(borough_hh) %>% 
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt()

ward_hh_data <- rbindlist(ward_hh) %>% 
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt()

ahs <- rbindlist(ahs) %>% 
  mutate(variant = factor(variant, levels = scenarios)) %>% 
  dtplyr::lazy_dt()

#-------------------------------------------------------------------------------

household_popn <- ward_hh_data %>% 
  select(gss_code_ward, year, variant, hh_pop = household_popn) %>% 
  filter(year >= 2011)

borough_ahs <- borough_hh_data %>% 
  select(gss_code, year, variant,
         hh_pop = household_population,
         hh = input_households,
         ahs = implied_ahs) %>% 
  filter(year == 2021) %>% 
  data.frame()

london_ahs <- ward_hh_data %>% 
  dtplyr::lazy_dt() %>% 
  group_by(year, variant) %>% 
  summarise(hh_pop = sum(household_popn),
            hh = sum(input_households),
            .groups = 'drop_last') %>% 
  mutate(ahs = hh_pop/hh) %>% 
  data.frame()

#-------------------------------------------------------------------------------
#Ward areas (hectares)
ward_shp <- "W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2022/ESRI/London/London_Ward_Clipped_to_MHW.shp"

ward_areas <- raster::shapefile(ward_shp)
ward_areas$hectares <- raster::area(ward_areas) / 10000
ward_areas <- data.frame(ward_areas)
ward_areas <- ward_areas %>% rename(gss_code = LAD22CD)

#-------------------------------------------------------------------------------

output_list <- list()

#London growth
london_population <- all_data %>% 
  filter(substr(gss_code,1,3) == "E09",
         component == "population") %>% 
  group_by(year, variant) %>% 
  summarise(value = sum(value),
            .groups = 'drop_last') %>% 
  data.frame()

london_2021 <- unique(filter(london_population, year == 2021)$value)
london_2011 <- unique(filter(london_population, year == 2011)$value)
london_growth <- london_2021 - london_2011

#-------------------------------------------------------------------------------

#London variables

output_list$london$london_growth_percent <- (london_growth / london_2011)*100
output_list$london$london_ahs <- unique(filter(london_ahs, year == 2021)$ahs)

#-------------------------------------------------------------------------------

filter_component <- function(all_data, cmpnt){
  
  all_data %>% 
    filter(component == cmpnt) %>% 
    group_by(gss_code_ward, year, variant) %>% 
    summarise(value = sum(value),
              .groups = 'drop_last') %>% 
    data.frame() %>% 
    dtplyr::lazy_dt()
  
}

a <- c("population", "births", "deaths", "net migration",
       "in migration", "out migration") %>% 
  lapply(function(x){
    filter_component(all_data, x)
  })

names(a) <- c("total_population", "total_births", "total_deaths",
              "net_migration", "in_migration", "out_migration")

a$age_groups <- all_data %>% 
  filter(component == "population",
         year %in% c(2021, 2031)) %>% 
  mutate(age_group = cut(age,
                         c(-1,3,10,15,17,64,Inf),
                         c("3 and under","4 to 10","11 to 15",
                           "16 to 17", "18 to 64", "65 and over"))) %>%
  group_by(gss_code_ward, year, age_group, variant) %>% 
  summarise(value = sum(value),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  dtplyr::lazy_dt()

a$age_structure <- all_data %>% 
  filter(component == "population",
         year %in% c(2011, 2021, 2031)) %>% 
  group_by(gss_code_ward, year, age, variant) %>% 
  summarise(value = sum(value),
            .groups = 'drop_last') %>% 
  data.frame() %>% 
  mutate(year = as.character(year)) %>% 
  dtplyr::lazy_dt()

#-------------------------------------------------------------------------------

# borough data

all_borough_popn <- filter(all_data, component == "population") %>% 
  group_by(year, gss_code, variant) %>% 
  summarise(value = sum(value),
            .groups = 'drop_last') %>% 
  data.frame()

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
  
  x$net_migration <- a$net_migration %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$in_migration <- a$in_migration %>% 
    filter(gss_code_ward == ward_code) %>% 
    data.frame()
  
  x$out_migration <- a$out_migration %>% 
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
  x$popn_2021 <- unique(filter(x$total_population, year == 2021)$value)
  x$popn_2011 <- unique(filter(x$total_population, year == 2011)$value)
  x$popn_2031_s1 <- filter(x$total_population,
                           year == 2031, variant == scenarios[1])$value
  x$popn_2031_s2 <- filter(x$total_population,
                           year == 2031, variant == scenarios[2])$value
  x$popn_2031_s3 <- filter(x$total_population,
                           year == 2031, variant == scenarios[3])$value
  x$growth <- x$popn_2021 - x$popn_2011
  x$growth_percent <- (x$growth / x$popn_2011)*100
  
  if(new_borough){
    
    borough_name <- filter(ward_to_district, gss_code_ward == ward_code)$la_name
    
    borough_popn <- filter(all_borough_popn, gss_code == borough_code) %>% 
      select(year, variant, value)
    
    borough_2021 <- unique(filter(borough_popn, year == 2021)$value)
    borough_2011 <- unique(filter(borough_popn, year == 2011)$value)
    
    b_ahs <- filter(borough_ahs, gss_code == borough_code)$ahs %>% unique()
    
  }
  
  x$borough_growth <- borough_2021 - borough_2011
  x$borough_growth_percent <- (x$borough_growth / borough_2011)*100
  
  x$future_growth_s1 <- x$popn_2031_s1 - x$popn_2021
  x$future_growth_s2 <- x$popn_2031_s2 - x$popn_2021
  x$future_growth_s3 <- x$popn_2031_s3 - x$popn_2021
  
  future <- c(x$future_growth_s1, x$future_growth_s2, x$future_growth_s3)
  
  x$future_growth_min <- min(future)
  x$future_growth_max <- max(future)
  x$ward_code <- ward_code
  x$borough_name <- borough_name
  x$borough_ahs <- b_ahs
  x$ward_area <- filter(ward_areas, WD22CD == ward_code)$hectares
  
  last_borough <- borough_code
  
  #-------------------------------------------------------------------------------
  output_list[[ward_code]] <- x
}

dir.create('outputs/markdown', showWarnings = FALSE)
saveRDS(output_list, 'outputs/markdown/2021_ward_profile_data.rds')
