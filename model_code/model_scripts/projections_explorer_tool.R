library(dplyr)
devtools::load_all("model_code/popmodules")

#####Projection Paths and Timestamps####

cen_proj_path <- "Q:/Teams/D&PA/Demography/Projections/2019_development/outputs/trend/2018/2018_central/"
cen_proj_timestamp <- "_19-11-13_2056"

long_proj_path <- "Q:/Teams/D&PA/Demography/Projections/2019_development/outputs/trend/2018/2018_long/"
long_proj_timestamp <- "_19-11-13_2144"

short_proj_path <- "Q:/Teams/D&PA/Demography/Projections/2019_development/outputs/trend/2018/2018_short/"
short_proj_timestamp <- "_19-11-13_2205"

housing_led_proj_path <- "M:/Projects/population_projections/outputs/housing_led/2018/shlaa_dev_20-01-31_1532/"


#------

####Functions####

names_lookup <- data.table::fread("M:/Projects/population_projections/input_data/lookup/lad18_code_to_name.csv")

process_projection <- function(x){
  nm <- last(names(x))
  
  y <- x %>%
    mutate(ethnic_group = "All persons",
           gss_code_ward = NA,
           ward_name = "Borough Total") %>%
    left_join(names_lookup, by="gss_code") %>%
    filter(substr(gss_code,1,3)=="E09") %>%
    filter(year %in% 2011:2050) %>%
    mutate(sex = substr(sex,1,1)) %>%
    select(gss_code, gss_code_ward,
           ethnic_group, year, sex, age, nm)
  
  return(y)
}


process_trend_projection <- function(proj_path, proj_timestamp, proj_name){
  
  proj <- list()
  
  proj[['pop']] <- readRDS(paste0(proj_path,"population",proj_timestamp, ".rds")) %>%
    rename(!!sym(paste0(proj_name,"_population")) := popn) %>%
    process_projection()
  
  proj[['bth']] <- readRDS(paste0(proj_path,"births",proj_timestamp, ".rds")) %>%
    rename(!!sym(paste0(proj_name,"_births")) := births)%>%
    process_projection()
  
  proj[['dth']] <- readRDS(paste0(proj_path,"deaths",proj_timestamp, ".rds")) %>%
    rename(!!sym(paste0(proj_name,"_deaths")) := deaths)%>%
    process_projection()
  
  proj[['di']] <- readRDS(paste0(proj_path,"dom_in",proj_timestamp, ".rds")) %>%
    rename(!!sym(paste0(proj_name,"_domestic_in")) := dom_in)%>%
    process_projection()
  
  proj[['do']] <- readRDS(paste0(proj_path,"dom_out",proj_timestamp, ".rds")) %>%
    rename(!!sym(paste0(proj_name,"_domestic_out")) := dom_out)%>%
    process_projection()
  
  proj[['ii']] <- readRDS(paste0(proj_path,"int_in",proj_timestamp, ".rds"))%>%
    rename(!!sym(paste0(proj_name,"_international_in")) := int_in)%>%
    process_projection()
  
  proj[['io']] <- readRDS(paste0(proj_path,"int_out",proj_timestamp, ".rds"))%>%
    rename(!!sym(paste0(proj_name,"_international_out")) := int_out) %>%
    process_projection()
  
  proj[['net']] <- rbind(
    rename(proj[['di']], x  = !!paste0(proj_name,"_domestic_in")),
    rename(proj[['do']], x = !!paste0(proj_name,"_domestic_out")) %>% mutate(x = x*-1),
    rename(proj[['ii']], x  = !!paste0(proj_name,"_international_in")),
    rename(proj[['io']], x = !!paste0(proj_name,"_international_out")) %>% mutate(x = x*-1)
  )
  
  proj[['net']] <- proj[['net']] %>%
    group_by(gss_code, gss_code_ward, ethnic_group, year, sex, age) %>%
    summarise(!!sym(paste0(proj_name,"_net_migration")) := sum(x)) %>%
    as.data.frame()
  
  return(proj)
  
}

ward_name_lookup <- readRDS("M:/Projects/population_projections/input_data/lookup/2011_ward_to_district.rds")

wrd_func <- function(data, yrs=c(2011:2050)){
  nm <- last(names(data))
  
  x <- data %>%
    left_join(ward_name_lookup, by=c("gss_code","gss_code_ward")) %>%
    left_join(names_lookup, by="gss_code") %>%
    mutate(ethnic_group = "All persons") %>%
    select(gss_code, gss_code_ward,
           ethnic_group, year, sex, age, nm) %>%
    mutate(sex = substr(sex,1,1)) %>%
    filter(gss_code != "E09000001") %>%
    filter(year %in% yrs)
  
  return(x)
  
}

#-----

####Processing####

central <- process_trend_projection(cen_proj_path, cen_proj_timestamp, proj_name="central")
long <- process_trend_projection(long_proj_path, long_proj_timestamp, proj_name="long")
short <- process_trend_projection(short_proj_path, short_proj_timestamp, proj_name="short")
housing_led <- process_trend_projection(housing_led_proj_path, proj_timestamp = "", proj_name="housing_led")

#----

####Wards####

ward_path <- paste0(housing_led_proj_path,"ward/")

ward_pop <- readRDS(paste0(ward_path,"population_ward.rds")) %>%
  wrd_func(yrs=c(2010:2050)) %>%
  rename(housing_led_population = popn)

ward_past_births <- readRDS("M:/Projects/population_projections/input_data/small_area_model/ward_births.rds") %>%
  group_by(year, gss_code_ward) %>%
  summarise(male = sum(births)*(105/205),
            female = sum(births)*(100/205)) %>%
  as.data.frame() %>%
  left_join(ward_name_lookup, by="gss_code_ward") %>%
  mutate(ethnic_group = "All persons",
         age = 0) %>%
  tidyr::pivot_longer(c("male","female"), names_to = "sex", values_to = "housing_led_births") %>%
  mutate(sex = substr(sex,1,1)) %>%
  filter(gss_code != "E09000001") %>%
  filter(year %in% 2011:2018) %>%
  select(gss_code, gss_code_ward, ethnic_group, year, sex, age, housing_led_births)

ward_bth <- readRDS(paste0(ward_path,"births_ward.rds")) %>%
  wrd_func() %>%
  rename(housing_led_births = births) %>%
  rbind(ward_past_births)

past_ward_deaths <- readRDS("M:/Projects/population_projections/input_data/small_area_model/ward_deaths.rds") %>%
  filter(year %in% 2011:2018) %>%
  mutate(min = substr(age_group,1,2),
         min = ifelse(min == "1_", "1",
                      ifelse(min == "5_", "5",
                             min)),
         min = as.numeric(min)) %>%
  mutate(max = case_when(min == 0 ~ 0,
                         min == 1 ~ 4,
                         min == 85 ~ 90,
                         TRUE ~ min + 4)) %>%
  left_join(ward_name_lookup) %>%
  select(-ward_name)

past_borough_deaths <- central[['dth']] %>%
  filter(year %in% 2011:2018) %>%
  select(gss_code, year, sex, age, central_deaths) %>%
  mutate(sex = ifelse(sex == "m", "male", "female"))

past_deaths <- list()
for(i in unique(past_ward_deaths$age_group)){
  
  a <- filter(past_ward_deaths, age_group ==i)
  past_deaths[[i]] <- distribute_within_age_band(a, past_borough_deaths, "deaths", "central_deaths",
                                                 unique(a$min), unique(a$max),
                                                 col_aggregation=c("year","gss_code","sex"))
}

past_deaths <- data.table::rbindlist(past_deaths) %>%
  as.data.frame() %>%
  mutate(sex = substr(sex,1,1),
         ethnic_group = "All persons") %>%
  rename(housing_led_deaths = deaths) %>%
  select(gss_code, gss_code_ward, ethnic_group, year, sex, age, housing_led_deaths) %>%
  filter(gss_code != "E09000001")

ward_dth <- readRDS(paste0(ward_path,"deaths_ward.rds")) %>%
  wrd_func() %>%
  rename(housing_led_deaths = deaths) %>%
  rbind(past_deaths)

past_net <- ward_pop %>%
  rename(popn = housing_led_population) %>%
  popn_age_on(col_aggregation = c("year", "gss_code_ward", "age", "sex"),
              births = rename(ward_bth, births = housing_led_births)) %>%
  filter(year %in% 2011:2018) %>%
  left_join(ward_dth, by = c("gss_code", "gss_code_ward", "ethnic_group", "year", "sex", "age")) %>%
  mutate(nat_chg = popn - housing_led_deaths) %>%
  left_join(ward_pop, by = c("gss_code", "gss_code_ward", "ethnic_group", "year", "sex", "age")) %>%
  mutate(housing_led_net_migration = housing_led_population - nat_chg) %>%
  select(gss_code, gss_code_ward, ethnic_group, year, sex, age, housing_led_net_migration)

ward_net <- readRDS(paste0(ward_path,"migration_ward.rds")) %>%
  wrd_func() %>%
  rename(housing_led_net_migration = migration) %>%
  rbind(past_net)

ward_pop <- filter(ward_pop, year >= 2011)


#----
housing_led[['pop']]<- rbind(housing_led[['pop']], ward_pop)
housing_led[['bth']] <- rbind(housing_led[['bth']], ward_bth)
housing_led[['dth']] <- rbind(housing_led[['dth']], ward_dth)
housing_led[['net']] <- rbind(housing_led[['net']], ward_net)

#----

####OUTPUT####

df_list <- list(central[['pop']], short[['pop']], long[['pop']], housing_led[['pop']],
                central[['bth']], short[['bth']], long[['bth']], housing_led[['bth']],
                central[['dth']], short[['dth']], long[['dth']], housing_led[['dth']],
                central[['di']], short[['di']], long[['di']], housing_led[['di']],
                central[['do']], short[['do']], long[['do']], housing_led[['do']],
                central[['ii']], short[['ii']], long[['ii']], housing_led[['ii']],
                central[['io']], short[['io']], long[['io']], housing_led[['io']],
                central[['net']], short[['net']], long[['net']], housing_led[['net']])

x <- list()
for(i in 1:length(df_list)){
  x[[i]] <- df_list[[i]] %>%
    tidyr::pivot_longer(cols = ncol(df_list[[i]]), values_to = "value", names_to ="var") %>%
    mutate(value = round(value, digits = 2))
  
}

pop_w_everything <- data.table::rbindlist(x) %>%
  tidyr::pivot_wider(names_from = var, values_from = value)

#This is temporary until the housing-led model is fixed
# no_negs <- select(pop_w_everything, -central_net_migration, -long_net_migration,
#                   -short_net_migration, -housing_led_net_migration)
# 
# no_negs[no_negs < 0] <- 0
# 
# pop_w_everything <- cbind(no_negs, select(pop_w_everything, central_net_migration, long_net_migration,
#                                           short_net_migration, housing_led_net_migration)) %>%
#   select(names(pop_w_everything))
####
sum(is.na(pop_w_everything))
pop_w_everything[is.na(pop_w_everything)] <- ""
sum(is.na(pop_w_everything))

data.table::fwrite(pop_w_everything, "Q:/Teams/D&PA/Demography/Projections/temp/example_files_for_mike/projections_output_for_explorer_tool.csv")
