#revised GLA estimates of population & international flows
#data created in a separate process
#this script calls on the processed data
#data created by BC January 2023

library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(popmodules)

message("2021 GLA MYE series")


f_paths <- list(gla_series = "E:/project_folders/demography/ben/R_projects/create_modelled_backseries/outputs/modelled_backseries.rds",
                region_lookup = "input_data/lookup/district_to_region_(2021 geog).rds",
                scotland_pop = "Q:/Teams/D&PA/Data/population_estimates/nrs_nisra_estimates/scotland/mye_2021/scotalnd_sya_population_2021.csv",
                nireland_pop = "Q:/Teams/D&PA/Data/population_estimates/nrs_nisra_estimates/northern_ireland/mye_2021/N_IRELAND_MYE21_SYA.csv",
                output_dir = "input_data/mye/2021/",
                previous_dir = "input_data/mye/2020/",
                dom_matrix_past = "input_data/domestic_migration/2020/domestic_migration_flows_ons_(2021_geog).rds",
                dom_matrix_2021 = "E:/project_folders/demography/ben/R_projects/create_modelled_backseries/outputs/modelled_od_flows.rds",
                dom_matrix_out = "input_data/domestic_migration/2021/domestic_migration_flows_ons_(2021_geog).rds")


#-----

dir.create(f_paths$output_dir, showWarnings = FALSE)
gla_series <- readRDS(f_paths$gla_series)
england_wales_codes <- c("E06", "E07", "E08", "E09", "W06")

#------

for(i in unique(gla_series$component)){
  if(!stringr::str_detect(i, "net")){
    print(paste(i, nrow(filter(gla_series, component == i, value < 0))))
  }
}

filter(gla_series, component == "population", value <0)

#------

# ENGLAND & WALES LAD data 2002 - 2011
eng_wales_past <- function(x, yr = 2012, codes = england_wales_codes){
  filter(x, year < yr, substr(gss_code,1,3) %in% codes)
}

eng_wales_popn_past <- readRDS(paste0(f_paths$previous_dir, "population_ons.rds")) %>% eng_wales_past(2011)
eng_wales_births_past <- readRDS(paste0(f_paths$previous_dir, "births_ons.rds")) %>% eng_wales_past()
eng_wales_deaths_past <- readRDS(paste0(f_paths$previous_dir, "deaths_ons.rds")) %>% eng_wales_past()
eng_wales_int_in_past <- readRDS(paste0(f_paths$previous_dir, "int_in_ons.rds")) %>% eng_wales_past()
eng_wales_int_out_past <- readRDS(paste0(f_paths$previous_dir, "int_out_ons.rds")) %>% eng_wales_past()
eng_wales_dom_in_past <- readRDS(paste0(f_paths$previous_dir, "dom_in_ons.rds")) %>% eng_wales_past()
eng_wales_dom_out_past <- readRDS(paste0(f_paths$previous_dir, "dom_out_ons.rds")) %>% eng_wales_past()

#eng_wales_int_net <- readRDS(paste0(f_paths$previous_dir, "int_net_ons.rds")) %>% eng_wales_past()
#eng_wales_dom_net <- readRDS(paste0(f_paths$previous_dir, "dom_net_ons.rds")) %>% eng_wales_past()

#-----

# N Ireland, Scotland, Wales data 2002 - 2020

NI_Sc_past <- function(x){
  x %>% filter(substr(gss_code,1,1) %in% c("N","S"))
}

other_popn_past <- readRDS(paste0(f_paths$previous_dir, "population_ons.rds")) %>% NI_Sc_past()
other_births_past <- readRDS(paste0(f_paths$previous_dir, "births_ons.rds")) %>% NI_Sc_past()
other_deaths_past <- readRDS(paste0(f_paths$previous_dir, "deaths_ons.rds")) %>% NI_Sc_past()
other_int_in_past <- readRDS(paste0(f_paths$previous_dir, "int_in_ons.rds")) %>% NI_Sc_past()
other_int_out_past <- readRDS(paste0(f_paths$previous_dir, "int_out_ons.rds")) %>% NI_Sc_past()
other_dom_in_past <- readRDS(paste0(f_paths$previous_dir, "dom_in_ons.rds")) %>% NI_Sc_past()
other_dom_out_past <- readRDS(paste0(f_paths$previous_dir, "dom_out_ons.rds")) %>% NI_Sc_past()

#-------------------------------------------------------------------------------

#### 2021 Populations and components

#-------------------------------------------------------------------------------

# Scotland: 2021 Population
# From NRS 2021 MYE

scotland_2021_pop <- fread(f_paths$scotland_pop, header = TRUE) %>% 
  data.frame() %>% 
  pivot_longer(cols = starts_with("X"), names_to = "age", values_to = "popn") %>% 
  mutate(age = str_replace_all(age, "X", ""),
         age = as.numeric(age),
         year = 2021) %>% 
  filter(Sex != "persons",
         Area.code == "S92000003") %>% 
  rename(sex = Sex,
         gss_code = Area.code) %>%
  select(names(eng_wales_popn_past))

# NRS Components are totals only
# Hard-coded here from NRS outputs
scotland_births_total_2021 <- 46734	
scotland_deaths_total_2021 <- 61280
scotland_dom_in_total_2021 <- 56200
scotland_int_in_total_2021 <- 41000
scotland_dom_out_total_2021 <- 47300
scotland_int_out_total_2021 <- 22100

dist_components <- function(x, area, component, value_2021, cols){
  
  gss <- case_when(area == "sc" ~ "S92000003",
                   area == "ni" ~ "N92000002",
                   TRUE ~ "ERROR")
  
  x %>% 
    rename(value := component) %>% 
    filter(year == 2020, gss_code == gss) %>% 
    mutate(share = value/sum(value),
           value = share * value_2021) %>% 
    mutate(year = 2021) %>% 
    rename(!!component := value) %>% 
    select(!!cols)
}

scotland_births_2021 <- data.frame(gss_code = "S92000003",
                                   age = 0,
                                   sex = c("female","male"),
                                   year = 2021,
                                   births = c(scotland_births_total_2021 * (100/205),
                                              scotland_births_total_2021 * (105/205))) %>% 
  complete_popn_dataframe(col_data = "births") %>% 
  select(names(eng_wales_births_past))

scotland_deaths_2021 <- dist_components(other_deaths_past, "sc", "deaths",
                                        scotland_deaths_total_2021,
                                        names(eng_wales_deaths_past))

scotland_int_in_2021 <- dist_components(other_int_in_past, "sc", "int_in",
                                        scotland_int_in_total_2021,
                                        names(eng_wales_int_in_past))

scotland_int_out_2021 <- dist_components(other_int_out_past, "sc", "int_out",
                                         scotland_int_out_total_2021,
                                         names(eng_wales_int_out_past))

scotland_dom_in_2021 <- dist_components(other_dom_in_past, "sc", "dom_in",
                                        scotland_dom_in_total_2021,
                                        names(eng_wales_dom_in_past))

scotland_dom_out_2021 <- dist_components(other_dom_out_past, "sc", "dom_out",
                                         scotland_dom_out_total_2021,
                                         names(eng_wales_dom_out_past))


#-----------------

# Norther Ireland 2021

n_ireland_2021_pop <- fread(f_paths$nireland_pop, header = TRUE) %>% 
  data.frame() %>% 
  filter(area_code == "N92000002",
         sex != "All persons") %>% 
  rename(gss_code = area_code,
         popn = MYE) %>% 
  select(names(eng_wales_popn_past)) %>% 
  mutate(sex = tolower(sex),
         sex = str_replace_all(sex, "males", "male"))

# NISRA Components are totals only
# Hard-coded here from NRS outputs

n_ireland_births_total_2021 <- 5783	
n_ireland_deaths_total_2021 <- 61280
n_ireland_dom_in_total_2021 <- 3765
n_ireland_int_in_total_2021 <- 3741
n_ireland_dom_out_total_2021 <- 4966
n_ireland_int_out_total_2021 <- 2903

n_ireland_births_2021 <- data.frame(gss_code = "N92000002",
                                    age = 0,
                                    sex = c("female","male"),
                                    year = 2021,
                                    births = c(n_ireland_births_total_2021 * (100/205),
                                               n_ireland_births_total_2021 * (105/205))) %>% 
  complete_popn_dataframe(col_data = "births") %>% 
  select(names(eng_wales_births_past))

n_ireland_deaths_2021 <- dist_components(other_deaths_past, "ni", "deaths",
                                         n_ireland_deaths_total_2021,
                                         names(eng_wales_deaths_past))

n_ireland_int_in_2021 <- dist_components(other_int_in_past, "ni", "int_in",
                                         n_ireland_int_in_total_2021,
                                         names(eng_wales_int_in_past))

n_ireland_int_out_2021 <- dist_components(other_int_out_past, "ni", "int_out",
                                          n_ireland_int_out_total_2021,
                                          names(eng_wales_int_out_past))

n_ireland_dom_in_2021 <- dist_components(other_dom_in_past, "ni", "dom_in",
                                         n_ireland_dom_in_total_2021,
                                         names(eng_wales_dom_in_past))

n_ireland_dom_out_2021 <- dist_components(other_dom_out_past, "ni", "dom_out",
                                          n_ireland_dom_out_total_2021,
                                          names(eng_wales_dom_out_past))

#-----------------

# split into individual components files
final_popn <- filter(gla_series, component == "population") %>% 
  mutate(value = ifelse(value < 0, 0, value)) %>% 
  select(gss_code, year, sex, age, popn = value) %>% 
  data.frame()  %>% 
  bind_rows(eng_wales_popn_past) %>% 
  filter(substr(gss_code,1,3) %in% england_wales_codes) %>% 
  bind_rows(other_popn_past, scotland_2021_pop, n_ireland_2021_pop) %>% 
  check_negative_values("popn")

final_births <- filter(gla_series, component == "births") %>% 
  mutate(value = ifelse(value < 0, 0, value)) %>% 
  select(gss_code, year, sex, age, births = value) %>% 
  data.frame() %>% 
  bind_rows(eng_wales_births_past)%>% 
  filter(substr(gss_code,1,3) %in% england_wales_codes) %>% 
  bind_rows(other_births_past, scotland_births_2021, n_ireland_births_2021) %>% 
  check_negative_values("births") %>% 
  complete_popn_dataframe(col_data = "births")

final_deaths <- filter(gla_series, component == "deaths") %>% 
  mutate(value = ifelse(value < 0, 0, value)) %>% 
  select(gss_code, year, sex, age, deaths = value) %>% 
  data.frame() %>% 
  bind_rows(eng_wales_deaths_past)%>% 
  filter(substr(gss_code,1,3) %in% england_wales_codes) %>% 
  bind_rows(other_deaths_past, scotland_deaths_2021, n_ireland_deaths_2021) %>% 
  check_negative_values("deaths")

final_int_in <- filter(gla_series, component == "international_in") %>% 
  mutate(value = ifelse(value < 0, 0, value)) %>% 
  select(gss_code, year, sex, age, int_in = value) %>% 
  data.frame() %>% 
  bind_rows(eng_wales_int_in_past) %>% 
  filter(substr(gss_code,1,3) %in% england_wales_codes) %>% 
  bind_rows(other_int_in_past, scotland_int_in_2021, n_ireland_int_in_2021) %>% 
  check_negative_values("int_in") 

final_int_out <- filter(gla_series, component == "international_out") %>% 
  mutate(value = ifelse(value < 0, 0, value)) %>% 
  select(gss_code, year, sex, age, int_out = value) %>% 
  data.frame() %>% 
  bind_rows(eng_wales_int_out_past)%>% 
  filter(substr(gss_code,1,3) %in% england_wales_codes) %>% 
  bind_rows(other_int_out_past, scotland_int_out_2021, n_ireland_int_out_2021) %>% 
  check_negative_values("int_out")

final_int_net <- final_int_out %>% 
  mutate(int_in = int_out *-1) %>% 
  select(-int_out) %>% 
  bind_rows(final_int_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(int_net = sum(int_in), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------
# Domestic 2021 OD matrix


matrix_existing <- readRDS(f_paths$dom_matrix_past)
matrix_2021 <- readRDS(f_paths$dom_matrix_2021) %>% 
  select(names(matrix_existing))

dom_matrix <- bind_rows(matrix_existing, matrix_2021)

#---

dir.create("input_data/domestic_migration/2021", showWarnings = FALSE)
saveRDS(dom_matrix, f_paths$dom_matrix_out)

#---

# Rebuild gross flows: Group OD data into gross flows data

region_lookup <- readRDS(f_paths$region_lookup)

aggregated_dom <- aggregate_regional_flows(dom_matrix,
                                         region_lookup = region_lookup,
                                         flow_col = "value",
                                         inner_outer_lookup =  "input_data/lookup/inner_and_outer_london.rds")

region_dom_in <- aggregated_dom[[1]] %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) == "E")

region_dom_out <- aggregated_dom[[1]] %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) == "E")

national_dom_in <- aggregated_dom[[2]] %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) %in% c("E","W"))

national_dom_out <- aggregated_dom[[2]] %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,1) %in% c("E","W"))

sub_reg_dom_in <- aggregated_dom[[3]] %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,3)=="E13") 

sub_reg_dom_out <- aggregated_dom[[3]] %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  filter(substr(gss_code,1,3)=="E13")

#--

other_dom_in <- bind_rows(national_dom_in, region_dom_in, sub_reg_dom_in) %>% 
  complete_popn_dataframe(col_data = "dom_in")

other_dom_out <- bind_rows(national_dom_out, region_dom_out, sub_reg_dom_out) %>% 
  complete_popn_dataframe(col_data = "dom_out")

#---

final_dom_in <- dom_matrix %>% 
  filter(gss_in != gss_out) %>% 
  group_by(gss_code = gss_in, year, sex, age) %>% 
  summarise(dom_in = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  complete_popn_dataframe(col_data = "dom_in") %>% 
  bind_rows(other_dom_in)

final_dom_out <- dom_matrix %>% 
  filter(gss_in != gss_out) %>% 
  group_by(gss_code = gss_out, year, sex, age) %>% 
  summarise(dom_out = sum(value), .groups = 'drop_last') %>% 
  data.frame() %>% 
  complete_popn_dataframe(col_data = "dom_out") %>% 
  bind_rows(other_dom_out) 

final_dom_net <- final_dom_out %>% 
  mutate(dom_in = dom_out *-1) %>% 
  select(-dom_out) %>% 
  bind_rows(final_dom_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(dom_net = sum(dom_in), .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

outputs <- list(final_popn = final_popn,
                final_births = final_births,
                final_deaths = final_deaths,
                final_int_in = final_int_in,
                final_int_out = final_int_out,
                final_int_net = final_int_net) %>% 
  lapply(FUN = function(x){
    aggregate_regions(x,
                      gss_col = "gss_code",
                      col_aggregation = c("year", "sex", "age"),
                      england = TRUE,
                      lookup = "input_data/lookup/district_to_region.rds")
  })

#-------------------------------------------------------------------------------

sum(is.na(outputs$final_popn))
sum(is.na(outputs$final_births))
sum(is.na(outputs$final_deaths))
sum(is.na(outputs$final_int_in))
sum(is.na(outputs$final_int_out))
sum(is.na(outputs$final_int_net))
sum(is.na(outputs$final_dom_in))
sum(is.na(outputs$final_dom_out))

#save
saveRDS(outputs$final_popn, paste0(f_paths$output_dir, "population_gla.rds"))
saveRDS(outputs$final_births, paste0(f_paths$output_dir, "births_gla.rds"))
saveRDS(outputs$final_deaths, paste0(f_paths$output_dir, "deaths_gla.rds"))
saveRDS(outputs$final_int_in, paste0(f_paths$output_dir, "int_in_gla.rds"))
saveRDS(outputs$final_int_out, paste0(f_paths$output_dir, "int_out_gla.rds"))
saveRDS(outputs$final_int_net, paste0(f_paths$output_dir, "int_net_gla.rds"))
saveRDS(final_dom_in, paste0(f_paths$output_dir, "dom_in_gla.rds"))
saveRDS(final_dom_out, paste0(f_paths$output_dir, "dom_out_gla.rds"))
saveRDS(final_dom_net, paste0(f_paths$output_dir, "dom_net_gla.rds"))

rm(list=ls())
