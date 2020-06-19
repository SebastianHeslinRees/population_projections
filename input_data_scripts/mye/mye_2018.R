library(tidyverse)
library(data.table)
devtools::load_all("model_code/popmodules/")

message("mye data")

ew_coc_file <- "Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2018/detailed_mye_coc.rds"
uk_pop_file <- "Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2018/MYEB1_detailed_population_estimates_series_UK_(2018).csv"
uk_coc_file <- "Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2018/MYEB3_summary_components_of_change_series_UK_(2018).csv"

#England and Wales
mye_coc <- readRDS(ew_coc_file) %>%
  rename(gss_code = ladcode18, gss_name = laname18) %>%
  mutate(geography = "LAD18", sex = case_when(sex == 1 ~ "male", sex == 2 ~ "female"), 
         year = as.integer(year)) %>%
  as.data.frame()

#Scotland & NI Population
uk_pop <- fread(uk_pop_file) %>%
  data.frame() %>%
  filter(country %in% c("N","S")) %>%
  gather(year, value, 6:23) %>%
  mutate(year = substr(year, 12, 15),
         year = as.numeric(year),
         component = "population",
         geography = "LAD18") %>%
  mutate(gss_code = case_when(country=="S" ~ "S92000003",
                              country=="N" ~ "N92000002")) %>%
  mutate(gss_name = case_when(country=="S" ~ "Scotland",
                              country=="N" ~ "Northern Ireland")) %>%
  mutate(sex = case_when(sex == 1 ~ "male", sex == 2 ~ "female")) %>%
  group_by(gss_code, gss_name, country, sex, age, component, year, geography) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#Scotland & NI Components
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

uk_coc <- fread(uk_coc_file) %>%
  data.frame() %>%
  filter(country %in% c("N","S")) %>%
  gather(var, value, 4:191) %>%
  mutate(year = as.numeric(right(var, 4)),
         year = as.numeric(year),
         component = substr(var,1,nchar(var)-5),
         geography = "LAD18") %>%
  mutate(gss_code = case_when(substr(ladcode18,1,1)=="S" ~ "S92000003",
                              substr(ladcode18,1,1)=="N" ~ "N92000002")) %>%
  mutate(gss_name = case_when(substr(ladcode18,1,1)=="S" ~ "Scotland",
                              substr(ladcode18,1,1)=="N" ~ "Northern Ireland")) %>%
  #mutate(sex = case_when(sex == 1 ~ "male", sex == 2 ~ "female")) %>%
  group_by(gss_code, gss_name, country, component, year, geography) %>%
  summarise(value = sum(value)) %>%
  ungroup()

#Scotland & NI births
uk_births <- filter(uk_coc, component == "births") %>%
  mutate(male = value * (105/205),
         female = value * (100/205)) %>%
  select(-value) %>%
  gather(sex, value, 7:8) %>%
  mutate(age = 0)
x <- list()
for(i in 1:90){x[[i]] <- mutate(uk_births, age = i, value = 0)}
uk_births <- rbind(uk_births, rbindlist(x))
rm(x,i)

#Scotland & NI International
international_curves <- filter(mye_coc, component %in% c("international_in","international_out")) %>%
  #filter(year == 2017) %>%
  group_by(component, age, sex, year) %>%
  summarise(estimate = sum(value)) %>%
  group_by(component) %>%
  mutate(total = sum(estimate),
         structure = estimate/total) %>%
  select(-estimate, -total)

uk_international <- filter(uk_coc, component %in%  c("international_in","international_out")) %>%
  left_join(international_curves, by=c("year","component")) %>%
  mutate(value = value * structure) %>%
  select(names(mye_coc))

uk_net_international <- uk_international %>%
  spread(component, value) %>%
  mutate(value = international_in - international_out,
         component = "international_net") %>%
  select(names(mye_coc))

#Scotland & NI deaths
death_curves <- filter(mye_coc, component == "deaths") %>%
  #filter(year == 2017) %>%
  group_by(component, age, sex, year) %>%
  summarise(estimate = sum(value)) %>%
  group_by(component) %>%
  mutate(total = sum(estimate),
         structure = estimate/total) %>%
  select(-estimate, -total)

uk_deaths <- filter(uk_coc, component == "deaths") %>%
  left_join(death_curves, by=c("year","component")) %>%
  mutate(value = value * structure) %>%
  select(names(mye_coc))

#Append Scotland and NI data to E&W
#Recode any gss codes which are not standard 2011 codes
mye_coc <- rbind(mye_coc, uk_pop, uk_births, uk_international, uk_net_international, uk_deaths) %>%
  recode_gss_codes(col_geog="gss_code",
                   data_cols = "value",
                   fun=list(sum),
                   recode_gla_codes = TRUE)

rm(death_curves, international_curves, uk_births, uk_coc, uk_deaths, uk_international, uk_net_international,
   uk_pop, ew_coc_file, uk_coc_file, uk_pop_file, right)

#Split out components
births <- filter(mye_coc, component == "births") %>% select(-component) %>% rename(births = value)
deaths <- filter(mye_coc, component == "deaths") %>% select(-component) %>% rename(deaths = value)
population <- filter(mye_coc, component == "population") %>% select(-component) %>% rename(popn = value)
international_in <- filter(mye_coc, component == "international_in") %>% select(-component) %>% rename(int_in = value)
international_out <- filter(mye_coc, component == "international_out") %>% select(-component) %>% rename(int_out = value)
international_net <- filter(mye_coc, component == "international_net") %>% select(-component) %>% rename(int_net = value)

#Domestic here does not include Scotland and NI
#This is processed this from the domestic migration dataset in domestic_migration_2018.R
# domestic_in <- filter(mye_coc, component == "internal_in") %>% select(-component) %>% rename(dom_in = value)
# domestic_out <- filter(mye_coc, component == "internal_out") %>% select(-component) %>% rename(dom_out = value)
# domestic_net <- filter(mye_coc, component == "internal_net") %>% select(-component) %>% rename(dom_net = value)

#Other components
uk_upc <- filter(births, country %in% c("N","S")) %>%
  rename(upc = births) %>%
  mutate(upc = 0)

upc <- filter(mye_coc, component %in% c("special_change", "unattrib", "other_adjust")) %>%
  group_by(gss_code, gss_name, country, sex, age, year, geography) %>%
  summarise(upc = sum(value)) %>%
  ungroup() %>%
  select(names(uk_upc)) %>%
  rbind(uk_upc)

# interpolate points where deaths == -1
# FIXME this is a clumsy way to do it but everything with grouping was taking a million years, and no 2018 data are missing
deaths <- arrange(deaths, gss_code, age, sex, year)
ix <- deaths$deaths < 0
deaths$deaths[ix] <- NA
assert_that(!any( range(deaths$year) %in% deaths$year[ix]), msg = "Min or max year of deaths missing: can't interpolate without more information")
missing_deaths <- approx(x = 1:nrow(deaths), y = deaths$deaths, xout = (1:nrow(deaths))[ix])
deaths$deaths[ix] <- missing_deaths$y
deaths <- arrange(deaths, gss_code, year, sex, age)


# TODO: CALL SOME DATA CHECKING FUNCTIONS HERE
validate_population(births)
validate_population(deaths)
validate_population(population)
validate_population(international_in)
validate_population(international_out)
validate_population(international_net)
validate_population(upc)

validate_same_geog(population, births)
validate_same_geog(population, deaths)
validate_same_geog(population, international_in)
validate_same_geog(population, international_out)
validate_same_geog(population, international_net)
validate_same_geog(population, upc)


datestamp <- Sys.Date()

dir.create("input_data/mye/2018", showWarnings = FALSE, recursive = TRUE)

saveRDS(births, file = paste0("input_data/mye/2018/births_ons.rds"))
saveRDS(deaths, file = paste0("input_data/mye/2018/deaths_ons.rds"))
saveRDS(population, file = paste0("input_data/mye/2018/population_ons.rds"))
saveRDS(international_in, file = paste0("input_data/mye/2018/international_in_ons.rds"))
saveRDS(international_out, file = paste0("input_data/mye/2018/international_out_ons.rds"))
saveRDS(international_net, file = paste0("input_data/mye/2018/international_net_ons.rds"))
#saveRDS(domestic_in, file = paste0("input_data/mye/2018/domestic_in_ons_", datestamp, ".rds"))
#saveRDS(domestic_out, file = paste0("input_data/mye/2018/domestic_out_ons_", datestamp, ".rds"))
#saveRDS(domestic_net, file = paste0("input_data/mye/2018/domestic_net_ons_", datestamp, ".rds"))
saveRDS(upc, file = paste0("input_data/mye/2018/upc_ons.rds"))

rm(list=ls())

