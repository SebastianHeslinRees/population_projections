#ONS Household Rates

read_hh_data_files <- function(file, file_location, year){
  
  x <- readRDS(paste0(file_location, file)) %>%
    filter(year == year)
  return(x)
}

file_location <- "Q:/Teams/D&PA/Demography/Projections/R Models/ONS Household Projections/GLA implementation/input"

rates_2001 <- read_hh_data_files("/hh_rep_rates.rds", file_location, 2001)
rates_2011 <- read_hh_data_files("/hh_rep_rates.rds", file_location, 2011)

households_2001 <- read_hh_data_files("/households.rds", file_location, 2001)
households_2011 <- read_hh_data_files("/households.rds", file_location, 2011)

ce <- read_hh_data_files("/communal_pop.rds", file_location, 2011)


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

setwd("M:/Projects/population_projections/input_data/household_model/")
saveRDS(rates, "ons_household_representative_rates.rds")

saveRDS(ce, "ons_communal_establishment_population.rds")

#--------------------------------------------------------

stage1_data <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model - original/Inputs/2014 DCLG stage 1 data.rds") %>%
  setnames(c("gss_code","year","sex","household_type","age_group","households",
             "household_population","institutional_population","total_population","hh_rep_rates"))


stage2_data <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model - original/Inputs/2014 DCLG Stage 2 headship rates.rds") %>%
  rename(rate = DCLG.rate) %>%
  select(-district)

setwd("M:/Projects/population_projections/input_data/household_model/")

saveRDS(stage1_data, "dclg_stage1_data_2014.rds")
saveRDS(stage2_data, "dclg_headship_rates_2014.rds")

