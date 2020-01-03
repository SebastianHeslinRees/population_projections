library(data.table)
library(dplyr)
library(tidyr)

#Set this to the location of the final borough components
mye_popn_path <- "input_data/mye/2018/population_gla_2019-11-13.rds"
borough_dom_in_path <- "input_data/domestic_migration/2018/domestic_migration_in.rds"
borough_dom_out_path <- "input_data/domestic_migration/2018/domestic_migration_out.rds"
borough_int_out_path <- "input_data/mye/2018/international_out_gla_2019-11-13.rds"
borough_int_in_path <- "input_data/mye/2018/international_in_gla_2019-11-13.rds"

#census data
census_foreign_born_path <- "Documents/work/census_tables/LC2103EW_ward_country_of_birth.csv"
ward_dom_out_path <- "Documents/work/census_tables/CT0356_london_wards_out_migration.csv"
ward_dom_in_path <- "Documents/work/census_tables/CT0354_london_wards_in_migration.csv"
ward_int_in_path <- "Documents/work/census_tables/CT0409_london_wards_in_migration_inc_international.csv"

#pre-processed ward model inputs
ward_births_path <- "input_data/small_area_model/ward_births_2001_2018.rds"
ward_deaths_path <- "input_data/small_area_model/ward_deaths_2001_2018.rds"

#lookup
ward_district <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/ward to district.rds")

#DOMESTIC OUT
#Data come from the census
#Data only goes to age 75 so ages 75-90 are modelled using borough distribution

borough_domestic_out <- readRDS(borough_dom_out_path) %>% filter(year == 2011)

domestic_out <- data.table::fread(ward_dom_out_path, header = T) %>%
        pivot_longer(cols = as.character(0:75), names_to = "age",
                     values_to = "domestic_out_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_ward, sex, age, domestic_out_migrants) %>%
        as.data.frame() %>%
        extend_ward_data(borough_data = borough_domestic_out,
                         ward_data_col = "domestic_out_migrants",
                         borough_data_col = "dom_out",
                         col_aggregation=c("gss_code","sex"))

#Scale from census to MYE
domestic_out <- popmodules::constrain_component(popn = domestic_out,
                                                     constraint = borough_domestic_out,
                                                     col_aggregation = c("sex","age"),
                                                     col_popn = "domestic_out_migrants",
                                                     col_constraint = "dom_out")
rm(borough_domestic_out)

#DOMESTIC IN
borough_domestic_in <- readRDS(borough_dom_in_path) %>% filter(year == 2011)

domestic_in <- data.table::fread(ward_dom_in_path, header = T) %>%
        pivot_longer(cols = as.character(0:75), names_to = "age",
                     values_to = "domestic_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_ward, sex, age, domestic_in_migrants) %>%
        as.data.frame() %>%
        extend_ward_data(borough_data = borough_domestic_in,
                         ward_data_col = "domestic_out_migrants",
                         borough_data_col = "dom_out",
                         col_aggregation=c("gss_code","sex"))

#Scale from census to MYE
domestic_in <- popmodules::constrain_component(popn = domestic_in,
                                                constraint = borough_domestic_in,
                                                col_aggregation = c("sex","age"),
                                                col_popn = "domestic_in_migrants",
                                                col_constraint = "dom_in")
rm(borough_domestic_in)

#INTERNATIONAL OUT
#Data from census on % of the borough's non-uk-born pop in each ward in that borough
#Apply that percentage to the international out flow for 2011 from the MYE CoC

foreign_born <- data.table::fread(census_foreign_born_path) %>%
        mutate(male = male_total - male_ukborn,
               female = female_total - female_ukborn) %>%
        select(gss_code_ward, male, female) %>%
        pivot_longer(cols = c(male, female),
                     names_to = "sex",
                     values_to = "nonUKborn") %>%
        left_join(ward_district, by="gss_code_ward") %>%
        group_by(gss_code_district, sex) %>%
        mutate(borough_nonUKborn = sum(nonUKborn),
               nonUKborn_proportion = nonUKborn/borough_nonUKborn) %>%
        as.data.frame() %>%
        select(gss_code_ward, gss_code, sex, nonUKborn_proportion) 

international_out <- readRDS(borough_int_out_path) %>%
        filter(year==2011) %>%
        select(gss_code, sex, age, int_out) %>%
        left_join(foreign_born, by=c("gss_code","sex")) %>%
        #filter(gss_code_borough != "E09000001") %>%
        mutate(international_out_migrants = int_out * nonUKborn_proportion) %>%
        select(gss_code_ward, gss_code, sex, age, international_out_migrants)

rm(foreign_born)

#INTERNATIONAL IN
borough_international_in <- readRDS(borough_int_in_path) %>% filter(year == 2011)

ward_int_in <- data.table::fread(ward_int_in_path, header = T) %>%
        pivot_longer(cols = as.character(0:75), names_to = "age",
                     values_to = "all_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_ward, sex, age, all_in_migrants)

subtract_domestic <- domestic_in %>%
        mutate(age = ifelse(age>75, 75, age)) %>%
        group_by(gss_code_ward, sex, age) %>%
        summarise(domestic = sum(domestic_in_migrants)) %>%
        data.frame() %>%
        left_join(ward_int_in, by=c("gss_code_ward","sex","age")) %>%
        replace_na(list(domestic = 0)) %>%
        mutate(international = all_in_migrants - domestic) %>%
        check_negative_values("international") %>%
        extend_ward_data(borough_data = borough_international_in,
                         ward_data_col = "international",
                         borough_data_col = "int_in",
                         col_aggregation=c("gss_code","sex"))

international_in <- popmodules::constrain_component(popn = subtract_domestic,
                                               constraint = borough_international_in,
                                               col_aggregation = c("sex","age"),
                                               col_popn = "international",
                                               col_constraint = "international_in_migrants")

rm(borough_international_in, ward_int_in, substract_domestic)
 
#Out migration rates
ward_births_2011 <- readRDS(ward_births_path) %>% filter(year == 2011)
ward_deaths_2011 <- readRDS(ward_deaths_path) %>% filter(year == 2011)
ward_popn_2010 <- readRDS(ward_popn_path)

denominators <- ward_popn_2010 %>%
        filter(year == 2010) %>%
        popmodules::popn_age_on(births = ward_births) %>%
        left_join(ward_deaths, by=c("gss_code_ward","sex","age")) %>%
        mutate(popn = popn - deaths) %>%
        select(-deaths)
        
out_migration_rates <- left_join(domestic_out, international_out,
                                 by=c("gss_code_ward","sex","age")) %>%
        left_join(denominators, by=c("gss_code_ward","sex","age")) %>%
        mutate(out_migration_rate = (domestic_out_migrants + international_out_migrants)/popn) %>%
        select(gss_code_ward, sex, age, out_migration_rate)


#In migration distribution
in_migration_characteristics <- left_join(domestic_in, international_in,
                                       by=c("gss_code_ward","sex","age")) %>%
        mutate(all_in_migration = domestic_in_migrants + international_in_migrations) %>%
        group_by(gss_code_ward, sex) %>%
        mutate(total_migration = sum(all_in_migration),
               migration_distribution = all_in_migration/total_migration) %>%
        as.data.frame() %>%
        mutate(adult_dist = ifelse(age >= 18, migration_distribution, 0)) %>%
        group_by(gss_code_ward, sex) %>%
        mutate(aggregate_dist = sum(adult_dist)) %>%
        as.data.frame() %>%
        mutate(scaling = 1/aggregate_dist) %>%
        mutate(scaled_dist = scaling*adult_dist) %>%
        mutate(final_dist = ifelse(age >= 18, scaled_dist, migration_distribution)) %>%
        select(gss_code_ward, sex, age, final_dist)
        #TODO rename(xxxx = final_dist)

#Save
saveRDS(in_migration_characteristics, "input_data/small_area_model/ward_in_migration_characteristics.rds")
saveRDS(out_migration_rates, "input_data/small_area_model/ward_out_migration_rates.rds")
        
#Denominators
#We need a ward population figure for 2010
#This is backwards-engineered from the 2011 poopulation and the components
#TODO check if we can get this from the ONS small area estimates
#If we can then I don't the the in migration stuff in this script is necessary

# setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/Base data/")
# ward_population <- fread("Base Population.csv") %>%
#         select(4,3,6,7) %>%
#         setnames(c("gss_code_ward","sex","age","census_population")) %>%
#         data.frame()
# 
# scale_to_borough <- ward_population %>%
#         left_join(ward_district, by="gss_code_ward") %>%
#         group_by(gss_code = gss_code_district, sex, age) %>%
#         summarise(ward_aggregate = sum(census_population)) %>%
#         data.frame() %>%
#         left_join(filter(final_mye_components,
#                          var == "population",
#                          substr(gss_code,1,3) == "E09",
#                          year==2011),
#                   by=c("gss_code","sex","age")) %>%
#         mutate(scaling = ifelse(ward_aggregate == 0, 0, estimate/ward_aggregate)) %>%
#         select(gss_code_district = gss_code, sex, age, scaling)
# 
# ward_population <- ward_population %>%
#         left_join(ward_district, by="gss_code_ward") %>%
#         left_join(scale_to_borough, by=c("gss_code_district","sex","age")) %>%
#         mutate(census_population = census_population*scaling) %>%
#         select(gss_code_ward, sex, age, census_population)
# 
# 
# model_deaths <- final_mye_components %>%
#         filter(var == "deaths",
#                substr(gss_code,1,3) == "E09",
#                year==2011) %>%
#         filter(gss_code != "E09000001") %>%
#         group_by(gss_code) %>%
#         mutate(borough_total = sum(estimate)) %>%
#         data.frame() %>%
#         mutate(b_proportion = estimate/borough_total) %>%
#         select(gss_code_borough = gss_code, sex, age, b_proportion)
# 
# setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/")
# ward_deaths <- readRDS("deaths.rds") %>%
#         filter(year == 2011) %>%
#         full_join(model_deaths, by=c("gss_code_borough")) %>%
#         mutate(modelled_deaths = b_proportion * deaths_actual) %>%
#         select(gss_code_ward, sex, age, modelled_deaths)
# 
# reverse <- ward_population %>%
#         left_join(domestic_out, by = c("gss_code_ward", "sex", "age")) %>%
#         left_join(domestic_in, by = c("gss_code_ward", "sex", "age")) %>%
#         left_join(international_out, by = c("gss_code_ward", "sex", "age")) %>%
#         left_join(international_in, by = c("gss_code_ward", "sex", "age")) %>%
#         replace_na(list(domestic_in_migrants = 0, domestic_out_migrants = 0,
#                         international_in_migrants = 0, international_out_migrants = 0)) %>%
#         left_join(ward_deaths, by=c("gss_code_ward","sex","age")) %>%
#         mutate(reversed_pop = census_population + domestic_out_migrants - domestic_in_migrants +
#                        international_out_migrants - international_in_migrants + modelled_deaths)
# 
# denominator <- select(reverse, gss_code_ward, sex, age, at_risk_population = reversed_pop)
# 
# rm(model_deaths, reverse, ward_deaths, ward_population)
# 
# #Out Migration Rates
# #Rates are the dom out plus the modelled int out over the census population
# probabilities <- left_join(domestic_out, international_out, by = c("gss_code_ward","sex","age")) %>%
#         left_join(denominator, by=c("gss_code_ward","sex","age")) %>%
#         mutate(out_migration_rate = (international_out_migrants + domestic_out_migrants)/at_risk_population) %>%
#         left_join(ward_district, by="gss_code_ward") %>%
#         select(gss_code_borough, gss_code_ward, sex, age, out_migration_rate)
# 
# #Take a look at the rates that are problematic
# #The code fixes these in the lines below but if there are a lot of cases
# #then perhaps some other approach is necessary
# summary(probabilities$out_migration_rate)
# View(filter(probabilities, !(out_migration_rate >= 0 & out_migration_rate <= 0.8)), "problem rates")
# 
# #Fix
# probabilities <- mutate(probabilities, out_migration_rate = ifelse(out_migration_rate>0.8, 0.8, out_migration_rate))
# probabilities <- mutate(probabilities, out_migration_rate = ifelse(out_migration_rate<=0, 1e-5, out_migration_rate))
# 
# #Checks
# #Only save if these checks are passed
# if(is.character(probabilities$gss_code_borough) &
#    is.character(probabilities$gss_code_ward) &
#    is.character(probabilities$sex) &
#    is.numeric(probabilities$age) &
#    is.numeric(probabilities$out_migration_rate) &
#    min(probabilities$out_migration_rate) >= 0 &
#    max(probabilities$out_migration_rate) != Inf &
#    max(probabilities$out_migration_rate) <= 0.8 &
#    nrow(probabilities) == 113568 &
#    ncol(probabilities) == 5){
#         #Save and overwrite base outmigration rates for ward model
#         setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/")
#         saveRDS(probabilities, "base_outmigration_rates.rds")
# } else {
#         message("WARNING: File not saved - error in data (see checks at the end of the script)")
# }
# 
# summary(probabilities$out_migration_rate)

