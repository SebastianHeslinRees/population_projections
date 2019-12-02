library(data.table)
library(dplyr)
library(tidyr)
#library(xlsx)

#Set this to the location of the final borough components
mye_path <- "input_data/mye/2018/population_gla_2019-11-13.rds"
mye_components <- readRDS(mye_components_path)
internal_migration <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Inputs/2017 base/constant inputs/ons_internal_migration.rds")

####Code below shouldn't need messing with

#add domestic to mye dataframe
mye_dom_in <- group_by(final_internal_migration, gss_code=in_la, year, age, sex) %>%
        summarise(estimate = sum(flow)) %>%
        data.frame() %>%
        mutate(var = "domestic_in") %>%
        left_join(unique(select(final_mye_components, gss_code, district)), by="gss_code") %>%
        select(names(final_mye_components))
        
mye_dom_out <- group_by(final_internal_migration, gss_code=out_la, year, age, sex) %>%
        summarise(estimate = sum(flow)) %>%
        data.frame() %>%
        mutate(var = "domestic_out") %>%
        left_join(unique(select(final_mye_components, gss_code, district)), by="gss_code") %>%
        select(names(final_mye_components))

final_mye_components <- rbind(final_mye_components, mye_dom_in, mye_dom_out)

rm(mye_dom_in, mye_dom_out, final_internal_migration)
gc()

#lookup
ward_district <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/ward to district.rds")

#DOMESTIC OUT
#Data come from the census
#Males and Females separately
#Most of this code is just getting it into a tidy format
#Data only goes to age 75 so ages 75-90 are modelled using borough distribution
setwd("Q:/Teams/D&PA/Census/Commissioned Tables/CT Tables 2011/")
CT0356_M <- read.xlsx("CT0356 London wards to UK outside ward x sya x sex.xlsx",
                      sheetName = "CT0356 Males", startRow = 11, header = T) %>%
        select(1, 4:79) %>%
        gather(age, out_migrants, 2:77) %>%
        mutate(sex = "M",
               age = gsub("Age.","",age),
               age = as.numeric(substr(age,1,2))) %>%
        filter(substr(ONS.Code,1,3)=="E05") %>%
        setnames(c("gss_code_ward","age","domestic_out_migrants","sex")) %>%
        select(gss_code_ward, sex, age, domestic_out_migrants)


CT0356_F <- read.xlsx("CT0356 London wards to UK outside ward x sya x sex.xlsx",
                     sheetName = "CT0356 Females", startRow = 11, header = T) %>%
        select(1, 4:79) %>%
        gather(age, out_migrants, 2:77) %>%
        mutate(sex = "F",
               age = gsub("Age.","",age),
               age = as.numeric(substr(age,1,2))) %>%
        filter(substr(ONS.Code,1,3)=="E05") %>%
        setnames(c("gss_code_ward","age","domestic_out_migrants","sex")) %>%
        select(gss_code_ward, sex, age, domestic_out_migrants)


domestic_out <- rbind(CT0356_F, CT0356_M) %>%
        mutate(gss_code_ward = as.character(gss_code_ward))
        

borough_codes <- filter(ward_district, substr(gss_code_district,1,3)=="E09") %>%
        select(gss_code_district) %>%
        unique() 

#Model ages 75 and over
borough_elderly <- filter(final_mye_components,
                          substr(gss_code,1,3)=="E09",
                          year == 2011, age %in% c(75:90),
                          var == "domestic_out") %>%
        filter(gss_code != "E09000001") %>%
        rename(borough_flow = estimate,
               gss_code_borough = gss_code) %>%
        right_join(expand.grid(gss_code_borough = borough_codes$gss_code_district,
                               sex=c("F","M"),
                               age=c(75:90),
                               stringsAsFactors=F),
                   by=c("gss_code_borough","sex","age")) %>%
        filter(gss_code_borough != "E09000001") %>%
        replace_na(list(borough_flow = 0)) %>%
        group_by(gss_code_borough, sex) %>%
        mutate(borough_total = sum(borough_flow)) %>%
        data.frame() %>%
        mutate(b_proportion = borough_flow / borough_total) %>%
        select(gss_code_borough, sex, age, b_proportion)

age_75_plus <- filter(domestic_out, age == 75) %>%
        left_join(ward_district, by="gss_code_ward") %>%
        rename(gss_code_borough = gss_code_district) %>%
        full_join(borough_elderly, by=c("gss_code_borough","sex")) %>%
        mutate(modelled_flow = b_proportion * domestic_out_migrants) %>%
        select(gss_code_ward, sex, age.y, modelled_flow) %>%
        setnames(names(domestic_out))

domestic_out <- filter(domestic_out, age < 75) %>%
        rbind(age_75_plus) 

rm(CT0356_M, CT0356_F, age_75_plus, borough_elderly)


#Scale from census to MYE
scale_to_borough <- domestic_out %>%
        left_join(ward_district, by="gss_code_ward") %>%
        group_by(gss_code = gss_code_district, sex, age) %>%
        summarise(ward_aggregate = sum(domestic_out_migrants)) %>%
        data.frame() %>%
        left_join(filter(final_mye_components,
                         var == "domestic_out",
                         substr(gss_code,1,3) == "E09",
                         year==2011),
                  by=c("gss_code","sex","age")) %>%
        mutate(scaling = ifelse(ward_aggregate == 0, 0, estimate/ward_aggregate)) %>%
        select(gss_code_district = gss_code, sex, age, scaling)

domestic_out <- domestic_out %>%
        left_join(ward_district, by="gss_code_ward") %>%
        left_join(scale_to_borough, by=c("gss_code_district","sex","age")) %>%
        mutate(domestic_out_migrants = domestic_out_migrants*scaling) %>%
        select(gss_code_ward, sex, age, domestic_out_migrants)

rm(scale_to_borough)

#DOMESTIC IN
CT0354_M <- read.xlsx("Q:/Teams/D&PA/Census/Commissioned Tables/CT Tables 2011/CT0354 outside ward to London wards x sya x sex.xlsx",
                      sheetName = "CT0354 Males", startRow = 11, header = T) %>%
        select(1, 4:79) %>%
        gather(age, in_migrants, 2:77) %>%
        mutate(sex = "M",
               age = gsub("Age.","",age),
               age = as.numeric(substr(age,1,2))) %>%
        filter(substr(ONS.Code,1,3)=="E05") %>%
        setnames(c("gss_code_ward","age","domestic_in_migrants","sex")) %>%
        select(gss_code_ward, sex, age, domestic_in_migrants)


CT0354_F <- read.xlsx("Q:/Teams/D&PA/Census/Commissioned Tables/CT Tables 2011/CT0354 outside ward to London wards x sya x sex.xlsx",
                     sheetName = "CT0354 Females", startRow = 11, header = T) %>%
        select(1, 4:79) %>%
        gather(age, in_migrants, 2:77) %>%
        mutate(sex = "F",
               age = gsub("Age.","",age),
               age = as.numeric(substr(age,1,2))) %>%
        filter(substr(ONS.Code,1,3)=="E05") %>%
        setnames(c("gss_code_ward","age","domestic_in_migrants","sex")) %>%
        select(gss_code_ward, sex, age, domestic_in_migrants)


domestic_in <- rbind(CT0354_F, CT0354_M) %>%
        mutate(gss_code_ward = as.character(gss_code_ward))


#Model ages 75 and over
borough_elderly <- filter(final_mye_components,
                          substr(gss_code,1,3)=="E09",
                          year == 2011, age %in% c(75:90),
                          var == "domestic_in") %>%
        filter(gss_code != "E09000001") %>%
        rename(borough_flow = estimate,
               gss_code_borough = gss_code) %>%
        right_join(expand.grid(gss_code_borough = borough_codes$gss_code_district,
                               sex=c("F","M"),
                               age=c(75:90),
                               stringsAsFactors=F),
                   by=c("gss_code_borough","sex","age")) %>%
        filter(gss_code_borough != "E09000001") %>%
        replace_na(list(borough_flow = 0)) %>%
        group_by(gss_code_borough, sex) %>%
        mutate(borough_total = sum(borough_flow)) %>%
        data.frame() %>%
        mutate(b_proportion = borough_flow / borough_total) %>%
        select(gss_code_borough, sex, age, b_proportion)

age_75_plus <- filter(domestic_in, age == 75) %>%
        left_join(ward_district, by="gss_code_ward") %>%
        rename(gss_code_borough = gss_code_district) %>%
        full_join(borough_elderly, by=c("gss_code_borough","sex")) %>%
        mutate(modelled_flow = b_proportion * domestic_in_migrants) %>%
        select(gss_code_ward, sex, age.y, modelled_flow) %>%
        setnames(names(domestic_in))

domestic_in <- filter(domestic_in, age < 75) %>%
        rbind(age_75_plus) %>%
        mutate(gss_code_ward = as.character(gss_code_ward))

rm(CT0354_M, CT0354_F, age_75_plus, borough_elderly)

#Scale from census to MYE
scale_to_borough <- domestic_in %>%
        left_join(ward_district, by="gss_code_ward") %>%
        group_by(gss_code = gss_code_district, sex, age) %>%
        summarise(ward_aggregate = sum(domestic_in_migrants)) %>%
        data.frame() %>%
        left_join(filter(final_mye_components,
                         var == "domestic_in",
                         substr(gss_code,1,3) == "E09",
                         year==2011),
                  by=c("gss_code","sex","age")) %>%
        mutate(scaling = ifelse(ward_aggregate == 0, 0, estimate/ward_aggregate)) %>%
        select(gss_code_district = gss_code, sex, age, scaling)

domestic_in <- domestic_in %>%
        left_join(ward_district, by="gss_code_ward") %>%
        left_join(scale_to_borough, by=c("gss_code_district","sex","age")) %>%
        mutate(domestic_in_migrants = domestic_in_migrants*scaling) %>%
        select(gss_code_ward, sex, age, domestic_in_migrants)

rm(scale_to_borough)

#INTERNATIONAL OUT
#Data from census on % of the borough's non-uk-born pop in each msoa in that borough
#Apply that percentage to the international out flow for 2011 from the MYE CoC

setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/census data/")
foreign_born <- fread("LC2103EW_COB_WARD.csv") %>%
        mutate(male_non_ukborn = male_total - male_ukborn,
               female_non_ukborn = female_total - female_ukborn) %>%
        select(1,6,7) %>%
        gather(sex, nonUKborn, 2:3) %>%
        mutate(sex = toupper(substr(sex,1,1))) %>%
        left_join(ward_district, by="gss_code_ward") %>%
        group_by(gss_code_district, sex) %>%
        mutate(borough_nonUKborn = sum(nonUKborn),
               nonUKborn_proportion = nonUKborn/borough_nonUKborn) %>%
        data.frame() %>%
        select(1,4,2,6) %>%
        rename(gss_code_borough = gss_code_district)


borough_international_out <- final_mye_components %>%
        filter(var == "international_out",
               substr(gss_code,1,3) == "E09",
               year==2011) %>%
        select(gss_code_borough = gss_code, sex, age, estimate)

international_out <- full_join(borough_international_out, foreign_born, by=c("gss_code_borough","sex")) %>%
        filter(gss_code_borough != "E09000001") %>%
        mutate(international_out_migrants = estimate * nonUKborn_proportion) %>%
        select(gss_code_ward, gss_code_borough, sex, age, international_out_migrants)

rm(foreign_born, borough_international_out)

#INTERNATIONAL IN
gc()
census_int_in_M <- read.xlsx("Q:/Teams/D&PA/Census/Commissioned Tables/CT Tables 2011/CT0409 outside ward to London wards inc international x age x sex London wards.xlsx",
                           sheetName = "CT0409 Males", startRow = 11, header = T) %>%
        select(1, 3:78) %>%
        gather(age, in_migrants, 2:77) %>%
        mutate(sex = "M",
               age = gsub("Age.","",age),
               age = as.numeric(substr(age,1,2))) %>%
        filter(substr(ONScode,1,3)=="E05") %>%
        setnames(c("gss_code_ward","age","all_in_migrants","sex")) %>%
        select(gss_code_ward, sex, age, all_in_migrants)

census_int_in_F <- read.xlsx("Q:/Teams/D&PA/Census/Commissioned Tables/CT Tables 2011/CT0409 outside ward to London wards inc international x age x sex London wards.xlsx",
                             sheetName = "CT0409 Females", startRow = 11, header = T) %>%
        select(1, 3:78) %>%
        gather(age, in_migrants, 2:77) %>%
        mutate(sex = "F",
               age = gsub("Age.","",age),
               age = as.numeric(substr(age,1,2))) %>%
        filter(substr(ONScode,1,3)=="E05") %>%
        setnames(c("gss_code_ward","age","all_in_migrants","sex")) %>%
        select(gss_code_ward, sex, age, all_in_migrants)

census_int_in <- rbind(census_int_in_F, census_int_in_M) %>%
        mutate(gss_code_ward = as.character(gss_code_ward))

subtract_domestic <- domestic_in %>%
        mutate(age = ifelse(age>75, 75, age)) %>%
        group_by(gss_code_ward, sex, age) %>%
        summarise(domestic = sum(domestic_in_migrants)) %>%
        data.frame() %>%
        left_join(census_int_in, by=c("gss_code_ward","sex","age")) %>%
        replace_na(list(domestic = 0)) %>%
        mutate(international = all_in_migrants - domestic) %>%
        mutate(international = ifelse(international < 0, 0, international))

borough_elderly <-  final_mye_components %>%
        filter(var == "international_in",
               substr(gss_code,1,3) == "E09",
               year==2011,
               age %in% c(75:90)) %>%
        filter(gss_code != "E09000001") %>%
        select(gss_code_borough = gss_code, sex, age, estimate)%>%
        group_by(gss_code_borough) %>%
        mutate(borough_total = sum(estimate)) %>%
        mutate(b_proportion = estimate / borough_total) %>%
        select(gss_code_borough, sex, age, b_proportion)

age_75_plus <- filter(subtract_domestic, age == 75) %>%
        left_join(ward_district, by="gss_code_ward") %>%
        rename(gss_code_borough = gss_code_district) %>%
        full_join(borough_elderly, by=c("gss_code_borough","sex")) %>%
        mutate(modelled_flow = b_proportion * international) %>%
        select(gss_code_ward, sex, age = age.y, international_in_migrants = modelled_flow)

international_in <- filter(subtract_domestic, age < 75) %>%
        select(gss_code_ward, sex, age, international_in_migrants = international) %>%
        rbind(age_75_plus) %>%
        mutate(gss_code_ward = as.character(gss_code_ward))

#calculate sya/sex scaling factors for each borough
scale_to_borough <- international_in %>%
        left_join(ward_district, by="gss_code_ward") %>%
        group_by(gss_code = gss_code_district, sex, age) %>%
        summarise(ward_aggregate = sum(international_in_migrants)) %>%
        data.frame() %>%
        left_join(filter(final_mye_components,
               var == "international_in",
               substr(gss_code,1,3) == "E09",
               year==2011),
               by=c("gss_code","sex","age")) %>%
        mutate(scaling = ifelse(ward_aggregate == 0, 0, estimate/ward_aggregate)) %>%
        replace_na(list(scaling = 0)) %>%
        select(gss_code_district = gss_code, sex, age, scaling)

#apply scaling factors
international_in <- international_in %>%
        left_join(ward_district, by="gss_code_ward") %>%
        left_join(scale_to_borough, by=c("gss_code_district","sex","age")) %>%
        mutate(international_in_migrants = international_in_migrants*scaling) %>%
        select(gss_code_ward, sex, age, international_in_migrants)

rm(age_75_plus, borough_elderly, census_int_in, census_int_in_F, census_int_in_M, scale_to_borough, subtract_domestic)

#Denominators
setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/Base data/")
ward_population <- fread("Base Population.csv") %>%
        select(4,3,6,7) %>%
        setnames(c("gss_code_ward","sex","age","census_population")) %>%
        data.frame()

scale_to_borough <- ward_population %>%
        left_join(ward_district, by="gss_code_ward") %>%
        group_by(gss_code = gss_code_district, sex, age) %>%
        summarise(ward_aggregate = sum(census_population)) %>%
        data.frame() %>%
        left_join(filter(final_mye_components,
                         var == "population",
                         substr(gss_code,1,3) == "E09",
                         year==2011),
                  by=c("gss_code","sex","age")) %>%
        mutate(scaling = ifelse(ward_aggregate == 0, 0, estimate/ward_aggregate)) %>%
        select(gss_code_district = gss_code, sex, age, scaling)

ward_population <- ward_population %>%
        left_join(ward_district, by="gss_code_ward") %>%
        left_join(scale_to_borough, by=c("gss_code_district","sex","age")) %>%
        mutate(census_population = census_population*scaling) %>%
        select(gss_code_ward, sex, age, census_population)


model_deaths <- final_mye_components %>%
        filter(var == "deaths",
               substr(gss_code,1,3) == "E09",
               year==2011) %>%
        filter(gss_code != "E09000001") %>%
        group_by(gss_code) %>%
        mutate(borough_total = sum(estimate)) %>%
        data.frame() %>%
        mutate(b_proportion = estimate/borough_total) %>%
        select(gss_code_borough = gss_code, sex, age, b_proportion)

setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/")
ward_deaths <- readRDS("deaths.rds") %>%
        filter(year == 2011) %>%
        full_join(model_deaths, by=c("gss_code_borough")) %>%
        mutate(modelled_deaths = b_proportion * deaths_actual) %>%
        select(gss_code_ward, sex, age, modelled_deaths)

reverse <- ward_population %>%
        left_join(domestic_out, by = c("gss_code_ward", "sex", "age")) %>%
        left_join(domestic_in, by = c("gss_code_ward", "sex", "age")) %>%
        left_join(international_out, by = c("gss_code_ward", "sex", "age")) %>%
        left_join(international_in, by = c("gss_code_ward", "sex", "age")) %>%
        replace_na(list(domestic_in_migrants = 0, domestic_out_migrants = 0,
                        international_in_migrants = 0, international_out_migrants = 0)) %>%
        left_join(ward_deaths, by=c("gss_code_ward","sex","age")) %>%
        mutate(reversed_pop = census_population + domestic_out_migrants - domestic_in_migrants +
                       international_out_migrants - international_in_migrants + modelled_deaths)

denominator <- select(reverse, gss_code_ward, sex, age, at_risk_population = reversed_pop)

rm(model_deaths, reverse, ward_deaths, ward_population)

#Out Migration Rates
#Rates are the dom out plus the modelled int out over the census population
probabilities <- left_join(domestic_out, international_out, by = c("gss_code_ward","sex","age")) %>%
        left_join(denominator, by=c("gss_code_ward","sex","age")) %>%
        mutate(out_migration_rate = (international_out_migrants + domestic_out_migrants)/at_risk_population) %>%
        left_join(ward_district, by="gss_code_ward") %>%
        select(gss_code_borough, gss_code_ward, sex, age, out_migration_rate)

#Take a look at the rates that are problematic
#The code fixes these in the lines below but if there are a lot of cases
#then perhaps some other approach is necessary
summary(probabilities$out_migration_rate)
View(filter(probabilities, !(out_migration_rate >= 0 & out_migration_rate <= 0.8)), "problem rates")

#Fix
probabilities <- mutate(probabilities, out_migration_rate = ifelse(out_migration_rate>0.8, 0.8, out_migration_rate))
probabilities <- mutate(probabilities, out_migration_rate = ifelse(out_migration_rate<=0, 1e-5, out_migration_rate))

#Checks
#Only save if these checks are passed
if(is.character(probabilities$gss_code_borough) &
   is.character(probabilities$gss_code_ward) &
   is.character(probabilities$sex) &
   is.numeric(probabilities$age) &
   is.numeric(probabilities$out_migration_rate) &
   min(probabilities$out_migration_rate) >= 0 &
   max(probabilities$out_migration_rate) != Inf &
   max(probabilities$out_migration_rate) <= 0.8 &
   nrow(probabilities) == 113568 &
   ncol(probabilities) == 5){
        #Save and overwrite base outmigration rates for ward model
        setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/ward inputs/")
        saveRDS(probabilities, "base_outmigration_rates.rds")
} else {
        message("WARNING: File not saved - error in data (see checks at the end of the script)")
}

summary(probabilities$out_migration_rate)

