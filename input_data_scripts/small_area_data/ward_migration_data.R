library(data.table)
library(dplyr)
library(tidyr)
library(popmodules)

message("ward migration data")

#Set these to the location of the final borough components
mye_popn_path <- "input_data/mye/2019/population_gla.rds"
borough_dom_in_path <- "input_data/mye/2020/dom_in_ons.rds"
borough_dom_out_path <- "input_data/mye/2020/dom_out_ons.rds"
borough_int_out_path <- "input_data/mye/2020/int_out_gla.rds"
borough_int_in_path <- "input_data/mye/2020/int_in_gla.rds"

#census data
census_foreign_born_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/LC2103EW_ward_country_of_birth.csv"
ward_dom_out_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/CT0356_london_wards_out_migration.csv"
ward_dom_in_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/CT0354_london_wards_in_migration.csv"
ward_in_migration_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/CT0409_london_wards_in_migration_inc_international.csv"

#pre-processed ward model inputs
ward_births_path <- "input_data/small_area_model/ward_data/ward_sya_births.rds"
ward_deaths_path <- "input_data/small_area_model/ward_data/ward_sya_deaths.rds"
ward_popn_path <- "input_data/small_area_model/ward_data/ward_population_estimates.rds"

#lookup
ward_to_district <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>%
        select(-ward_name)

####DOMESTIC OUT####
#Data come from the census
#Data only goes to age 75 so ages 75-90 are modeled using borough distribution

borough_domestic_out <- readRDS(borough_dom_out_path) %>%
        filter(year == 2011) %>%
        filter_to_LAs()

domestic_out <- data.table::fread(ward_dom_out_path, header = T) %>%
        pivot_longer(cols = as.character(0:75), names_to = "age",
                     values_to = "domestic_out_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_ward, sex, age, domestic_out_migrants) %>%
        as.data.frame()

domestic_out <- filter(domestic_out, age == 75) %>%
        left_join(ward_to_district, by="gss_code_ward") %>%
        distribute_within_age_band(popn_2 = borough_domestic_out,
                                   popn_1_col = "domestic_out_migrants",
                                   popn_2_col = "dom_out",
                                   min_age=75, max_age=90,
                                   col_aggregation=c("gss_code","sex")) %>%
        select(names(domestic_out)) %>%
        rbind(filter(domestic_out, age != 75))

#census to mid-year
domestic_out <- left_join(domestic_out, ward_to_district, by="gss_code_ward") %>%
        constrain_component(borough_domestic_out,
                            col_aggregation = c("gss_code","sex","age"),
                            col_popn = "domestic_out_migrants",
                            col_constraint = "dom_out")

####DOMESTIC IN####
borough_domestic_in <- readRDS(borough_dom_in_path) %>% filter(year == 2011) %>% filter_to_LAs()

domestic_in <- data.table::fread(ward_dom_in_path, header = T) %>%
        pivot_longer(cols = as.character(0:75), names_to = "age",
                     values_to = "domestic_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_ward, sex, age, domestic_in_migrants) %>%
        as.data.frame()

domestic_in <- filter(domestic_in, age == 75) %>%
        left_join(ward_to_district, by="gss_code_ward") %>%
        distribute_within_age_band(popn_2 = borough_domestic_in,
                                   popn_1_col = "domestic_in_migrants",
                                   popn_2_col = "dom_in",
                                   min_age=75, max_age=90,
                                   col_aggregation=c("gss_code","sex")) %>%
        select(names(domestic_in)) %>%
        rbind(filter(domestic_in, age != 75))

#census to mid-year
domestic_in <- left_join(domestic_in, ward_to_district, by="gss_code_ward") %>%
        constrain_component(borough_domestic_in,
                            col_aggregation = c("gss_code","sex","age"),
                            col_popn = "domestic_in_migrants",
                            col_constraint = "dom_in")

####NTERNATIONAL OUT####
#Data from census on % of the borough's non-uk-born pop in each ward in that borough
#Apply that percentage to the international out flow for 2011 from the MYE CoC

foreign_born <- data.table::fread(census_foreign_born_path) %>%
        as.data.frame()%>%
        mutate(male = male_total - male_ukborn,
               female = female_total - female_ukborn) %>%
        select(gss_code_ward, male, female) %>%
        pivot_longer(cols = c(male, female),
                     names_to = "sex",
                     values_to = "nonUKborn") %>%
        left_join(ward_to_district, by="gss_code_ward") %>%
        aggregate_city_wards("nonUKborn") %>%
        as.data.frame() %>%
        group_by(gss_code, sex) %>%
        mutate(borough_nonUKborn = sum(nonUKborn),
               nonUKborn_proportion = nonUKborn/borough_nonUKborn) %>%
        as.data.frame() %>%
        select(gss_code_ward, gss_code, sex, nonUKborn_proportion) 

international_out <- readRDS(borough_int_out_path) %>%
        filter(year==2011) %>%
        select(gss_code, sex, age, int_out) %>%
        right_join(foreign_born, by=c("gss_code","sex")) %>%
        mutate(international_out_migrants = int_out * nonUKborn_proportion) %>%
        select(gss_code_ward, gss_code, sex, age, international_out_migrants)

rm(foreign_born)

####INTERNATIONAL IN####
borough_international_in <- readRDS(borough_int_in_path) %>% filter(year == 2011)

ward_all_in <- data.table::fread(ward_in_migration_path, header = T) %>%
        pivot_longer(cols = as.character(0:75), names_to = "age",
                     values_to = "all_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_ward, sex, age, all_in_migrants)

#Subtract the domestic from the total
ward_int_in <- domestic_in %>%
        mutate(age = ifelse(age>75, 75, age)) %>%
        group_by(gss_code_ward, sex, age) %>%
        summarise(domestic = sum(domestic_in_migrants), .groups = 'drop_last') %>%
        data.frame() %>%
        left_join(ward_all_in, by=c("gss_code_ward","sex","age")) %>%
        mutate(international = all_in_migrants - domestic) %>%
        check_negative_values("international")

#Distribute the over 75s to sya
ward_int_in <- filter(ward_int_in, age == 75) %>%
        left_join(ward_to_district, by="gss_code_ward") %>%
        distribute_within_age_band(popn_2 = borough_international_in,
                                   popn_1_col = "international",
                                   popn_2_col = "int_in",
                                   min_age=75, max_age=90,
                                   col_aggregation=c("gss_code","sex")) %>%
        select(names(ward_int_in)) %>%
        rbind(filter(ward_int_in, age != 75))

#constrain to the borough-level mye
international_in <- ward_int_in %>%
        left_join(ward_to_district, by="gss_code_ward") %>%
        constrain_component(constraint = borough_international_in,
                            col_aggregation = c("gss_code","sex","age"),
                            col_popn = "international",
                            col_constraint = "int_in")

rm(borough_international_in, ward_int_in, ward_all_in)

####Out migration rates####
london_wards <- filter(ward_to_district, grepl("E09", gss_code))$gss_code_ward

ward_births_2011 <- readRDS(ward_births_path) %>%
        filter(year == 2011) %>% 
        select(year, gss_code_ward, sex, age, births) 

ward_deaths_2011 <- readRDS(ward_deaths_path) %>%
        filter(year == 2011, gss_code_ward %in% london_wards) %>%
        as.data.frame()  %>%
        left_join(ward_to_district, by="gss_code_ward")

ward_popn_2010 <- readRDS(ward_popn_path) %>%
        filter(year == 2010) %>%
        select(-gss_code)

denominators <- ward_popn_2010 %>%
        as.data.frame() %>%
        popn_age_on(col_aggregation = c("year", "gss_code_ward", "sex", "age"),
                    births = ward_births_2011) %>%
        left_join(ward_deaths_2011, by = c("year","gss_code_ward","sex","age")) %>%
        rename(start_popn = popn) %>%
        mutate(popn = start_popn - deaths) %>%
        select(-deaths, -start_popn) %>%
        check_negative_values("popn")

out_migration_rates <- left_join(domestic_out, international_out,
                                 by=c("gss_code_ward","sex","age")) %>%
        filter(gss_code_ward %in% london_wards) %>%
        mutate(out_migrants = domestic_out_migrants + international_out_migrants) %>%
        left_join(denominators, by=c("gss_code_ward","sex","age")) %>%
        mutate(out_migration_rate = ifelse(popn == 0, 0, out_migrants/popn))%>%
        select(gss_code_ward, sex, age, out_migration_rate) %>%
        arrange(gss_code_ward, sex, age) %>%
        as.data.frame()

ix <- out_migration_rates$out_migration_rate > 0.8
if(any(ix)) {
        warning(paste(sum(ix), "outmigration levels had rates > 0.8, these will be capped."))
        
        warning(paste0(capture.output({
                print(paste0("Outmigration levels had rates > 0.8 at ",
                             sum(ix), " aggregation levels. These will be capped."))
                if(sum(ix) < 30) {
                        print("Values:")
                        print(out_migration_rates[ix,])
                } else {
                        print("First 30 values:")
                        print(out_migration_rates[ix,][1:30,])
                }
        }), collapse = "\n"))
}        

out_migration_rates <- out_migration_rates %>%
        mutate(out_migration_rate = ifelse(out_migration_rate > 0.8, 0.8, out_migration_rate))


####In migration distribution####
in_migration_characteristics <- left_join(domestic_in, international_in,
                                          by=c("gss_code_ward","sex","age")) %>%
        mutate(all_in_migration = domestic_in_migrants + international) %>%
        group_by(gss_code_ward) %>%
        mutate(adult_only = ifelse(age >= 18, all_in_migration, 0)) %>%
        mutate(final_dist = all_in_migration / sum(adult_only)) %>%
        as.data.frame() %>%
        select(gss_code_ward, sex, age, final_dist) %>%
        rename(in_migration_rate = final_dist) %>%
        arrange(gss_code_ward, sex, age) %>%
        as.data.frame()

#Save
saveRDS(in_migration_characteristics, "input_data/small_area_model/ward_data/ward_in_migration_characteristics.rds")
saveRDS(out_migration_rates, "input_data/small_area_model/ward_data/ward_out_migration_rates.rds")

rm(list = ls())
