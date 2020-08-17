library(data.table)
library(dplyr)
library(tidyr)
library(popmodules)

#Set these to the location of the final borough components
mye_popn_path <- "input_data/mye/2018/population_gla.rds"
borough_dom_in_path <- "input_data/domestic_migration/2018/domestic_migration_in.rds"
borough_dom_out_path <- "input_data/domestic_migration/2018/domestic_migration_out.rds"
borough_int_out_path <- "input_data/mye/2018/international_out_gla.rds"
borough_int_in_path <- "input_data/mye/2018/international_in_gla.rds"
borough_deaths_path <- "input_data/mye/2018/deaths_ons.rds"

#census data
census_foreign_born_path <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/LC2103EW_COB_MSOA.csv"
census_data_dir <- "Q:/Teams/D&PA/Data/census_tables/small_area_model/"

msoa_dom_out_male_path <- "CT0543_male_domestic_out_from_msoa.csv"
msoa_dom_out_female_path <- "CT0544_female_domestic_out_from_msoa.csv"

msoa_dom_in_male_path <- "CT0498_male_domestic_in_to_msoa.csv"
msoa_dom_in_female_path <- "CT0499_female_domestic_in_to_msoa.csv"

msoa_int_in_male_path <- "CT0496_male_international_in_to_msoa.csv"
msoa_int_in_female_path <- "CT0497_female_international_in_to_msoa.csv"

#pre-processed msoa model inputs
msoa_births_path <- "input_data/small_area_model/msoa_births.rds"
msoa_deaths_path <- "input_data/small_area_model/msoa_deaths.rds"
msoa_popn_path <- "input_data/small_area_model/msoa_population_estimates.rds"

#lookup
msoa_to_district <- readRDS("input_data/lookup/msoa_to_district.rds") %>%
        select(-msoa_name)
london_msoas <- filter(msoa_to_district, grepl("E09", gss_code))$gss_code_msoa


####DOMESTIC OUT####
#Data come from the census
#Data only goes to age 75 so ages 75-90 are modelled using borough distribution

borough_domestic_out <- readRDS(borough_dom_out_path) %>% filter(year == 2011) %>% filter_to_LAs()

dom_out_male <- data.table::fread(paste0(census_data_dir,msoa_dom_out_male_path), header = T) %>%
        as.data.frame() %>%
        mutate(gss_code_msoa = substr(area,1,9),
               sex = "male") %>%
        pivot_longer(cols = as.character(1:75), names_to = "age",
                     values_to = "domestic_out_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_msoa, sex, age, domestic_out_migrants) %>%
        as.data.frame()

dom_out_female <- data.table::fread(paste0(census_data_dir,msoa_dom_out_female_path), header = T) %>%
        as.data.frame() %>%
        mutate(gss_code_msoa = substr(area,1,9),
               sex = "female") %>%
        pivot_longer(cols = as.character(1:75), names_to = "age",
                     values_to = "domestic_out_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_msoa, sex, age, domestic_out_migrants) %>%
        as.data.frame()

domestic_out <- rbind(dom_out_female, dom_out_male) %>%
        filter(gss_code_msoa %in% london_msoas)

#There are no 0 year olds in the msoa data
#Take the borough 0 year olds and distribute to msoas
#MSOA dist is the same as the 1 year old dist
borough_zero_out <- borough_domestic_out %>%
        filter(age == 0) %>%
        rename(borough_flow = dom_out)

age_0_out <- filter(domestic_out, age == 1)  %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        group_by(gss_code, sex) %>%
        mutate(borough_total = sum(domestic_out_migrants)) %>%
        data.frame() %>%
        mutate(b_proportion = domestic_out_migrants / borough_total) %>%
        mutate(age = 0) %>%
        left_join(borough_zero_out, by=c("gss_code","sex","age")) %>%
        mutate(modelled_flow = b_proportion * borough_flow) %>%
        select(gss_code_msoa, sex, age, modelled_flow) %>%
        setnames(names(domestic_out))

domestic_out <- filter(domestic_out, age == 75) %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        distribute_within_age_band(popn_2 = borough_domestic_out,
                                   popn_1_col = "domestic_out_migrants",
                                   popn_2_col = "dom_out",
                                   min_age=75, max_age=90,
                                   col_aggregation=c("gss_code","sex")) %>%
        select(names(domestic_out)) %>%
        rbind(filter(domestic_out, age != 75),
              age_0_out) %>%
        arrange(age, sex, gss_code_msoa)

#census to mid-year
domestic_out <- left_join(domestic_out, msoa_to_district, by="gss_code_msoa") %>%
        constrain_component(borough_domestic_out,
                            col_aggregation = c("gss_code","sex","age"),
                            col_popn = "domestic_out_migrants",
                            col_constraint = "dom_out")

####DOMESTIC IN####
borough_domestic_in <- readRDS(borough_dom_in_path) %>% filter(year == 2011) %>% filter_to_LAs()

dom_in_male <- data.table::fread(paste0(census_data_dir,msoa_dom_in_male_path), header = T) %>%
        as.data.frame() %>%
        mutate(gss_code_msoa = substr(area,1,9),
               sex = "male") %>%
        pivot_longer(cols = as.character(1:75), names_to = "age",
                     values_to = "domestic_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_msoa, sex, age, domestic_in_migrants) %>%
        as.data.frame()

dom_in_female <- data.table::fread(paste0(census_data_dir,msoa_dom_in_female_path), header = T) %>%
        as.data.frame() %>%
        mutate(gss_code_msoa = substr(area,1,9),
               sex = "female") %>%
        pivot_longer(cols = as.character(1:75), names_to = "age",
                     values_to = "domestic_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_msoa, sex, age, domestic_in_migrants) %>%
        as.data.frame()

domestic_in <- rbind(dom_in_female, dom_in_male) %>%
        filter(gss_code_msoa %in% london_msoas)

borough_zero_in <- borough_domestic_in %>%
        filter(age == 0) %>%
        rename(borough_flow = dom_in)

age_0_in <- filter(domestic_in, age == 1)  %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        group_by(gss_code, sex) %>%
        mutate(borough_total = sum(domestic_in_migrants)) %>%
        data.frame() %>%
        mutate(b_proportion = domestic_in_migrants / borough_total) %>%
        mutate(age = 0) %>%
        left_join(borough_zero_in, by=c("gss_code","sex","age")) %>%
        mutate(modelled_flow = b_proportion * borough_flow) %>%
        select(gss_code_msoa, sex, age, modelled_flow) %>%
        setnames(names(domestic_in))

domestic_in <- filter(domestic_in, age == 75) %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        distribute_within_age_band(popn_2 = borough_domestic_in,
                                   popn_1_col = "domestic_in_migrants",
                                   popn_2_col = "dom_in",
                                   min_age=75, max_age=90,
                                   col_aggregation=c("gss_code","sex")) %>%
        select(names(domestic_in)) %>%
        rbind(filter(domestic_in, age != 75),
              age_0_in) %>%
        arrange(age, sex, gss_code_msoa)

#census to mid-year
domestic_in <- left_join(domestic_in, msoa_to_district, by="gss_code_msoa") %>%
        constrain_component(borough_domestic_in,
                            col_aggregation = c("gss_code","sex","age"),
                            col_popn = "domestic_in_migrants",
                            col_constraint = "dom_in")

####NTERNATIONAL OUT####
#Data from census on % of the borough's non-uk-born pop in each msoa in that borough
#Apply that percentage to the international out flow for 2011 from the MYE CoC

foreign_born <- data.table::fread(census_foreign_born_path) %>%
        as.data.frame()%>%
        mutate(male = male_total - male_ukborn,
               female = female_total - female_ukborn) %>%
        select(gss_code_msoa, male, female) %>%
        pivot_longer(cols = c(male, female),
                     names_to = "sex",
                     values_to = "nonUKborn") %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        as.data.frame() %>%
        group_by(gss_code, sex) %>%
        mutate(borough_nonUKborn = sum(nonUKborn),
               nonUKborn_proportion = nonUKborn/borough_nonUKborn) %>%
        as.data.frame() %>%
        select(gss_code_msoa, gss_code, sex, nonUKborn_proportion)

international_out <- readRDS(borough_int_out_path) %>%
        filter(year==2011) %>%
        select(gss_code, sex, age, int_out) %>%
        right_join(foreign_born, by=c("gss_code","sex")) %>%
        mutate(international_out_migrants = int_out * nonUKborn_proportion) %>%
        select(gss_code_msoa, gss_code, sex, age, international_out_migrants)

rm(foreign_born)

####INTERNATIONAL IN####
borough_international_in <- readRDS(borough_int_in_path) %>% filter(year == 2011)

int_in_male <- data.table::fread(paste0(census_data_dir,msoa_int_in_male_path), header = T) %>%
        as.data.frame() %>%
        mutate(gss_code_msoa = substr(area,1,9),
               sex = "male") %>%
        pivot_longer(cols = as.character(1:75), names_to = "age",
                     values_to = "international_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_msoa, sex, age, international_in_migrants) %>%
        as.data.frame()

int_in_female <- data.table::fread(paste0(census_data_dir,msoa_int_in_female_path), header = T) %>%
        as.data.frame() %>%
        mutate(gss_code_msoa = substr(area,1,9),
               sex = "female") %>%
        pivot_longer(cols = as.character(1:75), names_to = "age",
                     values_to = "international_in_migrants") %>%
        mutate(age = as.numeric(age)) %>%
        select(gss_code_msoa, sex, age, international_in_migrants) %>%
        as.data.frame()

international_in <- rbind(int_in_female, int_in_male) %>%
        filter(gss_code_msoa %in% london_msoas)

borough_zero_int_in <- borough_international_in %>%
        filter(age == 0) %>%
        rename(borough_flow = int_in)

age_0_int_in <- filter(international_in, age == 1)  %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        group_by(gss_code, sex) %>%
        mutate(borough_total = sum(international_in_migrants)) %>%
        data.frame() %>%
        mutate(b_proportion = international_in_migrants / borough_total) %>%
        mutate(age = 0) %>%
        left_join(borough_zero_int_in, by=c("gss_code","sex","age")) %>%
        mutate(modelled_flow = b_proportion * borough_flow) %>%
        select(gss_code_msoa, sex, age, modelled_flow) %>%
        setnames(names(international_in))

international_in <- filter(international_in, age == 75) %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        distribute_within_age_band(popn_2 = borough_international_in,
                                   popn_1_col = "international_in_migrants",
                                   popn_2_col = "int_in",
                                   min_age=75, max_age=90,
                                   col_aggregation=c("gss_code","sex")) %>%
        select(names(international_in)) %>%
        rbind(filter(international_in, age != 75),
              age_0_int_in) %>%
        arrange(age, sex, gss_code_msoa)

#census to mid-year
international_in <- left_join(international_in, msoa_to_district, by="gss_code_msoa") %>%
        constrain_component(borough_international_in,
                            col_aggregation = c("gss_code","sex","age"),
                            col_popn = "international_in_migrants",
                            col_constraint = "int_in")

####Out migration rates####
msoa_births_2011 <- readRDS(msoa_births_path) %>%
        filter(year == 2011, gss_code_msoa %in% london_msoas) %>%
        group_by(year, gss_code_msoa) %>%
        summarise(births = sum(births)) %>%
        as.data.frame() %>%
        mutate(age = 0,
               male = births * (105/205),
               female = births - male) %>%
        select(-births) %>%
        pivot_longer(c("male", "female"), values_to = "births", names_to = "sex") %>%
        select(year, gss_code_msoa, sex, age, popn) %>% 
        data.frame()

borough_deaths <- readRDS(borough_deaths_path) %>%
        filter(substr(gss_code,1,3)=="E09",
               year == 2011)

msoa_deaths_2011 <- readRDS(msoa_deaths_path) %>%
        filter(year == 2011, gss_code_msoa %in% london_msoas) %>%
        as.data.frame()  %>%
        left_join(msoa_to_district, by="gss_code_msoa")

age_groups <- unique(msoa_deaths_2011$age_group)

minmax <- sapply(age_groups, strsplit, split = "_")
mn <- sapply(minmax, first)
mn <- ifelse(mn == "85+", 85, mn) %>% as.numeric()
mx <- sapply(minmax, last)
mx <- ifelse(mx == "85+", 90, mx) %>% as.numeric()

borough_deaths <- borough_deaths %>%
        mutate(min = sapply(age, function(x) max(mn[mn <= x])),
               max = sapply(age, function(x) min(mx[mx >= x])),
               age_group = paste(min, max, sep="_"),
               age_group = case_when(age_group == "0_0" ~ "0",
                                     age_group == "85_90" ~ "85+",
                                     TRUE ~ age_group)) %>%
        select(gss_code, year, sex, age, age_group, deaths_unconstrained = deaths)

msoa_deaths_2011 <- left_join(borough_deaths, msoa_to_district, by="gss_code") %>%
        constrain_component(msoa_deaths_2011,
                            col_aggregation = c("gss_code_msoa", "year", "sex", "age_group"),
                            col_popn = "deaths_unconstrained",
                            col_constraint = "deaths") %>%
        select(gss_code_msoa, gss_code, year, sex, age, deaths = deaths_unconstrained) %>%
        as.data.frame()

msoa_popn_2010 <- readRDS(msoa_popn_path) %>% filter(year == 2010) %>%
        select(-gss_code)

denominators <- msoa_popn_2010 %>%
        as.data.frame() %>%
        popn_age_on(col_aggregation = c("year", "gss_code_msoa", "sex", "age"),
                    births = msoa_births_2011) %>%
        left_join(msoa_deaths_2011, by = c("year","gss_code_msoa","sex","age")) %>%
        mutate(popn = popn - deaths) %>%
        select(-deaths) %>%
        check_negative_values("popn")

out_migration_rates <- left_join(domestic_out, international_out,
                                 by=c("gss_code_msoa","sex","age")) %>%
        filter(gss_code_msoa %in% london_msoas) %>%
        mutate(out_migrants = domestic_out_migrants + international_out_migrants) %>%
        left_join(denominators, by=c("gss_code_msoa","sex","age")) %>%
        mutate(out_migration_rate = ifelse(popn == 0, 0, out_migrants/popn))%>%
        select(gss_code_msoa, sex, age, out_migration_rate) %>%
        arrange(gss_code_msoa, sex, age) %>%
        as.data.frame()

ix <- out_migration_rates$out_migration_rate > 0.8
if(any(ix)) {
        warning(paste(sum(ix), "outmigration levels had rates > 0.8, these will be capped."))
}

out_migration_rates <- out_migration_rates %>%
        mutate(out_migration_rate = ifelse(out_migration_rate > 0.8, 0.8, out_migration_rate))


####In migration distribution####
in_migration_characteristics <- left_join(domestic_in, international_in,
                                          by=c("gss_code_msoa","sex","age")) %>%
        mutate(all_in_migration = domestic_in_migrants + international_in_migrants) %>%
        group_by(gss_code_msoa) %>%
        mutate(adult_only = ifelse(age >= 18, all_in_migration, 0)) %>%
        mutate(final_dist = all_in_migration / sum(adult_only)) %>%
        as.data.frame() %>%
        select(gss_code_msoa, sex, age, final_dist) %>%
        rename(in_migration_rate = final_dist) %>%
        arrange(gss_code_msoa, sex, age) %>%
        as.data.frame() %>%
        left_join(msoa_to_district, by="gss_code_msoa") %>%
        filter(substr(gss_code,1,3)=="E09") %>%
        select(-gss_code)

#Save
saveRDS(in_migration_characteristics, "input_data/small_area_model/msoa_in_migration_characteristics.rds")
saveRDS(out_migration_rates, "input_data/small_area_model/msoa_out_migration_rates.rds")
