get_household_data <- function(file_location, projection){
        
        library(dplyr)
        library(tidyr)
        library(data.table)
        
        #census data Populations
        
        read_hh_data_files <- function(file, file_location, year){
                
                x <- readRDS(paste0(file_location, file)) %>%
                        filter(year == year)
                return(x)
        }
        
        rates_2001 <- read_hh_data_files("/household_representative_rates.rds", file_location, 2001)
        rates_2011 <- read_hh_data_files("/household_representative_rates.rds", file_location, 2011)
        
        ce_2001 <- read_hh_data_files("/communal_establishment_population.rds", file_location, 2001)
        ce_2011 <- read_hh_data_files("/communal_establishment_population.rds", file_location, 2011)
        
        households_2001 <- read_hh_data_files("/households.rds", file_location, 2001)
        households_2011 <- read_hh_data_files("/households.rds", file_location, 2011)
        
        return(rates_2001, rates_2011, ce_2001, ce_2011, households_2001, households_2011, projection)
}

aggregate_geography <- function(projection){
        
        projection <- filter(projection, substr(gss_code,1,1)=="E")
        
        #TODO Get a region lookup
        
        
        projection <- projection %>%
                mutate(gss_code = case_when(gss_code == "E09000033" ~ "E090000001",
                                            gss_code == "E06000052" ~ "E06000053"))
        group_by(gss_code, sex, age, year) %>%
                summarise_all(funs(sum)) %>%
                as.data.frame
        
        eng <- projection %>%
                mutate(gss_code = "E92000001") %>%
                group_by(gss_code, sex, age, year) %>%
                summarise_all(funs(sum)) %>%
                as.data.frame()
        
        reg <- projection %>%
                left_join(region_lk, by="gss_code") %>%
                select(-gss_code, -district) %>%
                rename(gss_code=region_gss_code) %>%
                group_by(gss_code, sex, age, year) %>%
                summarise_all(funs(sum)) %>%
                as.data.frame()
        
        projection <- rbind(projection, reg, eng)
        
}

calculate_hh_population <- function(ce_2001, ce_2011, population){
        
        household_pop <- vector("list",2041)
        communal_pop <- vector("list",2041)
        
        for(y in 2001:2041){
                
                communal <- if(y<2011){ce_2001}else{ce_2011}
                
                df <- filter(population, year == y) %>%
                        left_join(select(communal, -year),
                                  by = c("gss_code", "sex", "age_group")) %>%
                        mutate(hh_pop = ifelse(is.na(ce_rate), popn - ce_pop,
                                               popn * (1-ce_rate)))  %>%
                        mutate(ce_pop = ifelse(is.na(ce_rate), ce_pop,
                                               popn * ce_rate))
                
                
                household_pop[[y]] <- df %>% select(gss_code, year, sex, age_group, hh_pop)
                
                communal_pop[[y]] <-  df %>%  select(gss_code, year, sex, age_group, ce_pop)
                
        }
        
        return(list(household_pop, communal_pop))
        
}

calculate_hh_rates <- function(rates_2001, rates_2011, households_2001, households_2011){
        
        rates <- vector("list", 2041)
        rates[[2001]] <- rates_2001
        rates[[2011]] <- rates_2011
        
        #       16 to 19-year-olds
        #       quinary age groups from 20-24 years to 85-89
        #       90 years and over.
        
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
        
        return(rates)
        
}

apply_rates <- function(household_pop, household_rates){
        household_projection <- vector("list", 2041)
        
        for(i in 2001:2041){
                
                household_projection[[i]] <- left_join(household_pop[[i]], rates[[i]],
                                                       by = c("gss_code", "year", "sex", "age_group")) %>%
                        mutate(households = hh_pop * HRR)
        }
        
        household_projection <- data.table::rbindlist(household_projection)
        
        return(household_projection)
        
}

constrain_regional_hh <- function(unconstrained_regional, england_proj){
        
        constraint_regional <- group_by(unconstrained_regional, year, sex, age_group) %>%
                summarise(reg_hh = sum(households)) %>%
                data.frame() %>%
                left_join(england_proj, by=c("year", "sex", "age_group")) %>%
                rename(eng_hh = households) %>%
                mutate(constr = ifelse(eng_hh == 0, 1, eng_hh/reg_hh)) %>%
                select(year, sex, age_group, constr)
        
        constrained_regional <-left_join(unconstrained_regional, constraint_regional, by=c("year","sex","age_group")) %>%
                mutate(households = households*constr) %>%
                select(-hh_pop, -HRR, -constr)
        
        return(constrained_regional)
        
}

constrain_district_hh <- function(unconstrained_la, constrained_regional){
        
        constraint_la <- left_join(unconstrained_la, region_lk, by="gss_code") %>%
                group_by(region_gss_code, year, sex, age_group) %>%
                summarise(la_hh = sum(households)) %>%
                data.frame() %>%
                left_join(constrained_regional, by=c("region_gss_code"="gss_code","year","sex","age_group")) %>%
                rename(reg_hh = households) %>%
                mutate(constr = ifelse(reg_hh == 0, 0, reg_hh/la_hh)) %>%
                select(region_gss_code, year, sex, age_group, constr)
        
        constrained_la <- left_join(unconstrained_la, region_lk, by="gss_code") %>%
                left_join(constraint_la, by=c("region_gss_code","year","sex","age_group")) %>%
                mutate(households = households*constr) %>%
                select(-hh_pop, -HRR, -constr, -region, -region_gss_code)
        
        return(constraint_la)
        
}

ons_household_model <- function(projection, file_location){
        
        hh_data <- get_household_data(file_location, projection)
        
        projection <- aggregate_geography(hh_data[[7]])
        
        #       Same number as 2011 for 0-74
        #       Same proportion 75+
        #       Prison population updated upto and inc 2016
        
        projection_age_groups <- projection %>%
                mutate(age_group = cut(age,
                                       breaks = c(0, 16,seq(19,84,5),Inf),
                                       
                                       include.lowest = T,
                                       
                                       labels = c("0_15","16_19","20_24","25_29","30_34","35_39","40_44",
                                                  "45_49","50_54","55_59","60_64","65_69",
                                                  "70_74","75_79","80_84","85_over")),
                       
                       age_group = as.character(age_group))%>%
                
                # filter(!is.na(age_group)) %>%
                group_by(gss_code, year, sex, age_group) %>%
                summarise(popn = sum(popn)) %>%
                data.frame()
        
        
        household_ce_pop <- calculate_hh_population(hh_data[[3]], hh_data[[4]], projection_age_groups)
        
        household_rates <- calculate_hh_rates(hh_data[[1]], hh_data[[2]], hh_data[[5]], hh_data[[6]])
        
        household_projection <- apply_rates(household_ce_pop[[1]], household_rates)
        
        
        #Constrain regions to England, LAs to regions
        unconstrained <- rbindlist(household_projection) %>% data.frame()
        unconstrained_regional <- filter(unconstrained, substr(gss_code,1,3)=="E12") 
        england_proj <- filter(unconstrained, substr(gss_code,1,3)=="E92")
        unconstrained_la <- filter(unconstrained, !substr(gss_code,1,3) %in% c("E12","E92"))
        
        #Regional constrained to England
        constrained_regional <- constrain_regional_hh(unconstrained_regional, england_proj)
        
        #Districts constrained to regions
        constrained_district <- constrain_district_hh(unconstrained_la, constrained_regional)
        
        return(list(unconstrained,
                    constrained = rbind(england_proj, constrained_regional, constrained_district),
                    household_population = data.table::rbindlist(household_ce_pop[[1]]),
                    communal_establishment_population = data.table::rbindlist(household_ce_pop[[2]]),
                    household_representative_rates = data.table::rbindlist(household_rates)))
        
}



summary_tbl <- left_join(constrained_proj, household_pop,
                         by=c("gss_code","year","sex","age_group")) %>%
        left_join(communal_pop,
                  by=c("gss_code","year","sex","age_group")) %>%
        mutate(total_pop = hh_pop + ce_pop) %>%
        select(-age_group, -sex) %>%
        data.frame() %>%
        group_by(gss_code, district, year) %>%
        summarise_all(funs(sum)) %>%
        data.frame() %>%
        mutate(ahs = hh_pop / households) %>%
        mutate(ahs = round(ahs, 3),
               total_pop = round(total_pop, 0),
               hh_pop = round(hh_pop, 0),
               ce_pop = round(ce_pop, 0),
               households = round(households, 0))




