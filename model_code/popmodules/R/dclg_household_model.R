library(dplyr)
library(data.table)
library(tidyr)

#RUN THE MODEL
stage_1 <- dclg_stage_1(population_projection, stage_1_file, stage_2_file, file_location)
stage_2 <- dclg_stage_2(stage_1[[1]])
dclg_outputs(stage_1[[2]], stage_1[[3]], stage_1[[4]], stage_1[[5]], stage_1[[6]],
             stage_2[[1]], stage_2[[2]])


#FUNCTIONS
extend_data <- function(stage1_data, max_dclg_year, max_projection_year){
        
        
        dclg_final_rates <- filter(stage1_data, year==max_dclg_year)
        dclg_extra_years <- vector("list", max_projection_year)
        
        if(max_projection_year > max_dclg_year){
                for(i in (max_dclg_year+1):max_projection_year){
                        
                        dclg_extra_years[[i]] <- dclg_final_rates %>%
                                mutate(year = i)
                        
                }
                
                dclg_extra_years <- rbindlist(dclg_extra_years) %>%
                        as.data.frame()
        }
        return(rbind(dclg_stage1_data, dclg_extra_years))
        
        
}


dclg_household_model <- function(population_projection, stage_1_file, stage_2_file, file_location){

        stage1_data <- readRDS(paste0(file_location,stage_1_file))
        headship_rates <- readRDS(paste0(file_location,stage_2_file))
        
        ### Group sya pop into 5 year bands up to 85+
        popn_5yr_bands <- population_projection %>%
                mutate(age_group =
                               cut(age,
                                   c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                                   c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44",
                                     "45_49","50_54","55_59","60_64","65_69","70_74","75_79","80_84","85&"),
                                   right=FALSE, include.lowest=TRUE)) %>%
                mutate(age_group = as.character(age_group)) %>%
                group_by(gss_code,year,sex,age_group) %>%
                summarise(popn_5yr_bands = sum(popn))
        
        ### Join all dclg inputs together
        ### calc implied dclg HH rep rates
        
        ### Create data for years beyond last dclg year ###
        
        stage1_data <- extend_data(stage1_data, max(stage1_data$year), max(population_projection$year))
        
        ### Aggregate populations by HH type
        dclg.summedhhtype <- stage1_data %>%
                group_by(gss_code,year,sex,age_group) %>%
                mutate(dclg_household_population.SummedHHtype = sum(dclg_household_population),
                       dclg_households.SummedHHtype = sum(dclg_households),
                       dclg_total_population.SummedHHtype = sum(dclg_total_population),
                       dclg_institutional_population.SummedHHtype = sum(dclg_institutional_population))
        
        
        ### Calc proportion in each HH type
        ### Multiply proportions by GLA popn
        dclg.gla <- left_join(dclg.summedhhtype, popn_5yr_bands,
                              by=c("gss_code", "year", "sex", "age_group")) %>%
                as.data.frame() %>%
                mutate( GLA.Total.popn = (dclg_total_population / dclg_total_population.SummedHHtype) * popn_5yr_bands,
                        GLA.Inst.popn = ifelse(age_group %in% c("75_79", "80_84", "85&"),
                                               (dclg_institutional_population / dclg_total_population) * GLA.Total.popn,
                                               dclg_institutional_population),
                        GLA.HH.popn = GLA.Total.popn - GLA.Inst.popn,
                        GLA.HH = ifelse(dclg_household_population > 0, GLA.HH.popn * hh_representative_rates, 0)) %>%
                
                mutate(GLA.Inst.popn = ifelse(GLA.Total.popn == 0, 0, GLA.Inst.popn),
                       GLA.HH.popn = ifelse(GLA.Total.popn == 0, 0, GLA.HH.popn))
        
        
        ### Total households by borough and year
        
        households <- group_by(dclg.gla, gss_code, year) %>%
                summarise(popn.Implied.HH = sum(GLA.HH)) %>%
                as.data.frame() %>%
                left_join(la_lookup, by="gss_code") %>%
                arrange(output_order) %>%
                select(gss_code, district, year, popn.Implied.HH) %>%
                filter(substr(gss_code,1,1)=="E")
        
        return_HH <- households
        
        ### Total households by region
        households_regional <- left_join(households, region_la_lookup, by="gss_code") %>%
                group_by(region, year) %>%
                summarise(popn.Implied.HH = sum(popn.Implied.HH))%>%
                select(region, year, popn.Implied.HH) %>%
                spread(year, popn.Implied.HH)
        
        
        #Spread Total hh table
        households <- households %>%
                spread(year, popn.Implied.HH)
        
        #Stage 1 output
        
        output.df <- left_join(dclg.gla, la_lookup, by="gss_code") %>%
                filter(substr(gss_code,1,1)=="E")
        
        stage1 <- output.df %>%
                select(gss_code, district, sex, household_type, age_group, year, GLA.HH)  %>%
                spread(year, GLA.HH)
        
        #Household population
        household_population <- output.df %>%
                select(gss_code, district, sex, household_type, age_group, year, GLA.HH.popn) %>%
                spread(year, GLA.HH.popn)
        
        
        #Institutional population
        institutional_population <- output.df %>%
                select(gss_code, district, sex, household_type, age_group, year, GLA.Inst.popn) %>%
                spread(year, GLA.Inst.popn)
        
        return(dclg.gla, households, stage_1, household_population, institutional_population, households_regional)
        
}

extend_rates <- function(dclg_headship_rates, max_dclg_year){
        
        dclg_final_rates <- filter(dclg_headship_rates, year==max_dclg_year)
        
        dclg_extra_years <- vector("list", max_projection_year)
        
        for(i in (max_dclg_year+1):max_projection_year){
                
                dclg_extra_years[[i]] <- dclg_final_rates %>%
                        mutate(year = i)
                
        }
        
        
        dclg_extra_years <- rbindlist(dclg_extra_years)
        
        return(as.data.frame(rbind(dclg_headship_rates, dclg_extra_years)))
}

dclg_stage_2 <- function(dclg_headship_rates, dclg.gla){

        extend_rates <- (dclg_headship_rates, max(dclg_headship_rates$year))
        
        age.change <- data.frame(
                age_group = c("15_19","20_24","25_29","30_34","35_39","40_44","45_49","50_54","55_59",
                              "60_64","65_69","70_74","75_79","80_84","85&"),
                dclg.bands = c("15_24","15_24","25_34","25_34","35_44","35_44","45_54","45_54","55_59",
                               "60_64","65_74","65_74","75_84","75_84","85&"),
                stringsAsFactors=F)
        
        tot.HH.stg.1 <- dclg.gla %>%
                filter(substr(gss_code,1,1)=="E") %>%
                group_by(gss_code, year) %>%
                summarise(stg1.HH = sum(GLA.HH))
        
        
        HH.pop.over14 <- dclg.gla %>%
                filter(!age_group %in% c("0_4","5_9","10_14")) %>%
                filter(substr(gss_code,1,1)=="E") %>%
                left_join(age.change, by="age_group")%>%
                select(-age_group) %>%
                setnames("dclg.bands", "age_group") %>%
                group_by(gss_code,year,age_group) %>%
                summarise(HH.popn = sum(GLA.HH.popn))
        
        
        stage2.unconstrained <- left_join(HH.pop.over14, la_lookup, by="gss_code")%>%
                left_join(dclg_headship_rates, by=c("gss_code","district","year","age_group")) %>%
                mutate(stg2.HH.uncon = HH.popn * DCLG.rate) %>%
                select(-DCLG.rate, -HH.popn, -output_order) %>%
                filter(substr(gss_code,1,3)=="E09")
        
        stage2.constrained <- group_by(stage2.unconstrained, gss_code, year) %>%
                mutate(stg2.total = sum(stg2.HH.uncon)) %>%
                as.data.frame() %>%
                mutate(Factor = stg2.HH.uncon / stg2.total) %>%
                left_join(tot.HH.stg.1, by = c("gss_code","year"))  %>%
                mutate(stg2.HH = Factor * stg1.HH)
        
        stage2 <- stage2.constrained %>%
                select(gss_code, year, district, age_group, household_type, stg2.HH) %>%
                spread(year, stg2.HH)
        
        
        AHS <- left_join(la_lookup, dclg.gla, by="gss_code") %>%
                filter(substr(gss_code,1,1)=="E") %>%
                group_by(gss_code, district, year) %>%
                summarise(HH.popn = sum(GLA.HH.popn), Households=sum(GLA.HH)) %>%
                as.data.frame() %>%
                mutate(AHS = HH.popn / Households)
        
        return(list(stage_2, AHS))
        
}

dclg_outputs <- function(households, stage_1, household_population, institutional_population,
                         households_regional, stage_2, AHS, output_location){
        ### Output files
        setwd(output_location)
        
        fwrite(households, "Households - district Totals.csv", quote = TRUE)
        fwrite(households_regional, "Households - Regional Totals.csv", quote = TRUE)
        
        fwrite(household_population, "Household population.csv", quote = TRUE)
        fwrite(institutional_population, "Institutional population.csv", quote = TRUE)
        
        fwrite(stage1, "Households - Stage 1.csv", quote = TRUE)
        fwrite(stage2, "Households - Stage 2.csv", quote = TRUE)
        fwrite(AHS, "Households - AHS.csv", quote = TRUE)
        
        
}


