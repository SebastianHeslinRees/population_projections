#' Output Stage 1 and Stage 2 Househoilds
#'
#' This is Sunny's code with a few superficial tweaks.
#' @param population Dataframe. SYA population output from the population_projection loop.
#' @param la_la_lookup Dataframe. Local authority code to name la_lookup with added output order.
#' @param region_la_lookup Dataframe. Local authority to region la_lookup.
#' @export


Output.DCLG.HH <- function(population_projection, la_lookup, region_la_lookup){
        
        library(dplyr); library(data.table); library(tidyr)
        
        la_lookup <- data.frame(la_lookup)
        
        ### DCLG data inputs ###
        if(user_selections$household.model == "2012 DCLG"){
                dclg_stage1_data <- readRDS(paste0(root.dir,"/Inputs/2012 DCLG Stage 1 data.rds"))
                dclg_headship_rates <- readRDS(paste0(root.dir,"/Inputs/2012 DCLG Stage 2 headship rates.rds"))
                
        }
        
        if(user_selections$household.model == "2014 DCLG"){
                dclg_stage1_data <- readRDS(paste0(root.dir,"/Inputs/2014 DCLG Stage 1 data.rds"))
                dclg_headship_rates <- readRDS(paste0(root.dir,"/Inputs/2014 DCLG Stage 2 headship rates.rds"))
        }
        
        
        
        proj.years <- c(min(population_projection$year):max(population_projection$year))
        
        
        
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
        max_dclg_year <- max(dclg_stage1_data$year)
        max_projection_year <- max(population_projection$year)
        
        dclg_final_rates <- filter(dclg_stage1_data, year==max_dclg_year)
        dclg_extra_years <- vector("list", max_projection_year)
        
        if(max_projection_year > max_dclg_year){
                for(i in (max_dclg_year+1):max_projection_year){
                        
                        dclg_extra_years[[i]] <- dclg_final_rates %>%
                                mutate(year = i)
                        
                }
                
                dclg_extra_years <- rbindlist(dclg_extra_years) %>%
                        as.data.frame()
                
                dclg_stage1_data <- rbind(dclg_stage1_data, dclg_extra_years)
                
        }
        
        
        ### Aggregate populations by HH type
        dclg.summedhhtype <- group_by(dclg_stage1_data, gss_code,year,sex,age_group) %>%
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
        
        
        #### STAGE 2 ####
        
        ### Create data for years beyond 2037 ###
        max_dclg_year <- max(dclg_headship_rates$year)
        
        dclg_final_rates <- filter(dclg_headship_rates, year==max_dclg_year)
        dclg_extra_years <- vector("list", max_projection_year)
        
        if(max(population_projection$year > max_dclg_year)){
                for(i in (max_dclg_year+1):max_projection_year){
                        
                        dclg_extra_years[[i]] <- dclg_final_rates %>%
                                mutate(year = i)
                        
                }
                dclg_extra_years <- rbindlist(dclg_extra_years)
                dclg_headship_rates <- as.data.frame(rbind(dclg_headship_rates, dclg_extra_years))
        }
        
        
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
        
        ### Output files
        dclg.dir <- paste0(dir.nm,"/DCLG Households")
        dir.create(dclg.dir, showWarnings = FALSE)
        setwd(dclg.dir)
        
        fwrite(households, "Households - district Totals.csv", quote = TRUE)
        fwrite(households_regional, "Households - Regional Totals.csv", quote = TRUE)
        
        fwrite(household_population, "Household population.csv", quote = TRUE)
        fwrite(institutional_population, "Institutional population.csv", quote = TRUE)
        
        fwrite(stage1, "Households - Stage 1.csv", quote = TRUE)
        fwrite(stage2, "Households - Stage 2.csv", quote = TRUE)
        fwrite(AHS, "Households - AHS.csv", quote = TRUE)
        
        setwd(dir.nm)
        #return(setnames(return_HH, "popn.Implied.HH", "households"))
        
}

