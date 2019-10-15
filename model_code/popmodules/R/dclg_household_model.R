library(dplyr)
library(data.table)
library(tidyr)

# dclg_outputs(stage_1[[2]], stage_1[[3]], stage_1[[4]], stage_1[[5]], stage_1[[6]],
#              stage_2[[1]], stage_2[[2]])


#FUNCTIONS



dclg_stage_1 <- function(population_projection, stage1_file, file_location){

  stage1_data <- readRDS(paste0(file_location,stage1_file))

  ### Group sya pop into 5 year bands up to 85+
  popn_5yr_bands <-  population_into_age_groups(population_projection,
                                                age_groups=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                                                labels=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44",
                                                         "45_49","50_54","55_59","60_64","65_69","70_74","75_79","80_84","85&"),
                                                popn_col="popn") %>%
    rename(popn_5yr_bands = popn)

  ### Join all dclg inputs together
  ### calc implied dclg HH rep rates

  ### Create data for years beyond last dclg year ###
  stage1_data <- extend_data(stage1_data, max(population_projection$year))

  ### Aggregate populations by HH type
  sum_hh_type <- stage1_data %>%
    group_by(gss_code,year,sex,age_group) %>%
    mutate(sum_hh_popn = sum(household_population),
           sum_households = sum(households),
           sum_total_popn = sum(total_population),
           sum_ce_popn = sum(institutional_population)) %>%
    ungroup()


  ### Calc proportion in each HH type
  ### Multiply proportions by GLA popn
  scaled_projection <- left_join(sum_hh_type, popn_5yr_bands, by=c("gss_code", "year", "sex", "age_group")) %>%
    mutate(scaled_popn = (total_population / sum_total_popn) * popn_5yr_bands)

  scaled_communal_popn <- scaled_projection %>%
    mutate(scaled_ce_popn = ifelse(age_group %in% c("75_79", "80_84", "85&"),
                                   (institutional_population / total_population) * scaled_popn,
                                   institutional_population),
           scaled_ce_popn = ifelse(scaled_popn == 0, 0, scaled_ce_popn)) %>%
    select(-total_population, -institutional_population)

  scaled_household_popn <- scaled_communal_popn %>%
    mutate(scaled_hh_popn = scaled_popn - scaled_ce_popn)

  scaled_households <- scaled_household_popn %>%
    mutate(scaled_households = ifelse(scaled_hh_popn > 0,
                                      scaled_hh_popn * hh_rep_rates,
                                      0))


  ### Total households by borough and year

  households <- scaled_households %>%
    group_by(gss_code, year) %>%
    summarise(stage_1_households = sum(scaled_households)) %>%
    ungroup()

  ### Total households by region
  # households_regional <- left_join(households, region_la_lookup, by="gss_code") %>%
  #   group_by(region, year) %>%
  #   summarise(households = sum(scaled_households))%>%
  #   select(region, year, households)


  return(list(scaled_households, households))

}

#---------------------------------------------------------



dclg_stage_2 <- function(headship_rates_file, file_location, stage1_output){

  headship_rates <- readRDS(paste0(file_location, headship_rates_file))

  scaled_households <- stage1_output[[1]]
  stg1_total_households <- stage1_output[[2]]

  headship_rates <- extend_data(headship_rates, max(scaled_households$year))

  recoding_ages <- c("15_19" = "15_24",
                     "20_24" = "15_24",
                     "25_29" = "25_34",
                     "30_34" = "25_34",
                     "35_39" = "35_44",
                     "40_44" = "35_44",
                     "45_49" = "45_54",
                     "50_54" = "45_54",
                     "65_69" = "65_74",
                     "70_74" = "65_74",
                     "75_79" = "75_84",
                     "80_84" = "75_84")


  adult_hh_pop <- scaled_households %>%
    filter(gss_code %in% unique(headship_rates$gss_code)) %>%
    filter(!age_group %in% c("0_4","5_9","10_14")) %>%
    #filter(substr(gss_code,1,1)=="E") %>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(hh_popn = sum(scaled_hh_popn)) %>%
    ungroup()


  stage2_unconstrained <- left_join(adult_hh_pop, headship_rates, by=c("gss_code","year","age_group")) %>%
    mutate(hh_stg2_unconstrained = hh_popn * rate)

  stage2_constrained <- group_by(stage2_unconstrained, gss_code, year) %>%
    mutate(stg2_total_hh = sum(hh_stg2_unconstrained)) %>%
    ungroup() %>%
    mutate(scaling_factor = hh_stg2_unconstrained / stg2_total_hh) %>%
    left_join(stg1_total_households, by = c("gss_code","year"))  %>%
    mutate(stage2_households = scaling_factor * stage_1_households)

  return(stage2_constrained)

}

# dclg_outputs <- function(households, stage_1, household_population, institutional_population,
#                          households_regional, stage_2, AHS, output_location){
#   ### Output files
#   setwd(output_location)
#
#   fwrite(households, "Households - district Totals.csv", quote = TRUE)
#   fwrite(households_regional, "Households - Regional Totals.csv", quote = TRUE)
#
#   fwrite(household_population, "Household population.csv", quote = TRUE)
#   fwrite(institutional_population, "Institutional population.csv", quote = TRUE)
#
#   fwrite(stage1, "Households - Stage 1.csv", quote = TRUE)
#   fwrite(stage2, "Households - Stage 2.csv", quote = TRUE)
#   fwrite(AHS, "Households - AHS.csv", quote = TRUE)
#
#
# }


