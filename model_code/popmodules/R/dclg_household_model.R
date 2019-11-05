#' Implementation of the DCLG 2014 Stage 1 Household Model
#'
#' Produce household projections from an input population
#' projection using the 2016 ONS household projection methodology
#'
#' @param population A data frame containing population data.
#' @param stage1_file_path String. Path to file containing DCLG
#'   stage 1 household inputs
#'
#' @return A list containing 2 dataframes: Stage 1 household by household
#'   type and total.

dclg_stage_1 <- function(population, stage1_file_path){
  
  stage1_data <- readRDS(stage1_file_path)
  
  ### Group sya pop into 5 year bands up to 85+
  popn_5yr_bands <-  population_into_age_groups(population,
                                                age_groups=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                                                labels=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44",
                                                         "45_49","50_54","55_59","60_64","65_69","70_74","75_79","80_84","85&"),
                                                popn_col="popn") %>%
    rename(popn_5yr_bands = popn)
  
  ### Join all dclg inputs together
  ### calc implied dclg HH rep rates
  
  ### Create data for years beyond last dclg year ###
  stage1_data <- extend_data(stage1_data, max(population$year))
  
  ### Aggregate populations by HH type
  sum_hh_type <- stage1_data %>%
    group_by(gss_code,year,sex,age_group) %>%
    mutate(sum_hh_popn = sum(household_population),
           sum_households = sum(households),
           sum_total_popn = sum(total_population),
           sum_ce_popn = sum(institutional_population)) %>%
    ungroup()
  
  ### Calc proportion in each HH type
  ### Multiply proportions by popn
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
  total_households <- scaled_households %>%
    group_by(gss_code, year) %>%
    summarise(stage_1_households = sum(scaled_households)) %>%
    ungroup()
  
  return(list(scaled_households, total_households))
  
}

#---------------------------------------------------------
#' Implementation of the DCLG 2014 Stage 2 Household Model
#'
#' Produce household projections from an input population
#' projection using the 2016 ONS household projection methodology
#'
#' @param headship_rates_path A data frame containing DCLG headship rates.
#' @param stage1_file_path String. DCLG stage 1 model outputs
#'
#' @return A list containing 5 dataframes: unconstrained and constrained
#'   household projections, household and communal establishment populations,
#'   and households by age.

dclg_stage_2 <- function(headship_rates_path, stage1_output){
  
  headship_rates <- readRDS(headship_rates_path)
  
  scaled_households <- stage1_output$scaled_households
  stg1_total_households <- stage1_output$total_households
  
  headship_rates <- extend_data(headship_rates, max(scaled_households$year))
  
  recoding_ages <- c("0_14" = "0_4",
                     "0_14" = "5_9",
                     "0_14" = "10_14",
                     "15_19" = "15_24",
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
  
  child_hh_pop <- scaled_households %>%
    filter(gss_code %in% unique(headship_rates$gss_code)) %>%
    filter(age_group %in% c("0_4","5_9","10_14"))%>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(hh_popn = sum(scaled_hh_popn)) %>%
    ungroup()
  
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
  
  stage2_unconstrained <- select(stage2_unconstrained, gss_code, year, age_group,
                                 household_type, households = hh_stg2_unconstrained)
  
  stage2_constrained <- select(stage2_constrained, gss_code, year, age_group,
                               household_type, households = stage2_households)
  
  household_population <- rbind(adult_hh_pop, child_hh_pop) %>%
    rename(household_popn = hh_popn)
  
  households_stage_2_ages <- scaled_households %>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(households = sum(households)) %>%
    ungroup()
  
  ce_popn <- scaled_households %>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(ce_pop = sum(scaled_ce_popn))
  
    return(list(unconstrained = stage2_unconstrained,
              constrained = stage2_constrained,
              household_population = household_population,
              communal_establishment = ce_popn,
              households_by_age = households_stage_2_ages))
         
}
