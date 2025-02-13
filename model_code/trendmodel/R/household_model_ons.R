#' Run the 2016/2018 ONS household model
#'
#' Run stages 1 and 2 of the ONS household model and return outputs.
#'
#' @param population A data frame containing population data.
#' @param stage1_file_path String. Path to file containing DCLG stage 1
#'   household inputs.
#' @param stage2_file_path String. Path to file containing DCLG stage 2
#'   household inputs.
#' @param communal_est_pop_path String. Path to file containing communal
#'   establishment population rates for the each year of the projection period.
#' @param first_proj_yr Numeric. First year of the model projection. Elderly
#'   communal populations are adjusted after this date.
#'
#' @return A list containing 2 lists: Stage 1 outputs and Stage 2 outputs.
#'
#' @import dplyr
#' @import popmodules
#'
#' @export
household_model_ons <- function(population, stage1_file_path, stage2_file_path, communal_est_pop_path, first_proj_yr){
  
  # Run stage 1 of the ONS household model
  stage_1 <- ons_stage_1(population, stage1_file_path, communal_est_pop_path, first_proj_yr)
  
  # Run stage 2 of the ONS household model using the outputs from stage 1
  stage_2 <- ons_stage_2(stage2_file_path, stage_1)

  # Validate the outputs of stage 1 and stage 2
  validate_ons_outputs(stage_1, stage_2, max(population$year))
  
  # Return the outputs as a list
  return(list(stage_1 = stage_1, stage_2 = stage_2))
  
}

#' Implementation of the ONS 2016 Household Model Stage 1
#'
#' Produce household projections from an input population projection using the
#' 2016 ONS household projection methodology.
#'
#' @param popn A data frame containing population data.
#' @param hh_rep_rates_path String. Path to file containing household
#'   representative rates for the each year of the projection period.
#' @param communal_est_pop_path String. Path to file containing communal
#'   establishment population rates for the each year of the projection period.
#' @param first_proj_yr Numeric. First year of the model projection. Elderly
#'   communal populations are adjusted after this date.
#'
#' @return A list containing 4 dataframes: Unconstrained and constrained
#'   household projections, communal establishment and household populations for
#'   the projection period.
#'
#' @import dplyr
#' @import popmodules
#'
#' @export
ons_stage_1 <- function(popn, hh_rep_rates_path, communal_est_pop_path, first_proj_yr){
  
  # Read the district to region lookup table
  district_to_region <- readRDS("input_data/lookup/district_to_region.rds")
  
  # Read the household representative rates
  household_rates <- readRDS(hh_rep_rates_path) %>%
    project_forward_flat(max(popn$year)) %>%
    filter(year %in% unique(popn$year))
  
  # Filter the population data to match the household rates
  population <- popn %>%
    filter(gss_code %in% unique(household_rates$gss_code), # filter to England, basically
           year %in% unique(household_rates$year))
#       Same number as 2011 for 0-74
  #       Same proportion 75+
  #       Prison population updated upto and inc 2016
  
  # Convert the population data into age groups
  population_age_groups <- population_into_age_groups(population, age_groups = c(0, 15, seq(19,89,5), Inf),
                                                      labels = c("0_15","16_19","20_24","25_29","30_34","35_39","40_44",
                                                                 "45_49","50_54","55_59","60_64","65_69",
                                                                 "70_74","75_79","80_84","85_89","90+"),
                                                      data_cols = "popn")

  # Get the communal establishment population
  communal_establishment <- get_communal_establishment_popn(communal_est_pop_path,
                                                            population_age_groups,
                                                            rates_ages = c("75_79","80_84","85_89","90+"),
                                                            first_proj_yr = first_proj_yr)
 
  # Get the household population
  household_population <- get_household_popn(population_age_groups, communal_establishment)
  
  # Apply household rates to the household population to get household projections
  household_projection <- apply_rate_to_population(popn = household_population,
                                                   rates = household_rates,
                                                   col_aggregation = c("gss_code", "sex", "year", "age_group"),
                                                   col_popn = "household_popn",
                                                   col_rate = "HRR",
                                                   col_out = "households")
  
  # Constrain regions to England, LAs to regions
  unconstrained_regional <- filter(household_projection, substr(gss_code,1,3)=="E12")
  england_proj <- filter(household_projection, substr(gss_code,1,3)=="E92")
  unconstrained_la <- filter(household_projection, !substr(gss_code,1,3) %in% c("E12","E92"))
  
  # Regional constrained to England
  constrained_regional <- constrain_regional_hh(unconstrained_regional, england_proj)
  
  # Districts constrained to regions
  constrained_district <- constrain_district_hh(unconstrained_la, constrained_regional, district_to_region)
  constrained <- data.frame(rbind(england_proj, constrained_regional, constrained_district))
  
  # Pretty-up outputs
  household_population <- rename(household_population, household_population = household_popn)
  communal_establishment <- rename(communal_establishment, communal_establishment_population = ce_pop)
  
  detailed_households <- left_join(constrained, household_population, by = c("gss_code", "sex", "year", "age_group")) %>%
    left_join(communal_establishment, by = c("gss_code", "sex", "year", "age_group"))
  
  # Return the outputs as a list
  return(list(detailed_households = detailed_households,
              unconstrained = data.frame(household_projection),
              constrained = constrained,
              household_population = data.frame(household_population),
              communal_establishment_population = data.frame(communal_establishment)))
}

#' Implementation of the ONS 2016 Stage 2 Household Model
#'
#' Produce household projections from an input population projection using the
#' 2016 ONS household projection methodology.
#'
#' @param stage2_file_path String. File path to a dataframe containing ONS
#'   headship rates.
#' @param stage1_output A list of dataframes of ONS stage 1 model outputs.
#'
#' @return A list containing 2 dataframes: unconstrained and constrained
#'   household projections.
#'
#' @import dplyr
#' @import popmodules
#'
#' @export
ons_stage_2 <- function(stage2_file_path, stage1_output){
  
  # Get the household population and stage 1 total households
  household_popn <- stage1_output$household_population
  stg1_total_households <- stage1_output$constrained
  
  # Read the headship rates
  headship_rates <- readRDS(stage2_file_path) %>%
    project_forward_flat(max(household_popn$year)) %>%
    filter(gss_code %in% unique(household_popn$gss_code),
           year %in% unique(household_popn$year))
  
  # Separate headship rates by household type
  hh_rates_no_sex <- filter(headship_rates, !household_type %in%
                              c("One person households: Male", "One person households: Female"))
  hh_rates_female <- filter(headship_rates, household_type == "One person households: Female")
  hh_rates_male <- filter(headship_rates, household_type == "One person households: Male")
  
  # Calculate household population by total, female, and male
  hh_pop_total <- group_by(household_popn, gss_code, year, age_group) %>%
    summarise(household_popn = sum(household_population), .groups = 'drop_last') %>%
    ungroup()
  hh_pop_female <- filter(household_popn, sex == "female") %>%
    group_by(gss_code, year, age_group) %>%
    summarise(household_popn = sum(household_population), .groups = 'drop_last') %>%
    ungroup()
  hh_pop_male <- filter(household_popn, sex == "male") %>%
    group_by(gss_code, year, age_group) %>%
    summarise(household_popn = sum(household_population), .groups = 'drop_last') %>%
    ungroup()
  
  # Calculate unconstrained household projections
  unconstrained_total <- left_join(hh_rates_no_sex, hh_pop_total, by = c("gss_code", "year", "age_group"))
  unconstrained_female <- left_join(hh_rates_female, hh_pop_female, by = c("gss_code", "year", "age_group"))
  unconstrained_male <- left_join(hh_rates_male, hh_pop_male, by = c("gss_code", "year", "age_group"))
  
  unconstrained_hh <- rbind(unconstrained_total, unconstrained_female, unconstrained_male) %>%
    mutate(unconstrained = household_popn * rate)
  
  # Calculate constrained household projections
  stg1_total_households <- group_by(stg1_total_households, year, gss_code, age_group) %>%
    summarise(stg1_total = sum(households), .groups = 'drop_last') %>%
    ungroup()
  
  constrained_hh <- group_by(unconstrained_hh, year, gss_code, age_group) %>%
    mutate(total_unconstrained = sum(unconstrained)) %>%
    left_join(stg1_total_households, by = c("gss_code", "year", "age_group")) %>%
    mutate(scaling = ifelse(total_unconstrained == 0, 0, stg1_total / total_unconstrained)) %>%
    mutate(constrained = unconstrained * scaling)
  
  constrained <- select(constrained_hh, year, gss_code, household_type, age_group, households = constrained)
  unconstrained <- select(constrained_hh, year, gss_code, household_type, age_group, households = unconstrained)
  
  # Return the outputs as a list
  return(list(unconstrained = unconstrained,
              constrained = constrained))
  
}


#-----------------------------------------------------

# Currently unused

aggregate_geography <- function(population, col_aggregation = c("gss_code", "sex", "age", "year")){
  
  population <- filter(population, substr(gss_code,1,1)=="E")
  
  #The household model groups some local authorities
  population <- population %>%
    mutate(gss_code = recode(gss_code,
                             "E09000033" = "E09000001",
                             "E06000052" = "E06000053")) %>%
    group_by_at(col_aggregation) %>%
    summarise(popn = sum(popn)) %>%
    ungroup()
  
  return(population)
  
}

#-----------------------------------------------------

get_communal_establishment_popn <- function(communal_est_pop_path, population, rates_ages, first_proj_yr) {
  
  ce <- readRDS(communal_est_pop_path)
  
  absolute <- filter(ce, !age_group %in% rates_ages, year == max(ce$year)) %>% select(-year, -ce_rate)
  proportional <- filter(ce, age_group %in% rates_ages, year == max(ce$year)) %>% select(-year, -ce_pop)
  
  older_ce <- list()
  younger_ce <- list()
  
  for(yr in first_proj_yr:max(population$year)){
    
    older_ce[[yr]] <- filter(population, year == yr, age_group %in% rates_ages) %>%
      left_join(proportional, by=c("gss_code","sex","age_group")) %>%
      mutate(ce_pop = popn * ce_rate) %>%
      select(-popn, -ce_rate)
    
    younger_ce[[yr]] <- mutate(absolute, year = yr)
    
  }
  
  ce <- select(ce, -ce_rate) %>%
    filter(year < first_proj_yr) %>%
    rbind(data.table::rbindlist(older_ce),
          data.table::rbindlist(younger_ce)) %>%
    as.data.frame()
  
  return(ce)
}

#-----------------------------------------------------

get_household_popn <- function(population, communal_establishment){
  
  household_population <- left_join(population, communal_establishment, by=c("gss_code","year","sex","age_group")) %>%
    mutate(household_popn = popn - ce_pop) %>%
    select(-popn, -ce_pop) %>%
    check_negative_values("household_popn")
  
  return(household_population)
}

#-----------------------------------------------------

constrain_regional_hh <- function(unconstrained_regional, england_proj){
  
  regional_constraint <- group_by(unconstrained_regional, year, sex, age_group) %>%
    summarise(reg_hh = sum(households), .groups = 'drop_last') %>%
    ungroup() %>%
    left_join(england_proj, by=c("year", "sex", "age_group")) %>%
    rename(eng_hh = households) %>%
    mutate(constr = ifelse(eng_hh == 0, 1, eng_hh/reg_hh)) %>%
    select(year, sex, age_group, constr)
  
  constrained_regional <-left_join(unconstrained_regional, regional_constraint, by=c("year","sex","age_group")) %>%
    mutate(households = households*constr) %>%
    select(-constr)
  
  return(constrained_regional)
  
}

#-----------------------------------------------------

constrain_district_hh <- function(unconstrained_la, constrained_regional, district_to_region){
  
  la_constraint <- left_join(unconstrained_la, district_to_region, by="gss_code") %>%
    group_by(region_gss_code, year, sex, age_group) %>%
    summarise(la_hh = sum(households), .groups = 'drop_last') %>%
    ungroup() %>%
    left_join(constrained_regional, by=c("region_gss_code"="gss_code","year","sex","age_group")) %>%
    rename(reg_hh = households) %>%
    mutate(constr = ifelse(reg_hh == 0, 0, reg_hh/la_hh)) %>%
    select(region_gss_code, year, sex, age_group, constr)
  
  constrained_la <- left_join(unconstrained_la, district_to_region, by="gss_code") %>%
    left_join(la_constraint, by=c("region_gss_code","year","sex","age_group")) %>%
    mutate(households = households*constr) %>%
    select(gss_code, year, sex, age_group, households)
  
  return(constrained_la)
  
}

#-----------------------------------------------------

validate_ons_outputs <- function(stage_1, stage_2, max_year) {
  
  validate_population(stage_1$detailed_households,
                      col_aggregation = c("year", "gss_code", "sex", "age_group"),
                      col_data = c("households", "household_population", "communal_establishment_population"),
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = TRUE)
  for(i in 2:5) {
    result <- stage_1[[i]] %>%
      filter(year <= max_year) %>%
      validate_population(col_aggregation = c("year", "gss_code", "sex", "age_group"),
                          test_complete = TRUE,
                          test_unique = TRUE,
                          check_negative_values = TRUE)
  }
  for(i in 1:2) {
    result <- stage_2[[i]] %>%
      filter(year <= max_year) %>%
      validate_population(col_aggregation = c("year", "gss_code", "household_type", "age_group"),
                          col_data = "households",
                          test_complete = TRUE,
                          test_unique = TRUE,
                          check_negative_values = TRUE)
  }
  return(TRUE)
}

