#' Run the 2014 DCLG household model
#'
#' Run stages 1 and 2 of the DCLG household model and retuen outputs
#'
#' @param population A data frame containing population data.
#' @param stage1_file_path String. Path to file containing DCLG
#'   stage 1 household inputs
#' @param stage2_file_path String. Path to file containing DCLG
#'   stage 2 household inputs
#'
#' @return A list containing 2 lists: Stage 1 outputs and Stage 2
#'   outputs.

household_model_dclg <- function(population, stage1_file_path, stage2_file_path){

  stage_1 <- dclg_stage_1(population, stage1_file_path)
  stage_2 <- dclg_stage_2(stage2_file_path, stage_1)

  validate_dclg_outputs(stage_1, stage_2, max(population$year))
  
  return(list(stage_1 = stage_1, stage_2 = stage_2))

}

#' Implementation of the DCLG 2014 Stage 1 Household Model
#'
#' Produce household projections from an input population
#' projection using the 2014 DCLG household projection methodology
#'
#' @param population A data frame containing population data.
#' @param stage1_file_path String. Path to file containing DCLG
#'   stage 1 household inputs
#'
#' @return A list containing 2 dataframes: Stage 1 household by household
#'   type and total.
#'
#' @import dplyr
#' @import popmodules
#' @importFrom dtplyr lazy_dt

dclg_stage_1 <- function(population, stage1_file_path){

  stage1_data <- readRDS(stage1_file_path) %>%
    filter(year <= max(population$year))

  ### Create data for years beyond last dclg year ###
  if(max(stage1_data$year) < max(population$year)){
    stage1_data <- project_forward_flat(stage1_data, max(population$year))
  }

  #Make sure years match
  common_years <- intersect(stage1_data$year, population$year)
  stage1_data <- filter(stage1_data, year %in% common_years)
  population <- filter(population, year %in% common_years)

  #separate out data
  hh_rep_rates <- select(stage1_data, gss_code, year, sex, household_type, age_group, hh_rep_rates)

  ### Group sya pop into 5 year bands up to 85+
  popn_5yr_bands <-  population %>%
    filter(gss_code %in% unique(stage1_data$gss_code)) %>%
    population_into_age_groups(age_groups=c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,Inf),
                               labels=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39","40_44",
                                        "45_49","50_54","55_59","60_64","65_69","70_74","75_79","80_84","85&"),
                               data_cols="popn") %>%
    rename(popn_5yr_bands = popn)

  ### Aggregate populations by HH type
  sum_hh_type <- stage1_data %>%
    group_by(gss_code, year, sex, age_group) %>%
    mutate(scaling_factor = total_population / sum(total_population)) %>%
    ungroup()

  ### Calculate proportion in each HH type
  ### Multiply proportions by popn
  validate_join_population(sum_hh_type, popn_5yr_bands, cols_common_aggregation = c("year", "gss_code", "sex", "age_group"),
                           pop1_is_subset = FALSE, many2one=TRUE, one2many = FALSE)

  scaled_projection <- left_join(sum_hh_type, popn_5yr_bands, by=c("gss_code", "year", "sex", "age_group")) %>%
    mutate(scaled_popn = scaling_factor * popn_5yr_bands) %>%
    select(year, gss_code, sex, age_group, household_type, institutional_population, total_population, scaled_popn)

  scaled_communal_popn <- scaled_projection %>%
    mutate(scaled_ce_popn = ifelse(age_group %in% c("75_79", "80_84", "85&"),
                                   (institutional_population / total_population) * scaled_popn,
                                   institutional_population),
           scaled_ce_popn = ifelse(scaled_popn == 0, 0, scaled_ce_popn)) %>%
    select(-total_population, -institutional_population)

  scaled_household_popn <- scaled_communal_popn %>%
    mutate(scaled_hh_popn = scaled_popn - scaled_ce_popn) %>%
    check_negative_values("scaled_hh_popn")

  scaled_households <- apply_rate_to_population(scaled_household_popn, hh_rep_rates,
                                       col_aggregation = c("year", "gss_code", "sex", "age_group", "household_type"),
                                       col_popn = "scaled_hh_popn", col_rate = "hh_rep_rates", col_out = "scaled_households")

  household_population <- lazy_dt(scaled_household_popn) %>%
    group_by(year, gss_code, sex, age_group) %>%
    summarise(household_population = sum(scaled_hh_popn)) %>%
    as.data.frame()

  ce_population <- lazy_dt(scaled_communal_popn) %>%
    group_by(year, gss_code, sex, age_group) %>%
    summarise(communal_establishment_population = sum(scaled_ce_popn)) %>%
    as.data.frame()

  total_households <- lazy_dt(scaled_households) %>%
    group_by(year, gss_code) %>%
    summarise(stage_1_households = sum(scaled_households)) %>%
    as.data.frame()

  detailed_households <- left_join(scaled_households, scaled_household_popn,
                                   by = c("year", "gss_code", "household_type", "sex", "age_group")) %>%
    select(year, gss_code, household_type, sex, age_group,
           households = scaled_households,
           household_population = scaled_hh_popn,
           communal_establishment_population = scaled_ce_popn)

  out <- list(detailed_households = detailed_households,
              total_households = total_households,
              household_population = household_population,
              communal_establishment_population = ce_population)
  
  # out <- lapply(out, aggregate_regions, england = TRUE)
  
  return(out)

}

#---------------------------------------------------------
#' Implementation of the DCLG 2014 Stage 2 Household Model
#'
#' Produce household projections from an input population
#' projection using the 2014 DCLG household projection methodology
#'
#' @param stage2_file_path String. File path to a dataframe containing
#'   DCLG headship rates.
#' @param stage1_output A list of DCLG stage 1 model outputs
#'
#' @return A list containing 5 dataframes: unconstrained and constrained
#'   household projections, household and communal establishment populations,
#'   and households by age.
#'
#' @import dplyr
#' @import popmodules
#' @importFrom dtplyr lazy_dt

dclg_stage_2 <- function(stage2_file_path, stage1_output){

  headship_rates <- readRDS(stage2_file_path)

  stage1_detailed_households <- stage1_output$detailed_households
  household_popn <- stage1_output$household_population
  stg1_total_households <- stage1_output$total_households

  headship_rates <- project_forward_flat(headship_rates, max(stage1_detailed_households$year)) %>%
    filter(year %in% household_popn$year)

  recoding_ages <- c("0_4" = "0_14",
                     "5_9" = "0_14",
                     "10_14" = "0_14",
                     "15_19" = "15_24",
                     "20_24" = "15_24",
                     "25_29" = "25_34",
                     "30_34" = "25_34",
                     "35_39" = "35_44",
                     "40_44" = "35_44",
                     "45_49" = "45_54",
                     "50_54" = "45_54",
                     "55_59" = "55_59",
                     "60_64" = "60_64",
                     "65_69" = "65_74",
                     "70_74" = "65_74",
                     "75_79" = "75_84",
                     "80_84" = "75_84")

  child_hh_pop <- lazy_dt(household_popn) %>%
    filter(gss_code %in% unique(headship_rates$gss_code)) %>%
    filter(age_group %in% c("0_4","5_9","10_14"))%>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(hh_popn = sum(household_population)) %>%
    as.data.frame()

  adult_hh_pop <- lazy_dt(household_popn) %>%
    filter(gss_code %in% unique(headship_rates$gss_code)) %>%
    filter(!age_group %in% c("0_4","5_9","10_14")) %>%
    #filter(substr(gss_code,1,1)=="E") %>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(hh_popn = sum(household_population)) %>%
    as.data.frame()

 stage2_unconstrained <- apply_rate_to_population(headship_rates, adult_hh_pop, col_aggregation=c("gss_code","year","age_group","household_type"),
                       col_rate = "hh_popn", col_popn = "rate", col_out = "hh_stg2_unconstrained",
                       many2one = TRUE)

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

  households_stage_2_ages <- stage1_detailed_households %>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(households = sum(households)) %>%
    ungroup()

  ce_popn <- stage1_output$communal_establishment_population %>%
    lazy_dt() %>%
    mutate(age_group = recode(age_group, !!!recoding_ages)) %>%
    group_by(gss_code, year, age_group) %>%
    summarise(communal_establishment_population = sum(communal_establishment_population)) %>%
    as.data.frame()

  return(list(unconstrained = stage2_unconstrained,
              constrained = stage2_constrained,
              detailed_households = households_stage_2_ages,
              household_population = household_population,
              communal_establishment_population = ce_popn))
}


#-----------------------------------------------------

validate_dclg_outputs <- function(stage_1, stage_2, max_year) {
  
  validate_population(stage_1$detailed_households,
                      col_aggregation = c("year", "gss_code", "household_type", "sex", "age_group"),
                      col_data = c("households", "household_population", "communal_establishment_population"))
  validate_population(stage_1$total_households,
                      col_aggregation = c("year", "gss_code"),
                      col_data = c("stage_1_households"))
  validate_population(stage_1$household_population,
                      col_aggregation = c("year", "gss_code", "sex", "age_group"),
                      col_data = "household_population")
  validate_population(stage_1$communal_establishment_population,
                      col_aggregation = c("year", "gss_code", "sex", "age_group"),
                      col_data = "communal_establishment_population")
  
  for(i in 1:2) {
    result <- stage_2[[i]] %>%
      filter(year <= max_year) %>%
      validate_population(col_aggregation = c("gss_code", "year", "age_group", "household_type"),
                          col_data = "households")
  }
  for(i in 3:5) {
    result <- stage_2[[i]] %>%
      filter(year <= max_year) %>%
      validate_population(col_aggregation = c("gss_code", "year", "age_group"))
  }
  return(TRUE)
}