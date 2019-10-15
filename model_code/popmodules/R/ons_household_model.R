ons_household_model <- function(projection, hh_rep_rates_file, ce_file, file_location){

  household_rates <- readRDS(paste0(file_location, hh_rep_rates_file))

  projection <- aggregate_geography(projection)

  #       Same number as 2011 for 0-74
  #       Same proportion 75+
  #       Prison population updated upto and inc 2016

  projection_age_groups <- population_into_age_groups(projection, age_groups = c(0, 16,seq(19,84,5),Inf),
                                                      labels = c("0_15","16_19","20_24","25_29","30_34","35_39","40_44",
                                                                 "45_49","50_54","55_59","60_64","65_69",
                                                                 "70_74","75_79","80_84","85_over"),
                                                      popn_col = "popn")


  communal_establishment <- get_communal_establishment_popn(paste0(file_location, ce_file),
                                                            projection_age_groups, rates_ages = c("75_79","80_84","85_over"))

  household_population <- get_household_popn(projection_age_groups, communal_establishment)

  household_projection <- apply_household_rates(household_population, household_rates, rates_col="HRR", hh_pop_col="household_popn")


  #Constrain regions to England, LAs to regions
  unconstrained_regional <- filter(household_projection, substr(gss_code,1,3)=="E12")
  england_proj <- filter(household_projection, substr(gss_code,1,3)=="E92")
  unconstrained_la <- filter(household_projection, !substr(gss_code,1,3) %in% c("E12","E92"))

  #Regional constrained to England
  constrained_regional <- constrain_regional_hh(unconstrained_regional, england_proj)

  #Districts constrained to regions
  #TODO add region-to-LA lookup
  #constrained_district <- constrain_district_hh(unconstrained_la, constrained_regional)

  return(list(unconstrained = household_projection,
              constrained = rbind(england_proj, constrained_regional, constrained_district),
              household_population = data.table::rbindlist(household_ce_pop[[1]]),
              communal_establishment_population = data.table::rbindlist(household_ce_pop[[2]])))

}

#-----------------------------------------------------

aggregate_geography <- function(projection){

  projection <- filter(projection, substr(gss_code,1,1)=="E")

  #TODO Get a region lookup


  projection <- projection %>%
    mutate(gss_code = case_when(gss_code == "E09000033" ~ "E090000001",
                                gss_code == "E06000052" ~ "E06000053")) %>%
    group_by(gss_code, sex, age, year) %>%
    summarise_all(funs(sum)) %>%
    as.data.frame

  # eng <- projection %>%
  #   mutate(gss_code = "E92000001") %>%
  #   group_by(gss_code, sex, age, year) %>%
  #   summarise_all(funs(sum)) %>%
  #   as.data.frame()
  #
  # reg <- projection %>%
  #   left_join(region_lk, by="gss_code") %>%
  #   select(-gss_code, -district) %>%
  #   rename(gss_code=region_gss_code) %>%
  #   group_by(gss_code, sex, age, year) %>%
  #   summarise_all(funs(sum)) %>%
  #   as.data.frame()
  #
  # projection <- rbind(projection, reg, eng)

  return(projection)

}

#-----------------------------------------------------

get_communal_establishment_popn <- function(ce_file, population, rates_ages) {

  ce <- readRDS(ce_file)

  absolute <- filter(ce, !age_group %in% rates_ages, year == max(ce$year)) %>% select(-year)
  proportional <- filter(ce, age_group %in% rates_ages, year == max(ce$year)) %>% select(-year)

  older_ce <- list()
  younger_ce <- list()

  for(yr in c(max(ce$year):max(population$year))){

    older_ce[[yr]] <- filter(population, year == yr) %>%
      left_join(proportional, by=c("gss_code","sex","age_group")) %>%
      mutate(ce_pop = popn * ce_rate) %>%
      select(-popn, -ce_rate)

    younger_ce[[yr]] <- mutate(absolute, year = yr) %>%
      select(-ce_rate)

  }

  ce <- rbind(older_ce <- data.table::rbindlist(older_ce),
              younger_ce <- data.table::rbindlist(younger_ce))

  return(ce)
}

#-----------------------------------------------------

get_household_popn <- function(population, communal_establishment){

  household_population <- left_join(population, communal_establishment, by=c("gss_code","year","sex","age_group")) %>%
    mutate(household_popn = popn - ce_pop) %>%
    select(-popn, -ce_pop)

  return(household_population)
}

#-----------------------------------------------------

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
    select(-constr)

  return(constrained_regional)

}

#-----------------------------------------------------

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


#-----------------------------------------------------


# summary_tbl <- left_join(constrained_proj, household_pop,
#                          by=c("gss_code","year","sex","age_group")) %>%
#         left_join(communal_pop,
#                   by=c("gss_code","year","sex","age_group")) %>%
#         mutate(total_pop = hh_pop + ce_pop) %>%
#         select(-age_group, -sex) %>%
#         data.frame() %>%
#         group_by(gss_code, district, year) %>%
#         summarise_all(funs(sum)) %>%
#         data.frame() %>%
#         mutate(ahs = hh_pop / households) %>%
#         mutate(ahs = round(ahs, 3),
#                total_pop = round(total_pop, 0),
#                hh_pop = round(hh_pop, 0),
#                ce_pop = round(ce_pop, 0),
#                households = round(households, 0))




