# TODO make this take a list? 

trend_core <- function(population, births, deaths, int_out, int_in,
                       fertility, mortality, int_out_rate, int_in_proj,
                       dom_in, dom_out, dom_rate,
                       first_proj_yr, n_proj_yr, int_out_flow_or_rate,
                       constraints = NULL, upc = NULL) {
  
  library(dplyr)
  library(assertthat)
  library(popmodules)
  
  validate_trend_core_inputs(population, births, deaths, int_out, int_in, dom_out, dom_in,
                             fertility, mortality, int_out_rate, int_in_proj, dom_rate,
                             first_proj_yr, n_proj_yr, int_out_flow_or_rate)
  
  
  # Load core functions
  #age_on <- popmodules::age_on_sya
  age_on <- popmodules::popn_age_on
  calc_deaths <- popmodules::component_from_popn_rate
  calc_int_out <- popmodules::component_from_popn_rate
  calc_dom_mign <- popmodules::migrate_domestic
  
  # set up projection
  last_proj_yr <-  first_proj_yr + n_proj_yr -1
  curr_yr_popn <- population %>% filter(year == first_proj_yr - 1)
  
  # set up output lists. Done in lists for speed and rbind done after projection.
  # TODO name the list elements with the years?
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_int_out <- list(int_out %>% filter(year < first_proj_yr))
  proj_int_in <- list(int_in %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  # TODO: would we rather calculate domestic migration backseries for the input (this is done in
  # input_data_scripts/domestic_migration_2018.R, it just needs to be recoded to
  # 2011 geographies and read in at the start)
  proj_dom_out <- list(dom_out %>% filter(year < first_proj_yr))
  proj_dom_in <- list(dom_in %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    cat('\r',paste("  Projecting year",my_year))
    flush.console()
    
    # TODO pass births, deaths, migration function in via list along with their arguments to make the core more flexible.
    # Would remove need for hard coded internation out migration method switch
    
    # aged on population is used due to definitions of MYE to ensure the correct denominator
    # population in population at 30th June
    # change rates are for changes that occured in the 12 months up to 30th June
    # age is the age the cohort is at 30th June
    aged_popn <- curr_yr_popn %>%
      age_on() 
    
    # TODO calculate the births from the popn/aged_on_popn combo
    births_by_mother <- popn_apply_rate(aged_popn,
                              filter(fertility, year == my_year, age!=0),
                              col_out = "births",
                              many2one = FALSE)
    
    if(!is.null(constraints)){
      births_by_mother <- births_constrain(births=births_by_mother, constraint=constraints$births_constraint)
    }
    
    birthratio_m2f <- 1.05 
    births <- sum_births_and_split_by_sex_ratio(births_by_mother, birthratio_m2f)
    
    aged_popn_w_births <- rbind(aged_popn, rename(births, popn = births))
    validate_population(aged_popn_w_births, col_data = "popn", comparison_pop = mutate(curr_yr_popn, year=year+1))
    
    deaths <- calc_deaths(popn = aged_popn_w_births,
                          component_rate = filter(mortality, year == my_year),
                          col_popn = "popn",
                          col_rate = "rate",
                          col_component = "deaths")
    validate_population(deaths, col_data = "deaths", comparison_pop = mutate(curr_yr_popn, year=year+1))
    validate_join_population(aged_popn_w_births, deaths, many2one = FALSE, one2many = FALSE) 
    
    if(!is.null(constraints)){
      deaths <- popn_constrain(popn=deaths, constraint = constraints$deaths_constraint, col_popn = "deaths")
    }
    
    #TODO do this better
    natural_change_popn <- left_join(aged_popn_w_births, deaths, by=c("year","gss_code","sex","age")) %>%
      mutate(popn = popn - deaths) %>%
      select(-deaths)
    
    if(int_out_flow_or_rate=="flow"){
      #TODO cosmetic but the df is called int_out_rate when its not always a rate
      int_out <- int_out_rate %>% filter(year == my_year)
    } else {
      int_out <- calc_int_out(popn = natural_change_popn,
                              component_rate = filter(int_out_rate, year == my_year),
                              col_popn = "popn",
                              col_rate = "int_out",
                              col_component = "int_out")
    }
    validate_population(int_out, col_data = "int_out")
    validate_join_population(aged_popn_w_births, int_out, many2one = FALSE, one2many = FALSE)
    
    
    if(!is.null(constraints)){
      int_out <- popn_constrain(popn=int_out, constraint = constraints$international_out_constraint,
                                col_popn = "int_out")
    }
    
    int_in <- int_in_proj %>% filter(year == my_year)
    validate_population(int_in, col_data = "int_in")
    validate_join_population(aged_popn_w_births, int_in, many2one = FALSE, one2many = FALSE)
    
    if(!is.null(constraints)){
      int_in <- popn_constrain(popn=int_in, constraint = constraints$international_in_constraint,
                               col_popn = "int_in")
    }
    
    # TODO adapt this to write out gss-to-gss flows by SYA
    # TODO adapt this to work with time-varying migration rates
    
    domestic_flow <- natural_change_popn %>%
      calc_dom_mign(mign_rate = dom_rate,
                    col_aggregation = c("gss_code"="gss_out", "sex", "age"),
                    col_gss_destination = "gss_in",
                    col_popn = "popn",
                    col_rate = "rate", 
                    col_flow = "flow", 
                    pop1_is_subset = FALSE, 
                    many2one = FALSE, 
                    missing_levels_rate = TRUE) %>%
      mutate(year = my_year)
    
    if(!is.null(constraints)){
      domestic_flow <- cross_border_constrain(domestic_flow = domestic_flow,
                                              in_constraint = constraints$cross_border_in_constraint,
                                              out_constraint = constraints$cross_border_out_constraint,
                                              col_flow = "flow")
    }
    
    #TODO Look at whether its worth doing this in data.table
    dom_out <- domestic_flow %>%
      group_by(year, gss_out, sex, age) %>%
      summarise(dom_out = sum(flow)) %>%
      rename(gss_code = gss_out)
    
    dom_in <- domestic_flow %>%
      group_by(year, gss_in, sex, age) %>%
      summarise(dom_in = sum(flow)) %>%
      rename(gss_code = gss_in)
    
    #TODO Do we need to do this in the model core - inconsistent with what we do for international
    dom_net <- left_join(dom_out, dom_in, by = c("year", "gss_code", "sex", "age")) %>%
      tidyr::replace_na(list(dom_out = 0, dom_in = 0)) %>%
      tidyr::complete(year, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
      mutate(dom_net = dom_in - dom_out)
    
    if(is.null(upc)){
      
      next_yr_popn <- natural_change_popn %>% 
        arrange(year, gss_code, sex, age) %>%
        left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
        left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(dom_net, by = c("year", "gss_code", "age", "sex")) %>% 
        mutate(popn = popn - int_out + int_in + dom_net) %>%
        select(-c(int_in, int_out, dom_in, dom_out, dom_net)) %>%
        # FIXME / TODO This setup creates negative populations - should we add
        # deaths/int/domestic migration as soon as each is calculated?? For now
        # I'm just setting -ve pops to zero and noting this in the pull request
        # TODO warn on negative populations
        mutate(popn = ifelse(popn < 0, 0, popn))
      
    } else {
      
      next_yr_popn <- natural_change_popn %>% 
        arrange(year, gss_code, sex, age) %>%
        left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
        left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(dom_net, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(upc, by = c("gss_code", "age", "sex")) %>%
        replace_na(list(upc = 0)) %>%
        mutate(popn = popn - int_out + int_in + dom_net + upc) %>%
        select(-c(int_in, int_out, dom_in, dom_out, dom_net, upc)) %>%
        # FIXME / TODO This setup creates negative populations - should we add
        # deaths/int/domestic migration as soon as each is calculated?? For now
        # I'm just setting -ve pops to zero and noting this in the pull request
        # TODO warn on negative populations
        mutate(popn = ifelse(popn < 0, 0, popn))
    }
    
    validate_population(next_yr_popn, col_data = "popn",
                        comparison_pop = curr_yr_popn,
                        col_comparison = c("gss_code","sex","age"))
    
    if(!is.null(constraints)){
      warning("skipping an important test - FIXME")
      #testthat::expect_equal(popn_constrain(popn = next_yr_popn,
      #                                      constraint = constraints$population_constraint,
      #                                      col_popn = "popn"),
      #                       next_yr_popn)
    }
    
    proj_popn[[length(proj_popn)+1]] <- next_yr_popn
    proj_deaths[[length(proj_deaths)+1]] <- deaths
    proj_births[[length(proj_births)+1]] <- births
    proj_int_out[[length(proj_int_out)+1]] <- int_out
    proj_int_in[[length(proj_int_in)+1]] <- int_in
    proj_dom_out[[length(proj_dom_out)+1]] <- dom_out
    proj_dom_in[[length(proj_dom_in)+1]] <- dom_in
    proj_natural_change[[length(proj_dom_in)+1]] <- natural_change_popn
    proj_births_by_mother[[length(proj_dom_in)+1]] <- births_by_mother
    
    curr_yr_popn <- next_yr_popn
  }
  
  proj_popn   <- data.frame(data.table::rbindlist(proj_popn, use.names=TRUE))
  proj_deaths <- data.frame(data.table::rbindlist(proj_deaths, use.names=TRUE))
  proj_births <- data.frame(data.table::rbindlist(proj_births, use.names=TRUE))
  proj_int_out <- data.frame(data.table::rbindlist(proj_int_out, use.names=TRUE))
  proj_int_in <- data.frame(data.table::rbindlist(proj_int_in, use.names=TRUE))
  proj_dom_out <- data.frame(data.table::rbindlist(proj_dom_out, use.names=TRUE))
  proj_dom_in <- data.frame(data.table::rbindlist(proj_dom_in, use.names=TRUE))
  proj_natural_change <- data.frame(data.table::rbindlist(proj_natural_change, use.names=TRUE))
  proj_births_by_mother <- data.frame(data.table::rbindlist(proj_births_by_mother, use.names=TRUE)) %>%
    filter(sex == "female", age %in% 15:49)
  
  message(" ")
 
  #For int_out and domestic the rate is constant so there is no need to output all years
  int_out_rate <- filter(int_out_rate, year <= first_proj_yr)
  
  return(list(population = proj_popn, deaths = proj_deaths, births = proj_births,
              int_out = proj_int_out, int_in = proj_int_in,
              dom_out = proj_dom_out, dom_in = proj_dom_in,
              births_by_mothers_age = proj_births_by_mother,
              natural_change = proj_natural_change,
              fertility_rates = fertility,
              mortality_rates = mortality,
              int_out_rates = int_out_rate,
              domestic_rates = dom_rate))
}


# do checks on the input data
validate_trend_core_inputs <- function(population, births, deaths, int_out, int_in, dom_out, dom_in,
                                       fertility, mortality, int_out_rate, int_in_proj, dom_rate,
                                       first_proj_yr, n_proj_yr, int_out_flow_or_rate) {
  
  popmodules::validate_population(population, col_data = "popn")
  popmodules::validate_population(births, col_data = "births")
  popmodules::validate_population(deaths, col_data = "deaths")
  popmodules::validate_population(int_out, col_data = "int_out")
  popmodules::validate_population(int_in, col_data = "int_in")
  popmodules::validate_population(dom_out, col_aggregation = c("year","gss_code","sex","age"), col_data = c("dom_out"), test_complete = TRUE, test_unique = TRUE)
  popmodules::validate_population(dom_in, col_aggregation = c("year","gss_code","sex","age"), col_data = c("dom_in"), test_complete = TRUE, test_unique = TRUE)
  
  popmodules::validate_population(fertility, col_data = "rate")
  popmodules::validate_population(mortality, col_data = "rate")
  
  assert_that(int_out_flow_or_rate %in% c("flow", "rate"),
              msg = "the config variable int_out_flow_or_rate must be either 'flow' or 'rate'")
  popmodules::validate_population(int_out_rate, col_data = ifelse(int_out_flow_or_rate == "flow", "int_out", "rate"))
  popmodules::validate_population(int_in_proj, col_data = "int_in")
  #TODO add check for completeness of dom_rate
  popmodules::validate_population(dom_rate, col_aggregation = c("gss_out","gss_in","sex","age"), col_data = "rate", test_complete = FALSE, test_unique = TRUE)
  
  # check that the rates join onto the population
  ## TODO make the aggregations columns flexible. Make this more elegant.
  popmodules::validate_join_population(population, mortality, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, fertility, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, int_out_rate, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  popmodules::validate_join_population(population, int_in_proj, cols_common_aggregation = c("gss_code", "sex", "age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  # TODO fix these checks for dom_rate double geography.  Currently the below both fail
  #popmodules::validate_join_population(population, dom_rate, cols_common_aggregation = c("gss_code"="gss_out","sex","age"), pop1_is_subset = FALSE, warn_unused_shared_cols = FALSE)
  #popmodules::validate_join_population(dom_rate, population, cols_common_aggregation = c("gss_out"="gss_code","sex","age"), pop1_is_subset = TRUE, many2one = TRUE, one2many = FALSE)
  
  # check that the coverage of years is correct
  last_proj_yr <- first_proj_yr + n_proj_yr -1
  assert_that((first_proj_yr - 1) %in% unique(population$year), msg = paste0("the population backseries doesn't contain the projection jump-off year (", first_proj_yr-1,")"))
  assert_that(all(first_proj_yr:last_proj_yr %in% fertility$year), msg = "the projected fertility data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% mortality$year), msg = "the projected mortality data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% int_out_rate$year), msg = "the projected int_out_rate data doesn't contain all the projection years")
  assert_that(all(first_proj_yr:last_proj_yr %in% int_in_proj$year), msg = "the projected int_in data doesn't contain all the projection years")
  
  # check that the rates values are always between 0 and 1 
  assert_that(max(fertility$rate) <= 1 & min(fertility$rate) >= 0, msg = "projected fertility contains rates outside the range 0-1")
  assert_that(max(mortality$rate) <= 1 & min(mortality$rate) >= 0, msg = "projected mortality contains rates outside the range 0-1")
  if(int_out_flow_or_rate == "rate") {
    assert_that(max(int_out_rate$rate) <= 1 & min(int_out_rate$rate) >= 0, msg = "projected international out migration rate contains rates outside the range 0-1")
  }
  assert_that(max(dom_rate$rate) <= 1 & min(dom_rate$rate) >= 0, msg = "projected domestic migration rate contains rates outside the range 0-1")
  
  invisible(TRUE)
}
