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
  
  # set up projection
  last_proj_yr <-  first_proj_yr + n_proj_yr -1
  curr_yr_popn <- population %>% filter(year == first_proj_yr - 1)
  
  proj_popn <- list(population %>% filter(year < first_proj_yr))
  proj_int_out <- list(int_out %>% filter(year < first_proj_yr))
  proj_int_in <- list(int_in %>% filter(year < first_proj_yr))
  proj_deaths <- list(deaths %>% filter(year < first_proj_yr))
  proj_births <- list(births %>% filter(year < first_proj_yr))
  proj_dom_out <- list(dom_out %>% filter(year < first_proj_yr))
  proj_dom_in <- list(dom_in %>% filter(year < first_proj_yr))
  proj_natural_change <- list()
  proj_births_by_mother <- list()
  
  # run projection
  for (my_year in first_proj_yr:last_proj_yr) {
    
    cat('\r',paste("  Projecting year",my_year))
    flush.console()
    
    # aged on population is used due to definitions of MYE to ensure the correct denominator
    # population in population at 30th June
    # change rates are for changes that occured in the 12 months up to 30th June
    # age is the age the cohort is at 30th June
    aged_popn <- curr_yr_popn %>%
      popn_age_on() 
    
    at_risk <- curr_yr_popn %>%
      mutate(age = age+1) %>%
      left_join(curr_yr_popn, by=c("gss_code","year","sex","age")) %>%
      mutate(popn = (popn.x + popn.y)/2)       %>%
      mutate(popn = ifelse(is.na(popn),0,popn),
             year = year +1) %>%
      select(gss_code, year, sex, age, popn) %>%
      arrange(gss_code, year, sex, age) %>%
      filter(age <=90)
    
    births_by_mother <- apply_rate_to_population(at_risk,
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
    
    deaths <- component_from_popn_rate(popn = aged_popn_w_births,
                                       component_rate = filter(mortality, year == my_year),
                                       col_popn = "popn",
                                       col_rate = "rate",
                                       col_component = "deaths")
    validate_population(deaths, col_data = "deaths", comparison_pop = mutate(curr_yr_popn, year=year+1))
    validate_join_population(aged_popn_w_births, deaths, many2one = FALSE, one2many = FALSE) 
    
    if(!is.null(constraints)){
      deaths <- popn_constrain(popn=deaths, constraint = constraints$deaths_constraint, col_popn = "deaths")
    }
    
    natural_change_popn <- left_join(aged_popn_w_births, deaths, by=c("year","gss_code","sex","age")) %>%
      mutate(popn = popn - deaths) %>%
      select(-deaths)
    
    if(int_out_flow_or_rate=="flow"){
      #TODO cosmetic but the df is called int_out_rate when its not always a rate
      int_out <- int_out_rate %>% filter(year == my_year)
    } else {
      int_out <- component_from_popn_rate(popn = natural_change_popn,
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
    
    domestic_flow <- natural_change_popn %>%
      apply_domestic_migration_rates(mign_rate = dom_rate,
                                     col_aggregation = c("gss_code"="gss_out", "sex", "age"),
                                     col_gss_destination = "gss_in",
                                     col_popn = "popn",
                                     col_rate = "rate", 
                                     col_flow = "flow", 
                                     pop1_is_subset = FALSE, 
                                     many2one = FALSE) %>%
      mutate(year = my_year)
    
    if(!is.null(constraints)){
      domestic_flow <- cross_border_constrain(domestic_flow = domestic_flow,
                                              in_constraint = constraints$cross_border_in_constraint,
                                              out_constraint = constraints$cross_border_out_constraint,
                                              col_flow = "flow")
    }
    
    dom_out <- dtplyr::lazy_dt(domestic_flow) %>%
      group_by(year, gss_out, sex, age) %>%
      summarise(dom_out = sum(flow)) %>%
      as.data.frame() %>%
      rename(gss_code = gss_out)%>%
      tidyr::complete(year, gss_code, sex, age=0:90, fill=list(dom_out=0))
    
    dom_in <- dtplyr::lazy_dt(domestic_flow) %>%
      group_by(year, gss_in, sex, age) %>%
      summarise(dom_in = sum(flow)) %>%
      as.data.frame()%>%
      rename(gss_code = gss_in)%>%
      tidyr::complete(year, gss_code, sex, age=0:90, fill=list(dom_in=0))
    
    if(is.null(upc)){
      
      next_yr_popn <- natural_change_popn %>% 
        arrange(year, gss_code, sex, age) %>%
        left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
        left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(dom_out, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(dom_in, by = c("year", "gss_code", "age", "sex")) %>% 
        mutate(next_popn = popn - int_out + int_in - dom_out + dom_in)
      
    } else {
      
      next_yr_popn <- natural_change_popn %>% 
        arrange(year, gss_code, sex, age) %>%
        left_join(int_out, by = c("year", "gss_code", "age", "sex")) %>%
        left_join(int_in, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(dom_out, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(dom_in, by = c("year", "gss_code", "age", "sex")) %>% 
        left_join(upc, by = c("gss_code", "age", "sex")) %>%
        tidyr::replace_na(list(upc = 0)) %>%
        mutate(next_popn = popn - int_out + int_in - dom_out + dom_in + upc)
    }
    
    # FIXME / TODO This setup creates negative populations - should we add
    # deaths/int/domestic migration as soon as each is calculated?? For now
    # I'm just setting -ve pops to zero and noting this in the pull request
    
    if(any(next_yr_popn$next_popn < 0)) {
      ix <- next_yr_popn$next_popn < 0
      sum_negative <- sum(next_yr_popn$next_popn[ix])
      
      warning(paste0(capture.output({
        print(paste("Negative populations were created in the", my_year, "loop, summing to", sum_negative))
        
        if(sum(ix) < 20) {
          print("Values:")
          print(next_yr_popn[ix,])
        } else {
          print("First 20 values:")
          print(next_yr_popn[ix,][1:20,])
          print("Levels affected:")
          sapply(c("gss_code", "age", "sex"), function(col) {
            print("col:")
            print(unique(next_yr_popn[ix, col]))
          })
        }
      }), collapse = "\n"))
      
      next_yr_popn <- mutate(next_yr_popn, next_popn = ifelse(next_popn < 0, 0, next_popn))
    }
    
    next_yr_popn <- select(next_yr_popn, year, gss_code, age, sex, popn = next_popn)
    
    validate_population(next_yr_popn, col_data = "popn",
                        comparison_pop = curr_yr_popn,
                        col_comparison = c("gss_code","sex","age"))
    
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
  proj_births_by_mother <- data.frame(data.table::rbindlist(proj_births_by_mother, use.names=TRUE))
  
  message(" ")
  
  #For int_out and domestic the rate is constant so there is no need to output all years
  int_out_rate <- filter(int_out_rate, year <= first_proj_yr)
  
  return(list(population = proj_popn,
              deaths = proj_deaths,
              births = proj_births,
              int_out = proj_int_out,
              int_in = proj_int_in,
              dom_out = proj_dom_out,
              dom_in = proj_dom_in,
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
  popmodules::validate_population(int_out_rate, col_data = ifelse(int_out_flow_or_rate == "flow", "int_out", "int_out"))
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
    assert_that(max(int_out_rate$int_out) <= 1 & min(int_out_rate$int_out) >= 0, msg = "projected international out migration rate contains rates outside the range 0-1")
  }
  assert_that(max(dom_rate$rate) <= 1 & min(dom_rate$rate) >= 0, msg = "projected domestic migration rate contains rates outside the range 0-1")
  
  invisible(TRUE)
}
