#' Output the results of the housing-led model
#' 
#' Save RDS and CSV files to a specified folder path. Outputs include population,
#' components of change (including backseries data) and the input dwelling and
#' household trajectories.
#'
#' @param projection A list. The output from the run_housing_led_model function
#' @param output_dir A string. The directory in which to save the model output files
#' @param external_trend_path A string. The output path of the trend model used to
#'   run the housing-led model. This is uses to import the backseries
#'   components.
#' @param additional_dwellings A dataframe. The dwelling trajectory used in the
#'   projection model.
#' @param housing_stock A dataframe. The total dwelling stock by year.
#' @param household_trajectory A dataframe. The household trectory (as opposed
#'   to the dwelling trajectory) used in the model.
#' @param first_proj_yr Numeric. The first year of the projection
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom assertthat assert_that
#' @importFrom dtplyr lazy_dt
#' @importFrom data.table fread fwrite rbindlist

output_housing_led_projection <- function(projection, output_dir,
                                          external_trend_path,
                                          additional_dwellings, housing_stock,
                                          household_trajectory,
                                          first_proj_yr){
  
  dir.create(output_dir, recursive = T, showWarnings = FALSE)

  # Add backseries to projection
  backseries <- get_data_from_file(
    list(population = paste0(external_trend_path,"population.rds"),
         births = paste0(external_trend_path,"births.rds"),
         deaths = paste0(external_trend_path,"deaths.rds"),
         int_out = paste0(external_trend_path,"int_out.rds"),
         int_in = paste0(external_trend_path,"int_in.rds"),
         dom_out = paste0(external_trend_path,"dom_out.rds"),
         dom_in = paste0(external_trend_path,"dom_in.rds")
    )) %>%
    lapply(filter_to_LAs)
  
  
  for(x in names(backseries)){
    
    backseries[[x]] <- filter(backseries[[x]], year %in% 2011:(first_proj_yr-1))
    projection[[x]] <- filter(projection[[x]], year >= first_proj_yr)
    
    projection[[x]] <- rbindlist(list(backseries[[x]], projection[[x]]),
                                             use.names = TRUE) %>%
      as.data.frame()
  }
  
  for(i in 1:13) {
    col_aggregation = intersect(c("year", "gss_code", "age", "sex"), names(projection[[i]]))
    col_data <- setdiff(names(projection[[i]]), col_aggregation)
    comparison_pop <- projection$population
    if(names(projection)[i] %in% c("ahs", "ahs_choice", "household_population", "adjusted_domestic_migration")) {
      comparison_pop <- filter(comparison_pop, substr(gss_code, 1, 3) == "E09")
    }
    if(names(projection)[i] %in% c("ahs", "ahs_choice", "household_population", "trend_population", "adjusted_domestic_migration", "unconstrained_population")) {
      comparison_pop <- filter(comparison_pop, year >= min(projection[[i]]$year))
    }
    if(identical(col_data, "births")) {
      col_comparison <- setdiff(col_aggregation, "age")
    } else {
      col_comparison <- col_aggregation
    }
    check_negative <- !(names(projection)[i] %in% c("adjusted_domestic_migration", "unconstrained_population"))
    validate_population(projection[[i]],
                        col_aggregation = col_aggregation,
                        col_data = col_data,
                        check_negative_values = check_negative,
                        comparison_pop = comparison_pop,
                        col_comparison = col_comparison)
    
    saveRDS(projection[[i]], paste0(output_dir, names(projection)[i],".rds"))
  }
  
  if(!is.null(projection[['upc']])){
    saveRDS(projection[['upc']], paste0(output_dir, "upc.rds"))
  }
  
  validate_population(household_trajectory, col_aggregation = c("gss_code", "year"))
  saveRDS(household_trajectory, paste0(output_dir, "household_trajectory.rds"))
  
  # Create extra tables to to output
  names_lookup <- get_gss_names()

  popn <- left_join(projection[["population"]], names_lookup, by="gss_code") %>%
    filter(substr(gss_code,1,3)=="E09")

  london_totals <- function(data, col_aggregation=setdiff(names(data),data_col), data_col){

    assertthat::assert_that("gss_code" %in% names(data))
    assertthat::assert_that(all(grepl("E09", data$gss_code)))
    

      x <- arrange(data, gss_code, year)

      x <- x %>%
        mutate(gss_code = "E12000007") %>%
        mutate(gss_name = "London (total)") %>%
        dtplyr::lazy_dt() %>%
        group_by_at(col_aggregation) %>%
        summarise(!!data_col := sum(!!sym(data_col))) %>%
        as.data.frame() %>%
        select(names(data)) %>%
        rbind(x)
      
      return(x)
    
  }
  
  order_gss_with_london_total_first <- function(data) {
    assertthat::assert_that("gss_code" %in% names(data))
    data %>%
      mutate(gss_code = factor(gss_code, levels = c("E12000007", paste0("E0900000", 1:9), paste0("E090000", 10:33)))) %>%
      arrange(gss_code) %>%
      mutate(gss_code = as.character(gss_code))
  }
  
  females <- filter(popn, sex == "female") %>%
    london_totals(data_col = "popn") %>%
    mutate(popn = round(popn, digits=2)) %>%
    pivot_wider(names_from = year, values_from = popn) %>%
    rename(borough = gss_name) %>%
    select(gss_code, borough, sex, age, as.character(min(popn$year):max(popn$year)))
  
  males <- filter(popn, sex == "male") %>%
    london_totals(data_col = "popn") %>%
    mutate(popn = round(popn, digits=2)) %>%
    pivot_wider(names_from = year, values_from = popn) %>%
    rename(borough = gss_name) %>%
    select(gss_code, borough, sex, age, as.character(min(popn$year):max(popn$year)))
  
  persons <- mutate(popn, sex = "persons") %>%
    london_totals(data_col = "popn") %>%
    dtplyr::lazy_dt() %>%
    group_by(gss_code, gss_name, year, sex, age) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    mutate(popn = round(popn, digits=2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    order_gss_with_london_total_first() %>%
    rename(borough = gss_name) %>%
    select(gss_code, borough, sex, age, as.character(min(popn$year):max(popn$year)))
  
  components <- list()

  # Rename last column (containing component data) in each output dataframe to
  # 'value' so we can rbind them
  for(x in names(projection)){
    nm <- last(names(projection[[x]]))
    if(x %in% c("population","births","deaths","int_out","int_in","dom_out","dom_in")){
      components[[x]] <- rename(projection[[x]], value := !!sym(nm)) %>%
        mutate(component = nm) %>%
        filter(substr(gss_code,1,3)=="E09")%>%
        left_join(names_lookup, by="gss_code") %>%
        dtplyr::lazy_dt() %>%
        group_by(year, gss_code, gss_name, component) %>%
        summarise(value = sum(value)) %>%
        as.data.frame() %>%
        mutate(value = round(value, digits=2))
    }
  }
  

  components <- data.table::rbindlist(components, use.names = TRUE) %>%
    london_totals(data_col = "value") %>%
    tidyr::pivot_wider(names_from = component, values_from = value) %>%
    mutate(int_net = round(int_in - int_out, 2),
           dom_net = round(dom_in - dom_out, 2),
           total_change = round(births - deaths + int_net + dom_net, 2),
           borough = gss_name) %>%
    select(gss_code, borough, year, popn, births, deaths, int_in, int_out, int_net,
           dom_in, dom_out, dom_net, total_change) %>%
    arrange(gss_code, year) %>%
    order_gss_with_london_total_first()
  
  # All London dom_in/out isn't just the sum of components, and we can't calculate it in this model
  components[components$gss_code == "E12000007", c("dom_in", "dom_out")] <- NA
  
  
  if("ahs" %in% names(projection[["ahs_choice"]])) {
    ahs <- projection[["ahs_choice"]]
  } else {
    ahs <- left_join(projection[["ahs"]], projection[["ahs_choice"]], by = c("year", "gss_code"))
  }
  ahs <- arrange(ahs, year, gss_code)
  
  stock <- housing_stock %>%
    arrange(gss_code, year) %>%
    left_join(names_lookup, by="gss_code") %>%
    london_totals(data_col = "dwellings") %>%
    mutate(dwellings = round(dwellings, 2)) %>%
    tidyr::pivot_wider(names_from = "year", values_from = "dwellings") %>%
    rename(borough = gss_name)

  annual_dev <- additional_dwellings %>%
    filter(year != 2011) %>%
    arrange(gss_code, year) %>%
    left_join(names_lookup, by="gss_code") %>%
    london_totals(data_col = "units") %>%
    mutate(units = round(units, 2)) %>%
    pivot_wider(names_from = "year", values_from = "units") %>%
    rename(borough = gss_name)
  
  dir.create(paste0(output_dir,"csv"), showWarnings = FALSE)
  csvs <- list(persons=persons, males=males, females=females, components=components,
               assumed_dev = annual_dev, housing_stock = stock, ahs = ahs)
  
  for(i in seq_along(csvs)) {
    col_agg <- intersect(names(csvs[[i]]), c("gss_code", "age", "sex", "year"))
    col_data <- setdiff(names(csvs[[i]]), c(col_agg, "borough"))
    if(names(csvs)[i] == "components") {
      validation_data <- filter(csvs[[i]], substr(gss_code,1,3) != "E12")
    } else {
      validation_data <- csvs[[i]]
    }
    validate_population(validation_data, col_aggregation = col_agg, col_data = col_data, check_negative_values = FALSE)
    
    data.table::fwrite(csvs[[i]], paste0(output_dir, "csv/",names(csvs)[i],".csv"))
  }
  
}