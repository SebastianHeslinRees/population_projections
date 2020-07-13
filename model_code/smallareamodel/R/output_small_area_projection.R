#' Output the results of the housing-led model
#' 
#' Save RDS and CSV files to a specified folder path. Outputs include population,
#' components of change (including backseries data) and the input dwelling and
#' household trajectories.
#'
#' @param projection A list. The output from the run_housing_led_model function
#' @param output_dir A string. The directory in which to save the model output files
#' @param projection_type A string. Is the projection at ward or MSOA level? Used in
#'   naming the output files only
#' @param lookup A dataframe. A gss_code to name lookup for the modelled areas
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom dtplyr lazy_dt
#' @importFrom data.table fwrite rbindlist

output_small_area_projection <- function(projection, output_dir, projection_type, lookup){
  
  borough_names <- get_gss_names()

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  code <- paste0("gss_code_",projection_type)
  name <- paste0(projection_type, "_name")
  
  #add area names and arrange outputs
  proj_output <- list()
  for(i in names(projection)[1:4]){
    col_nm <- last(names(projection[[i]]))
    proj_output[[i]] <- projection[[i]] %>%
      left_join(lookup, by=c("gss_code","gss_code_small_area")) %>%
      left_join(borough_names, by = "gss_code") %>%
      rename(borough = gss_name) %>%
      rename(!!code := gss_code_small_area) %>%
      select_at(c("year", "gss_code", "borough", code, name, "sex", "age", col_nm)) %>%
      arrange_at(c("year", "gss_code", code, "sex", "age"))
  }
  
  #development data
  proj_output[["total_stock"]] <- projection[["assumed_development"]] %>%
    left_join(lookup, by=c("gss_code_small_area")) %>%
    left_join(borough_names, by = "gss_code") %>%
    rename(!!code := gss_code_small_area) %>%
    rename(borough = gss_name) %>%
    select_at(c("year", "gss_code", "borough", code, name, "units")) %>%
    arrange_at(c("gss_code", code, "year"))
  
  proj_output[["assumed_development"]] <- proj_output[["total_stock"]] %>%
    group_by_at(c("gss_code", "borough", code, name)) %>%
    mutate(lag_units = lag(units)) %>%
    mutate(units = units-lag_units) %>%
    as.data.frame() %>% 
    filter(year > 2011) %>%
    select_at(c("year", "gss_code", "borough", code, name, "units")) 
  
  for(i in seq_along(proj_output)) {
    saveRDS(proj_output[[i]], paste0(output_dir, names(proj_output)[i],"_",projection_type,".rds"))
  }
  
  #assumed dev csv
  assumed_dev <- proj_output[["assumed_development"]] %>%
    mutate(units = round(units, 2)) %>% 
    tidyr::pivot_wider(names_from = year, values_from = units)
  
  #Published Ouputs (csv)
  popn <- proj_output[["population"]]
  
  col_aggregation <- c("gss_code", "borough", code, name, "sex", "age", as.character(2011:max(popn$year)))
  
  females <- filter(popn, sex == "female", year >= 2011) %>%
    mutate(popn = round(popn, 2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select_at(col_aggregation)
  
  males <- filter(popn, sex == "male", year >= 2011) %>%
    mutate(popn = round(popn, 2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select_at(col_aggregation)

  
  persons <- mutate(popn, sex = "persons") %>%
    lazy_dt() %>%
    filter(year >= 2011) %>%
    group_by_at(c("year", "gss_code", "borough", code, name, "sex", "age")) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    mutate(popn = round(popn, 2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select_at(col_aggregation)

  #components of change output sheet
  #and residual migration claculation
  components <- list()
  for(i in names(projection)[1:4]){
    nm <- last(names(projection[[i]]))
    components[[i]] <- rename(proj_output[[i]], value := !!sym(nm)) %>%
      mutate(component = nm)
  }

  components <- data.table::rbindlist(components, use.names = TRUE) %>%
    as.data.frame() %>%
    select_at(c("year","gss_code","borough", code, name, "value", "component")) %>%
    dtplyr::lazy_dt() %>%
    group_by_at(c("year","gss_code","borough", code, name, "component")) %>%
    summarise(value = sum(value)) %>%
    as.data.frame() %>%
    mutate(value = round(value, 2)) %>%
    pivot_wider(names_from = component, values_from = value) %>%
    group_by_at(c("gss_code","borough", code, name)) %>%
    mutate(popn_lag = lag(popn)) %>%
    as.data.frame() %>%
    filter(year >= 2011) %>%
    mutate(migration = ifelse(!is.na(migration), migration,
                              round(popn - popn_lag + deaths - births, 2))) %>%
    mutate(total_change = round(births - deaths + migration, 2)) %>%
    select_at(c("gss_code", "borough", code, name, "year", "popn", "births", "deaths", "migration", "total_change")) %>%
    arrange_at(c("gss_code", code, "year"))
  
  csvs <- list(persons=persons, males=males, females=females, components=components, assumed_dev=assumed_dev)
  for(i in seq_along(csvs)) {
    fwrite(csvs[[i]], paste0(output_dir, names(csvs)[i], "_", projection_type, ".csv"))
  }
  
  projection[["csvs"]] <- csvs
  
  return(projection)
}

