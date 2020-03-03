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
#' @param births A dataframe. The projeted births output from the small area model
#' @param deaths A dataframe. The projeted deaths output from the small area model
#' @param first_proj_yr Numeric. The first year of the projection
#' @param lookup A dataframe. A gss_code to name lookup for the modelled areas
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom dtplyr lazy_dt
#' @importFrom data.table fread fwrite rbindlist
#' 
#' @export

output_small_area_projection <- function(projection, output_dir, projection_type,
                                         births, deaths, first_proj_yr,
                                         lookup){
  
  borough_names <- fread("input_data/lookup/lad18_code_to_name.csv")
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  code <- paste0("gss_code_",projection_type)
  name <- paste0(projection_type, "_name")
  
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
  
  proj_output[["assumed_development"]] <- projection[["assumed_development"]] %>%
    left_join(lookup, by=c("gss_code_small_area")) %>%
    left_join(borough_names, by = "gss_code") %>%
    rename(!!code := gss_code_small_area) %>%
    rename(borough = gss_name) %>%
    select_at(c("year", "gss_code", "borough", code, name, "units")) %>%
    arrange_at(c("gss_code", code, "year"))
  
  for(i in seq_along(proj_output)) {
    saveRDS(proj_output[[i]], paste0(output_dir, names(proj_output)[i],"_",projection_type,".rds"))
  }
  
  #Published Ouputs (csv)
  popn <- proj_output[["population"]]
  
  females <- filter(popn, sex == "female", year >= 2011) %>%
    mutate(popn = round(popn, 2)) %>%
    pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(2011:max(popn$year)))
  
  males <- filter(popn, sex == "male", year >= 2011) %>%
    mutate(popn = round(popn, 2)) %>%
    pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(2011:max(popn$year)))
  
  persons <- mutate(popn, sex = "persons") %>%
    lazy_dt() %>%
    filter(year >= 2011) %>%
    group_by_at(c("year", "gss_code", "borough", code, name, "sex", "age")) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    mutate(popn = round(popn, 2)) %>%
    pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(2011:max(popn$year)))
  
  components <- list()
  for(i in names(projection)[1:4]){
    nm <- last(names(projection[[i]]))
    components[[i]] <- rename(proj_output[[i]], value := !!sym(nm)) %>%
      mutate(component = nm)
  }
  
  births <- filter(births, year %in% 2011:(first_proj_yr-1)) %>%
    left_join(lookup, by=c("gss_code_small_area")) %>%
    left_join(borough_names, by = "gss_code") %>%
    rename(borough = gss_name) %>%
    rename(!!code := gss_code_small_area) %>%
    rename(value = births) %>%
    mutate(component = "births") %>%
    select_at(c("year","gss_code","borough", code, name, "value", "component"))
  
  deaths <- filter(deaths, year %in% 2011:(first_proj_yr-1)) %>%
    left_join(lookup, by=c("gss_code_small_area")) %>%
    left_join(borough_names, by = "gss_code") %>%
    rename(borough = gss_name) %>%
    rename(!!code := gss_code_small_area) %>%
    rename(value = deaths) %>%
    mutate(component = "deaths") %>%
    select_at(c("year","gss_code","borough", code, name, "value", "component"))
  
  components <- rbindlist(components, use.names = TRUE) %>%
    as.data.frame() %>%
    select_at(c("year","gss_code","borough", code, name, "value", "component")) %>%
    rbind(births, deaths) %>%
    lazy_dt() %>%
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
    select(gss_code, borough, code, name, year, popn, births, deaths, migration, total_change) %>%
    arrange_at(c("gss_code", code, "year"))
  
  csvs <- list(persons=persons, males=males, females=females, components=components)
  for(i in seq_along(csvs)) {
    fwrite(csvs[[i]], paste0(output_dir, names(csvs)[i], "_", projection_type, ".csv"))
  }
  
  projection[["csvs"]] <- csvs
  
  return(projection)
}

