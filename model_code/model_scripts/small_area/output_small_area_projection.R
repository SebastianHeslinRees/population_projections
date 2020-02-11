output_small_area_projection <- function(projection, output_dir, projection_type,
                                         births, deaths, first_proj_yr,
                                         lookup, bpo=FALSE){
  
  borough_names <- data.table::fread("input_data/lookup/lad18_code_to_name.csv")
  
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
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(2011:max(popn$year)))
  
  males <- filter(popn, sex == "male", year >= 2011) %>%
    mutate(popn = round(popn, 2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(2011:max(popn$year)))
  
  persons <- mutate(popn, sex = "persons") %>%
    dtplyr::lazy_dt() %>%
    filter(year >= 2011) %>%
    group_by_at(c("year", "gss_code", "borough", code, name, "sex", "age")) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    mutate(popn = round(popn, 2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
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
  
  components <- data.table::rbindlist(components, use.names = TRUE) %>%
    as.data.frame() %>%
    select_at(c("year","gss_code","borough", code, name, "value", "component")) %>%
    rbind(births, deaths) %>%
    dtplyr::lazy_dt() %>%
    group_by_at(c("year","gss_code","borough", code, name, "component")) %>%
    summarise(value = sum(value)) %>%
    as.data.frame() %>%
    mutate(value = round(value, 2)) %>%
    tidyr::pivot_wider(names_from = component, values_from = value) %>%
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
    data.table::fwrite(csvs[[i]], paste0(output_dir, names(csvs)[i], "_", projection_type, ".csv"))
  }
  
  if(bpo != FALSE){

    bpo_data <- function(x, bpo_gss=bpo,
                         col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name", "sex", "age")){
      y <- x %>%
        dtplyr::lazy_dt() %>%
        filter(gss_code == bpo_gss) %>%
        mutate(gss_code_ward = gss_code,
               ward_name = paste0(borough, " (total)")) %>%
        group_by_at(col_aggregation) %>%
        summarise_all(.funs=sum) %>%
        as.data.frame() %>%
        rbind(x) %>%
        dtplyr::lazy_dt() %>%
        filter(gss_code == bpo_gss) %>%
        as.data.frame()
    }
    
    wb <- xlsx::loadWorkbook("input_data/housing_led_model/ward_housing_led_2018_based_template.xlsx")
    wb_sheets<- getSheets(wb)
    
    xlsx::addDataFrame(bpo_data(persons), wb_sheets$Persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
    xlsx::addDataFrame(bpo_data(males), wb_sheets$Males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
    xlsx::addDataFrame(bpo_data(females), wb_sheets$Females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
    xlsx::addDataFrame(bpo_data(components, col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name","year")),
                       wb_sheets$Components, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
    
    wb_filename <- paste0(output_dir,"bpo_workbook.xlsx")
    saveWorkbook(wb, wb_filename)
    
  }
}


