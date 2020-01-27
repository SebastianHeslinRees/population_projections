output_housing_led_projection <- function(projection, output_dir, timestamp,
                                          external_trend_path, external_trend_datestamp){
  
  dir.create(output_dir, recursive = T, showWarnings = FALSE)

  # Add backseries to projection
  backseries <- get_data_from_file(
    list(population = paste0(external_trend_path,"population_",external_trend_datestamp,".rds"),
         births = paste0(external_trend_path,"births_",external_trend_datestamp,".rds"),
         deaths = paste0(external_trend_path,"deaths_",external_trend_datestamp,".rds"),
         int_out = paste0(external_trend_path,"int_out_",external_trend_datestamp,".rds"),
         int_in = paste0(external_trend_path,"int_in_",external_trend_datestamp,".rds"),
         dom_out = paste0(external_trend_path,"dom_out_",external_trend_datestamp,".rds"),
         dom_in = paste0(external_trend_path,"dom_in_",external_trend_datestamp,".rds")
    ))
  
  for(x in names(backseries)){
    
    backseries[[x]] <- filter(backseries[[x]], year %in% 2011:(first_proj_yr-1))
    
    projection[[x]] <- data.table::rbindlist(list(backseries[[x]], projection[[x]]),
                                             use.names = TRUE) %>%
      as.data.frame()
  }
  
  for(i in seq_along(projection)) {
    saveRDS(projection[[i]], paste0(output_dir, names(projection)[i],"_",timestamp,".rds"))
  }
  
  # Create extra tables to to ouput
  names_lookup <- data.table::fread("input_data/lookup/lad18_code_to_name.csv") %>%
    as.data.frame()
  popn <- left_join(projection[["population"]], names_lookup, by="gss_code") %>%
    filter(substr(gss_code,1,3)=="E09")
  
  females <- filter(popn, sex == "female") %>%
    mutate(popn = round(popn, digits=2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    rename(borough = gss_name) %>%
    select(gss_code, borough, sex, age, as.character(min(popn$year):max(popn$year)))
  
  males <- filter(popn, sex == "male") %>%
    mutate(popn = round(popn, digits=2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    rename(borough = gss_name) %>%
    select(gss_code, borough, sex, age, as.character(min(popn$year):max(popn$year)))
  
  persons <- mutate(popn, sex = "persons") %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code, gss_name, sex, age) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    mutate(popn = round(popn, digits=2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    rename(borough = gss_name) %>%
    select(gss_code, borough, sex, age, as.character(min(popn$year):max(popn$year)))
  
  components <- list()
  # Rename last column (containing component data) in each output dataframe to
  # 'value' so we can rbind them
  for(x in names(projection)){
    nm <- last(names(projection[[x]]))
    components[[x]] <- rename(projection[[x]], value := !!sym(nm)) %>%
      mutate(component = nm) %>%
      dtplyr::lazy_dt() %>%
      filter(substr(gss_code,1,3)=="E09")%>%
      group_by(year, gss_code, component) %>%
      summarise(value = sum(value)) %>%
      as.data.frame()
      mutate(value = round(value, digits=2))
  }
  
  components <- data.table::rbindlist(components, use.names = TRUE) 
    tidyr::pivot_wider(names_from = component, values_from = value) %>%
    left_join(names_lookup, by="gss_code") %>%
    mutate(int_net = round(int_in - int_out, 2),
           dom_net = round(dom_in - dom_out, 2),
           total_change = round(births - deaths + int_net + dom_net, 2),
           borough = gss_name) %>%
    select(gss_code, borough, year, births, deaths, int_in, int_out, int_net,
           dom_in, dom_out, dom_net, total_change) %>%
    arrange(gss_code, year)
  
  stock <- housing_stock %>%
    left_join(names_lookup, by="gss_code") %>%
    mutate(dwellings = round(dwellings, 2)) %>%
    arrange(gss_code, year) %>%
    tidyr::pivot_wider(names_from = "year", values_from = "dwellings") %>%
    rename(borough = gss_name)
  
  annual_dev <- additional_dwellings %>%
    filter(year != 2011) %>%
    left_join(names_lookup, by="gss_code") %>%
    arrange(gss_code, year) %>%
    mutate(units = round(units, 2)) %>%
    tidyr::pivot_wider(names_from = "year", values_from = "units") %>%
    rename(borough = gss_name)
  
  dir.create(paste0(output_dir,"csv"), showWarnings = FALSE)
  csvs <- list(persons=persons, males=males, females=females, components=components,
               assumed_dev = annual_dev, housing_stock = stock)
  
  for(i in seq_along(csvs)) {
    data.table::fwrite(csvs[[i]], paste0(output_dir, names(csvs)[i],"_",timestamp,".csv"))
  }

}
