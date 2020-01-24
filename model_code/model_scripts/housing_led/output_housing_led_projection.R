output_housing_led_projection <- function(projection, output_dir, timestamp){
  
  dir.create(output_dir, recursive = T, showWarnings = FALSE)

  lapply(seq_along(projection),
         function(i) saveRDS(projection[[i]],
                             paste0(output_dir, names(projection)[i],"_",timestamp,".rds")))
  
  names_lookup <- data.table::fread("input_data/lookup/lad18_code_to_name.csv") %>%
    as.data.frame()
  popn <- left_join(projection[["popn"]], names_lookup, by="gss_code") %>%
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
  for(i in 1:7){
    nm <- last(names(projection[[i]]))
    components[[i]] <- rename(projection[[i]], value := !!sym(nm)) %>%
      mutate(component = nm)
  }
  
  components <- data.table::rbindlist(components, use.names = TRUE) %>%
    dtplyr::lazy_dt() %>%
    filter(substr(gss_code,1,3)=="E09")%>%
    group_by(year, gss_code, component) %>%
    summarise(value = sum(value)) %>%
    mutate(value = round(value, digits=2)) %>%
    as.data.frame() %>%
    tidyr::pivot_wider(names_from = component, values_from = value) %>%
    left_join(names_lookup, by="gss_code") %>%
    mutate(int_net = round(int_in - int_out, 2),
           dom_net = round(dom_in - dom_out, 2),
           total_change = round(births - deaths + int_net + dom_net, 2),
           borough = gss_name) %>%
    select(gss_code, borough, year, births, deaths, int_in, int_out, int_net,
           dom_in, dom_out, dom_net, total_change) %>%
    arrange(gss_code, year)
  
  csvs <- list(persons=persons, males=males, females=females, components=components)
  lapply(seq_along(csvs),
         function(i) data.table::fwrite(csvs[[i]],
                                        paste0(output_dir, names(csvs)[i],"_",timestamp,".csv"))) %>%
    invisible()
  
}
