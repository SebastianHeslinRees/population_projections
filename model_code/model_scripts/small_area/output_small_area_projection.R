output_small_area_projection <- function(projection, output_dir, projection_name,
                                         projection_type, lookup){

  borough_names <- data.table::fread("input_data/lookup/lad18_code_to_name.csv")
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  code <- paste0("gss_code_",projection_type)
  name <- paste0(projection_type, "_name")
  
  x <- list()
  for(i in 1:4){
    x[[i]] <- projection[[i]] %>%
      left_join(lookup, by=c("gss_code","gss_code_small_area")) %>%
      left_join(borough_names, by = "gss_code") %>%
      rename(borough = gss_name) %>%
      rename(!!code := gss_code_small_area)
  }
  
  saveRDS(x, paste0(output_dir, projection_name, ".rds"))

  popn <- x[[1]]
  
  females <- filter(popn, sex == "female") %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(min(popn$year):max(popn$year)))
  
  males <- ales <- filter(popn, sex == "male") %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(min(popn$year):max(popn$year)))
  
  persons <- mutate(popn, sex = "persons") %>%
    dtplyr::lazy_dt() %>%
    group_by_at(c("year", "gss_code", "borough", code, name, "sex", "age")) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select(gss_code, borough, code, name, sex, age, as.character(min(popn$year):max(popn$year)))
  
  components <- list()
  for(i in 1:4){
    nm <- last(names(projection[[i]]))
    components[[i]] <- rename(x[[i]], value := !!sym(nm)) %>%
      mutate(component = nm)
  }
  
  componentsx <- data.table::rbindlist(components, use.names = TRUE) %>%
    dtplyr::lazy_dt() %>%
    group_by_at(c("year","gss_code","borough", code, name, "component")) %>%
    summarise(value = sum(value)) %>%
    as.data.frame() %>%
    tidyr::pivot_wider(names_from = component, values_from = value) %>%
    mutate(total_change = births - deaths + migration) %>%
    select(gss_code, borough, code, name, year, popn, births, deaths, migration, total_change) %>%
    arrange_at(c("gss_code", code, "year"))
  
  csvs <- list(persons=persons, males=males, females=females, components=components)
  lapply(seq_along(csvs),
         function(i) data.table::fwrite(csvs[[i]],
                                        paste0(output_dir, names(csvs)[i], "_", projection_type, ".csv"))) %>%
    invisible()
}
