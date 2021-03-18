#Data for BPO rmd
library(data.table)
library(dplyr)

#-------------------------------------------------------------------------------

read_data <- function(data_list, component, gss){
  df <- list()
  for(i in 1:length(data_list)){
    df[[i]] <- readRDS(paste0(data_list[[i]], "/", component, ".rds")) %>% 
      mutate(variant = names(data_list)[i]) %>% 
      filter(gss_code == gss)
  }
  df <- rbindlist(df, use.names = TRUE)
  return(data.frame(df))
}

#-------------------------------------------------------------------------------

read_migration <- function(data_list, gss){
  
  dom_in <- read_data(data_list, "dom_in", gss) %>%
    rename(value = dom_in)
  
  dom_out <- read_data(data_list, "dom_out", gss) %>%
    mutate(value = dom_out*-1) %>%
    select(names(dom_in))
  
  int_in <- read_data(data_list, "int_in", gss) %>%
    rename(value = int_in)
  
  int_out <- read_data(data_list, "int_out", gss) %>%
    mutate(value = int_out*-1) %>%
    select(names(dom_in))
  
  net <- bind_rows(dom_in, dom_out, int_in, int_out) %>% 
    dtplyr::lazy_dt() %>% 
    filter(gss_code == gss) %>% 
    group_by(gss_code, year, sex, age, variant) %>% 
    summarise(value = sum(value)) %>% 
    data.frame()
  
  return(net)
}

#-------------------------------------------------------------------------------

get_bpo_data <- function(x, gss_borough){
  #browser()
  borough <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% 
    filter(gss_code == gss_borough) %>% 
    .$gss_name %>% 
    unique()

  births_ward <- read_data(x, "ward/births_ward", gss_borough) %>%
    select(year, gss_code = gss_code_ward, ward_name, sex, age, variant, births)

  deaths_ward <- read_data(x, "ward/deaths_ward", gss_borough) %>%
    select(year, gss_code = gss_code_ward, ward_name, sex, age, variant, deaths)

  population_ward <- read_data(x, "ward/population_ward", gss_borough) %>%
    select(year, gss_code = gss_code_ward, ward_name, sex, age, variant, popn)
  
  migration_ward <- read_data(x, "ward/migration_ward", gss_borough) %>% 
    select(year, gss_code = gss_code_ward, ward_name, sex, age, variant, value=migration)
  
  assumed_dev <- read_data(x, "ward/assumed_development_ward", gss_borough) %>% 
    mutate(variant = "assumed development") %>% 
    unique()
  
  births <- births_ward %>% 
    mutate(gss_code = gss_borough,
           ward_name = borough) %>% 
    group_by(year, gss_code, ward_name, sex, age, variant) %>% 
    summarise(births = sum(births)) %>% 
    data.frame() %>% 
    rbind(births_ward)
  
  deaths <- deaths_ward %>% 
    mutate(gss_code = gss_borough,
           ward_name = borough) %>% 
    group_by(year, gss_code, ward_name, sex, age, variant) %>% 
    summarise(deaths = sum(deaths)) %>% 
    data.frame() %>% 
    rbind(deaths_ward)
  
  population <- population_ward %>% 
    mutate(gss_code = gss_borough,
           ward_name = borough) %>% 
    group_by(year, gss_code, ward_name, sex, age, variant) %>% 
    summarise(popn = sum(popn)) %>% 
    data.frame() %>% 
    rbind(population_ward)
  
  migration <- migration_ward %>% 
    mutate(gss_code = gss_borough,
           ward_name = borough) %>% 
    group_by(year, gss_code, ward_name, sex, age, variant) %>% 
    summarise(value = sum(value)) %>% 
    data.frame() %>% 
    rbind(migration_ward)
  
  data_return <- list(borough_name = borough,
                      births = births,
                      deaths = deaths,
                      population = population,
                      migration = migration,
                      assumed_dev = assumed_dev)
  
  return(data_return)
}

#-------------------------------------------------------------------------------

