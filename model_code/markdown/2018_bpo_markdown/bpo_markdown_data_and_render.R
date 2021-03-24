bpo_markdown_data_and_render <- function(bpo_name, output_dir = "C:/temp", root_dir){
  
  library(dplyr)
  library(ggplot2)
  #output
  output_dir = paste0(root_dir,"/",output_dir,"/",bpo_name,"/")
  dir.create(output_dir, showWarnings = TRUE, recursive = TRUE)
  
  #get paths of 3 sceanrios
  bpo_dir <- paste0(root_dir,"/outputs/housing_led/2018/bpo")
  bpo_projections <- list.dirs(bpo_dir, recursive = F)
  
  low_dir <- bpo_projections[grepl(paste0(bpo_name,"_low_migration"),bpo_projections)] %>% last()
  med_dir <- bpo_projections[grepl(paste0(bpo_name,"_medium_migration"),bpo_projections)] %>% last()
  high_dir <- bpo_projections[grepl(paste0(bpo_name,"_high_migration"),bpo_projections)] %>% last()
  
  #get borough
  borough_name <- substr(bpo_name,1,gregexpr(pattern ='_',bpo_name)[[1]][1]-1)
  if(borough_name==""){borough_name <- bpo_name}
  
  borough_code <- readRDS(paste0(root_dir,"/input_data/lookup/lad18_and_region_code_to_name.rds")) %>%
    filter(substr(gss_code,1,3) == "E09") %>%
    mutate(partial_name = substr(toupper(gss_name),1,nchar(borough_name))) %>%
    filter(partial_name == toupper(borough_name))
  borough_code <- borough_code$gss_code %>% as.character()
  
  #read in data
  get_data <- function(dir, scenario, borough_code){
    
    ward_pop <- readRDS(paste0(dir,"/ward/population_ward.rds")) %>%
      filter(gss_code == borough_code) %>%
      mutate(scenario = scenario) %>%
      dtplyr::lazy_dt()
    
    borough_pop <- ward_pop %>%
      group_by(year, gss_code, borough, sex, age, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    total_borough_pop <- ward_pop %>%
      group_by(year, gss_code, borough, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    total_ward_pop <- ward_pop %>%
      group_by(year, gss_code_ward, ward_name, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    ward_pop_change <- total_ward_pop %>%
      filter(year == 2011) %>%
      select(gss_code_ward, base_pop = popn) %>%
      left_join(total_ward_pop, by="gss_code_ward") %>%
      mutate(change = ((popn - base_pop)/base_pop)*100) %>%
      mutate(area = "ward") %>%
      select(year, gss_code_ward, change, area)
    
    borough_pop_change <- total_borough_pop %>%
      filter(year == 2011) %>%
      select(gss_code, base_pop = popn) %>%
      left_join(total_borough_pop, by="gss_code") %>%
      mutate(change = ((popn - base_pop)/base_pop)*100) %>%
      mutate(area = "borough") %>%
      select(year, change, area)
    
    ward_age_structure <- ward_pop %>%
      filter(year %in% c(2011,2018,2033)) %>%
      group_by(year, gss_code_ward, ward_name, age) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    borough_age_structure <- borough_pop %>%
      filter(year %in% c(2011,2018,2033)) %>%
      group_by(year, gss_code, borough, age) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    ward_primary <- ward_pop %>%
      filter(age %in% 4:10) %>%
      group_by(year, gss_code_ward, ward_name, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    borough_primary <- ward_pop %>%
      filter(age %in% 4:10) %>%
      group_by(year, gss_code, borough, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    ward_secondary <- ward_pop %>%
      filter(age %in% 11:15) %>%
      group_by(year, gss_code_ward, ward_name, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    borough_secondary <- ward_pop %>%
      filter(age %in% 11:15) %>%
      group_by(year, gss_code, borough, scenario) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame()
    
    ward_age_tbl <- ward_age_structure %>%
      mutate(age_group = case_when(age <4 ~ "under 4",
                                   age %in% 4:10 ~ "primary (4-10)",
                                   age %in% 11:15 ~ "secondary (11-15)",
                                   age %in% 16:64 ~ "working age (16-64)",
                                   age %in% 65:79 ~ "older (65-79)",
                                   age >79 ~ "elderly (80+)")) %>%
      group_by(year, gss_code_ward, age_group) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame() %>%
      mutate(label = paste0(scenario,"_",year)) %>%
      select(-year)
    
    borough_age_tbl <- borough_age_structure %>%
      mutate(age_group = case_when(age < 4 ~ "under 4",
                                   age %in% 4:10 ~ "primary (4-10)",
                                   age %in% 11:15 ~ "secondary (11-15)",
                                   age %in% 16:64 ~ "working age (16-64)",
                                   age %in% 65:79 ~ "older (65-79)",
                                   age >79 ~ "elderly (80+)")) %>%
      group_by(year, gss_code, age_group) %>%
      summarise(popn = sum(popn)) %>%
      as.data.frame() %>%
      mutate(label = paste0(scenario,"_",year)) %>%
      select(-year)
    
    ward_age_tbl <- total_ward_pop %>%
      filter(year %in% c(2011,2018,2033)) %>%
      mutate(age_group = "total") %>%
      mutate(label = paste0(scenario,"_",year)) %>%
      select(gss_code_ward, age_group, popn, label) %>%
      rbind(ward_age_tbl) %>%
      mutate(popn = round(popn,0)) %>%
      mutate(popn = format(popn, big.mark = ','))
    
    borough_age_tbl <- total_borough_pop %>%
      filter(year %in% c(2011,2018,2033)) %>%
      mutate(age_group = "total") %>%
      mutate(label = paste0(scenario,"_",year)) %>%
      select(gss_code, age_group, popn, label) %>%
      rbind(borough_age_tbl) %>%
      mutate(popn = round(popn,0)) %>%
      mutate(popn = format(popn, big.mark = ','))
    
    ward_migration <-readRDS(paste0(dir,"/ward/migration_ward.rds")) %>%
      filter(gss_code == borough_code) %>%
      mutate(scenario = scenario) %>%
      dtplyr::lazy_dt() %>%
      group_by(year, gss_code_ward, ward_name, scenario) %>%
      summarise(migration = sum(migration)) %>%
      as.data.frame()
    
    borough_migration <- ward_migration %>%
      mutate(gss_code = borough_code) %>%
      dtplyr::lazy_dt() %>%
      group_by(year, gss_code, scenario) %>%
      summarise(migration = sum(migration)) %>%
      as.data.frame()
    
    ward_births <-readRDS(paste0(dir,"/ward/births_ward.rds")) %>%
      filter(gss_code == borough_code) %>%
      mutate(scenario = scenario) %>%
      dtplyr::lazy_dt() %>%
      group_by(year, gss_code_ward, ward_name, scenario) %>%
      summarise(births = sum(births)) %>%
      as.data.frame()
    
    borough_births <- ward_births %>%
      mutate(gss_code = borough_code) %>%
      dtplyr::lazy_dt() %>%
      group_by(year, gss_code, scenario) %>%
      summarise(births = sum(births)) %>%
      as.data.frame()
    
    ward_deaths <-readRDS(paste0(dir,"/ward/deaths_ward.rds")) %>%
      filter(gss_code == borough_code) %>%
      mutate(scenario = scenario) %>%
      dtplyr::lazy_dt() %>%
      group_by(year, gss_code_ward, ward_name, scenario) %>%
      summarise(deaths = sum(deaths)) %>%
      as.data.frame()
    
    borough_deaths <- ward_deaths %>%
      mutate(gss_code = borough_code) %>%
      dtplyr::lazy_dt() %>%
      group_by(year, gss_code, scenario) %>%
      summarise(deaths = sum(deaths)) %>%
      as.data.frame()
    
    return(list(total_ward = total_ward_pop,
                total_borough = total_borough_pop,
                change = ward_pop_change,
                borough_change = borough_pop_change,
                primary = ward_primary,
                borough_primary = borough_primary,
                secondary = ward_secondary,
                borough_secondary = borough_secondary,
                age_structure = ward_age_structure,
                borough_age_structure = borough_age_structure,
                age_tbl = ward_age_tbl,
                borough_age_tbl = borough_age_tbl,
                migration = ward_migration,
                borough_migration = borough_migration,
                births = ward_births,
                borough_births = borough_births,
                deaths = ward_deaths,
                borough_deaths = borough_deaths))
    
  }
  
  low <- get_data(low_dir, "low", borough_code)
  med <- get_data(med_dir, "medium", borough_code)
  high <- get_data(high_dir, "high", borough_code)
  
  #------------------------------------
  
  all_wards <- unique(low[["total_ward"]]$gss_code_ward)
  
  #Make charts
  chart_1 <- list()
  chart_primary <- list()
  chart_secondary <- list()
  chart_change_low <- list()
  chart_change_med <- list()
  chart_change_high <- list()
  chart_age_strcture_low <- list()
  chart_age_strcture_med <- list()
  chart_age_strcture_high <- list()
  chart_net_migration <- list()
  chart_births <- list()
  chart_deaths <- list()
  chart_dev <- list()
  age_table <- list()
  
  for(current_ward in all_wards){
    
    chart_1[[current_ward]] <- 
      rbind(low[["total_ward"]], med[["total_ward"]], high[["total_ward"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, popn, colour = scenario)) +
      geom_line(size=1.1) 
    
    chart_primary[[current_ward]] <- 
      rbind(low[["primary"]], med[["primary"]], high[["primary"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, popn, colour = scenario)) +
      geom_line(size=1.1) 
    
    chart_secondary[[current_ward]] <- 
      rbind(low[["secondary"]], med[["secondary"]], high[["secondary"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, popn, colour = scenario)) +
      geom_line(size=1.1) 
    
    chart_change_low[[current_ward]] <- 
      filter(low[["change"]], gss_code_ward == current_ward) %>%
      select(year, change, area) %>%
      rbind(low[["borough_change"]]) %>%
      ggplot(aes(year, change, colour = area )) +
      geom_line(size=1.1) 
    
    chart_change_med[[current_ward]] <- 
      filter(med[["change"]], gss_code_ward == current_ward) %>%
      select(year, change, area) %>%
      rbind(med[["borough_change"]]) %>%
      ggplot(aes(year, change, colour = area )) +
      geom_line(size=1.1)
    
    chart_change_high[[current_ward]] <- 
      filter(high[["change"]], gss_code_ward == current_ward) %>%
      select(year, change, area) %>%
      rbind(high[["borough_change"]]) %>%
      ggplot(aes(year, change, colour = area )) +
      geom_line(size=1.1) 
    
    chart_age_strcture_low[[current_ward]] <- 
      filter(low[["age_structure"]], gss_code_ward == current_ward) %>%
      mutate(year = as.character(year)) %>%
      ggplot(aes(age, popn, colour = year)) +
      geom_line(size=1.1) 
    
    chart_age_strcture_med[[current_ward]] <- 
      filter(med[["age_structure"]], gss_code_ward == current_ward) %>%
      mutate(year = as.character(year)) %>%
      ggplot(aes(age, popn, colour = year)) +
      geom_line(size=1.1) 
    
    chart_age_strcture_high[[current_ward]] <- 
      filter(high[["age_structure"]], gss_code_ward == current_ward) %>%
      mutate(year = as.character(year)) %>%
      ggplot(aes(age, popn, colour = year)) +
      geom_line(size=1.1) 
    
    #table
    age_table[[current_ward]] <- rbind(low[["age_tbl"]],
                                       med[["age_tbl"]],
                                       high[["age_tbl"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      tidyr::pivot_wider(names_from=label, values_from = popn) %>%
      mutate(order = case_when(substr(age_group,1,1)=="u" ~ 1,
                               substr(age_group,1,1)=="p" ~ 2,
                               substr(age_group,1,1)=="s" ~ 3,
                               substr(age_group,1,1)=="w" ~ 4,
                               substr(age_group,1,1)=="o" ~ 5,
                               substr(age_group,1,1)=="e" ~ 6,
                               substr(age_group,1,1)=="t" ~ 7)) %>%
      arrange(order) %>%
      select(age_group, low_2011, low_2018, low_2033, medium_2033, high_2033) %>%
      data.table::setnames(c("age group","2011", "2018",
                             "low 2033", "medium 2033","high 2033"))%>%
      as.data.frame()
    
    chart_net_migration[[current_ward]] <- 
      rbind(low[["migration"]], med[["migration"]], high[["migration"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, migration, colour = scenario)) +
      geom_line(size=1.1)
    
    chart_births[[current_ward]] <- 
      rbind(low[["births"]], med[["births"]], high[["births"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, births, colour = scenario)) +
      geom_line(size=1.1)
    
    chart_deaths[[current_ward]] <- 
      rbind(low[["deaths"]], med[["deaths"]], high[["deaths"]]) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, deaths, colour = scenario)) +
      geom_line(size=1.1)
    
    chart_dev[[current_ward]] <- 
      readRDS(paste0(low_dir,"/ward/assumed_development_ward.rds")) %>%
      filter(gss_code_ward == current_ward) %>%
      ggplot(aes(year, units)) +
      geom_line(size=1.1)
  }
  
  ward_name_lookup <- readRDS(paste0(root_dir,"/input_data/lookup/2011_ward_to_district.rds"))
  
  for(current_ward in all_wards){
    ward_name <- filter(ward_name_lookup, gss_code_ward == current_ward)[1,2]
    
    rmarkdown::render(paste0(root_dir,"/model_code/markdown/bpo_markdown/bpo_markdown_template.Rmd"),
                      output_file = paste0(output_dir, current_ward," - ",ward_name,".html"),
                      params = list(set_title = paste0("BPO projection - ",ward_name)))
  }
  
  chart_1[['borough']] <- 
    rbind(low[["total_borough"]], med[["total_borough"]], high[["total_borough"]]) %>%
    ggplot(aes(year, popn, colour = scenario)) +
    geom_line(size=1.1) 
  
  chart_primary[['borough']] <- 
    rbind(low[["borough_primary"]], med[["borough_primary"]], high[["borough_primary"]]) %>%
    ggplot(aes(year, popn, colour = scenario)) +
    geom_line(size=1.1) 
  
  chart_secondary[['borough']] <- 
    rbind(low[["borough_secondary"]], med[["borough_secondary"]], high[["borough_secondary"]]) %>%
    ggplot(aes(year, popn, colour = scenario)) +
    geom_line(size=1.1) 
  
  chart_change_low[['borough']] <- 
    low[["borough_change"]] %>%
    ggplot(aes(year, change, colour = area )) +
    geom_line(size=1.1) 
  
  chart_change_med[['borough']] <- 
    med[["borough_change"]] %>%
    ggplot(aes(year, change, colour = area )) +
    geom_line(size=1.1)
  
  chart_change_high[['borough']] <- 
    high[["borough_change"]] %>%
    ggplot(aes(year, change, colour = area )) +
    geom_line(size=1.1) 
  
  chart_age_strcture_low[['borough']] <- 
    low[["borough_age_structure"]] %>%
    mutate(year = as.character(year)) %>%
    ggplot(aes(age, popn, colour = year)) +
    geom_line(size=1.1) 
  
  chart_age_strcture_med[['borough']] <- 
    med[["borough_age_structure"]] %>%
    mutate(year = as.character(year)) %>%
    ggplot(aes(age, popn, colour = year)) +
    geom_line(size=1.1) 
  
  chart_age_strcture_high[['borough']] <- 
    high[["borough_age_structure"]] %>%
    mutate(year = as.character(year)) %>%
    ggplot(aes(age, popn, colour = year)) +
    geom_line(size=1.1) 
  
  #table
  age_table[['borough']] <- rbind(low[["borough_age_tbl"]],
                                  med[["borough_age_tbl"]],
                                  high[["borough_age_tbl"]]) %>%
    tidyr::pivot_wider(names_from=label, values_from = popn) %>%
    mutate(order = case_when(substr(age_group,1,1)=="u" ~ 1,
                             substr(age_group,1,1)=="p" ~ 2,
                             substr(age_group,1,1)=="s" ~ 3,
                             substr(age_group,1,1)=="w" ~ 4,
                             substr(age_group,1,1)=="o" ~ 5,
                             substr(age_group,1,1)=="e" ~ 6,
                             substr(age_group,1,1)=="t" ~ 7)) %>%
    arrange(order) %>%
    select(age_group, low_2011, low_2018, low_2033, medium_2033, high_2033) %>%
    data.table::setnames(c("age group","2011", "2018",
                           "low 2033", "medium 2033","high 2033"))%>%
    as.data.frame()
  
  chart_net_migration[['borough']] <- 
    rbind(low[["borough_migration"]], med[["borough_migration"]], high[["borough_migration"]]) %>%
    ggplot(aes(year, migration, colour = scenario)) +
    geom_line(size=1.1)
  
  chart_births[['borough']] <- 
    rbind(low[["borough_births"]], med[["borough_births"]], high[["borough_births"]]) %>%
    ggplot(aes(year, births, colour = scenario)) +
    geom_line(size=1.1)
  
  chart_deaths[['borough']] <- 
    rbind(low[["borough_deaths"]], med[["borough_deaths"]], high[["borough_deaths"]]) %>%
    ggplot(aes(year, deaths, colour = scenario)) +
    geom_line(size=1.1)
  
  chart_dev[['borough']] <-
    data.table::fread(paste0(low_dir,"/csv/assumed_dev.csv"), header=TRUE) %>%
    as.data.frame() %>%
    filter(gss_code == borough_code) %>%
    tidyr::pivot_longer(names_to = 'year', values_to = 'units', cols = 3:41) %>%
    as.data.frame() %>%
    filter(year <= 2041) %>%
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(year, units)) +
    geom_line(size=1.1)
  
  current_ward <- 'borough'
  ward_name <- borough_name
  rmarkdown::render(paste0(root_dir,"/model_code/markdown/bpo_markdown/bpo_markdown_template.Rmd"),
                    output_file = paste0(output_dir, borough_name," - borough total.html"),
                    params = list(set_title = paste0("BPO projection - ",borough_name)))
  
  
}
