library(dplyr)
library(stringr)

camel <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}

bpo_rmd_2021 <- function(borough_name, proj_name = borough_name){
  
  message(paste(borough_name, "BPO RMD"))
  
  gss_code_to_name <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% filter(substr(gss_code,1,3)=="E09")
  #borough_gss <- filter(gss_code_to_name, gss_name == borough_name)$gss_code
  borough_gss <- filter(gss_code_to_name, str_detect(tolower(gss_name), tolower(borough_name)))$gss_code
  assertthat::assert_that(length(borough_gss)==1, msg = "problem with borough name")
  
  proj_name <- proj_name %>% tolower()
  proj_name <- str_replace_all(proj_name, " ", "_")
  
  projections <- list.dirs("outputs/flexible_area_model/2021_based/bpo", recursive = FALSE)
  
  projections <- projections[grepl(proj_name, projections)]
  
  scenario_1_path <- projections[grepl("5-year", tolower(projections))]
  scenario_2_path <- projections[grepl("10-year", tolower(projections))]
  
  assertthat::assert_that(length(scenario_1_path)==1, msg = "problem with 5-year projection path")
  assertthat::assert_that(length(scenario_2_path)==1, msg = "problem with 10-year projection path")
  
  
  
  components <- c("population","births","deaths","in_migration","out_migration","net_migration")
  
  scenario_1 <- components %>% 
    lapply(function(x){
      readRDS(paste0(scenario_1_path, "/", x, ".rds")) %>%
        filter(gss_code == borough_gss)
    })
  names(scenario_1) <- components
  
  scenario_2 <- components %>% 
    lapply(function(x){
      readRDS(paste0(scenario_2_path, "/", x, ".rds")) %>%
        filter(gss_code == borough_gss)
    })
  names(scenario_2) <- components
  
  
  scenario_1$primary <- filter(scenario_1$population, age %in% 4:10)
  scenario_2$primary <- filter(scenario_2$population, age %in% 4:10)
  
  scenario_1$secondary <- filter(scenario_1$population, age %in% 11:15)
  scenario_2$secondary <- filter(scenario_2$population, age %in% 11:15)
  
  group_data <- function(x){
    nm <- last(names(x))
    group_by(x, gss_code, la_name, gss_code_ward, ward_name, year) %>% 
      summarise(value := sum(!!sym(nm)), .groups = 'drop_last') %>% 
      data.frame()
  }
  
  scenario_1 <- lapply(scenario_1, group_data)
  scenario_2 <- lapply(scenario_2, group_data)
  
  scenario_1$age_structure <- readRDS(paste0(scenario_1_path, "/", "population", ".rds")) %>% 
    filter(year %in% c(2011,2021,2036)) %>%
    mutate(year = ifelse(year == 2036, "2036 (scenario 1)", as.character(year))) %>% 
    group_by(gss_code, la_name, gss_code_ward, ward_name, year, age) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame()
  
  scenario_2$age_structure <- readRDS(paste0(scenario_2_path, "/", "population", ".rds")) %>% 
    filter(year == 2036) %>%
    mutate(year = "2036 (scenario 2)") %>% 
    group_by(gss_code, la_name, gss_code_ward, ward_name, year, age) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame()
  
  all_data <- list()
  for(i in 1:9){
    all_data[[i]] <- rbind(mutate(scenario_1[[i]], scenario = "scenario 1"),
                           mutate(scenario_2[[i]], scenario = "scenario 2"))
    names(all_data)[i] <- names(scenario_2)[i]
  }
  
  all_data$dev <- readRDS(paste0(scenario_1_path, "/", "dwelling_trajectory", ".rds")) %>% filter(gss_code == borough_gss)
  
  borough_wards <-  all_data$population$gss_code_ward %>% unique()
  
  #-----------------------------------------------------------------------------
  
  message("running RMDs")
  
  for(ward_code in borough_wards){
    ward_data <- lapply(all_data, function(x) filter(x, gss_code_ward == ward_code))
    ward_data$ward_code <- ward_code
    ward_data$ward_name <- ward_data$population$ward_name %>% unique()
    ward_data$borough_name <- camel(str_replace_all(proj_name,"_"," "))
    ward_data$dev_trajectory <- "BPO"
    ward_data <<- ward_data
  
    dir.create("outputs/markdown/2021_bpo/", showWarnings = FALSE)
    
    rmarkdown::render("model_code/markdown/2021_bpo_markdown/2021_bpo_markdown.Rmd",
                      output_file = paste0(ward_code,"_",ward_data$ward_name,".html"),
                      output_dir = paste0("outputs/markdown/2021_bpo/",proj_name))
  }
  rm(ward_data)
}

