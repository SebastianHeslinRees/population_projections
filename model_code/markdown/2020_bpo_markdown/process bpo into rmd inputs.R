library(dplyr)
library(stringr)

camel <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}

bpo_rmd_2020 <- function(borough_name, wards, proj_name = borough_name){
  
  message(paste(borough_name, "BPO RMD"))
  
  gss_code_to_name <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% filter(substr(gss_code,1,3)=="E09")
  #borough_gss <- filter(gss_code_to_name, gss_name == borough_name)$gss_code
  borough_gss <- filter(gss_code_to_name, str_detect(gss_name, borough_name))$gss_code
  assertthat::assert_that(length(borough_gss)==1, msg = "problem with borough name")
  
  proj_name <- proj_name %>% tolower()
  
  projections <- list.dirs("outputs/flexible_area_model/bpo", recursive = FALSE)

  projections <- projections[grepl(proj_name, projections)]

  projections <- projections[grepl(wards, projections)]
  
  upper_path <- projections[grepl("scenario1", tolower(projections))]
  lower_path <- projections[grepl("scenario2", tolower(projections))]
  
  assertthat::assert_that(length(upper_path)==1, msg = "problem with upper path")
  assertthat::assert_that(length(lower_path)==1, msg = "problem with lower path")
  
  scenario_upper <- list()
  scenario_lower <- list()
  
  for(x in c("population","births","deaths","in_migration","out_migration","net_migration")){
    message(paste("reading", x))
    scenario_upper[[x]] <- readRDS(paste0(upper_path, "/", x, ".rds")) %>% filter(gss_code == borough_gss)
    scenario_lower[[x]] <- readRDS(paste0(lower_path, "/", x, ".rds")) %>% filter(gss_code == borough_gss)
  }
  
  scenario_upper$primary <- filter(scenario_upper$population, age %in% 4:10)
  scenario_lower$primary <- filter(scenario_lower$population, age %in% 4:10)
  
  scenario_upper$secondary <- filter(scenario_upper$population, age %in% 11:15)
  scenario_lower$secondary <- filter(scenario_lower$population, age %in% 11:15)
  
  group_data <- function(x){
    nm <- last(names(x))
    group_by(x, gss_code, la_name, gss_code_ward, ward_name, year) %>% 
      summarise(value := sum(!!sym(nm)), .groups = 'drop_last') %>% 
      data.frame()
  }
  
  scenario_upper <- lapply(scenario_upper, group_data)
  scenario_lower <- lapply(scenario_lower, group_data)
  
  scenario_upper$age_structure <- readRDS(paste0(upper_path, "/", "population", ".rds")) %>% 
    filter(year %in% c(2011,2020,2035)) %>%
    mutate(year = ifelse(year == 2035, "2035 (scenario 2)", as.character(year))) %>% 
    group_by(gss_code, la_name, gss_code_ward, ward_name, year, age) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame()
  
  scenario_lower$age_structure <- readRDS(paste0(lower_path, "/", "population", ".rds")) %>% 
    filter(year == 2035) %>%
    mutate(year = "2035 (scenario 1 )") %>% 
    group_by(gss_code, la_name, gss_code_ward, ward_name, year, age) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame()
  
  all_data <- list()
  for(i in 1:9){
    all_data[[i]] <- rbind(mutate(scenario_upper[[i]], scenario = "scenario 2"),
                           mutate(scenario_lower[[i]], scenario = "scenario 1"))
    names(all_data)[i] <- names(scenario_lower)[i]
  }
  
  all_data$dev <- readRDS(paste0(upper_path, "/", "dwelling_trajectory", ".rds")) %>% filter(gss_code == borough_gss)
  
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
    wards <<- wards
    rmarkdown::render("model_code/markdown/2020_bpo_markdown/2020_bpo_markdown.Rmd",
                      output_file = paste0(ward_code,"_",ward_data$ward_name,".html"),
                      output_dir = paste0("outputs/markdown/2020_bpo/","/",proj_name,"_",wards))
  }
  rm(ward_data)
}

# 
# bpo_rmd_2020("Greenwich", "WD13", "greenwich_scenario_1")
# bpo_rmd_2020("Greenwich", "WD13", "greenwich_scenario_3")
# 
# bpo_rmd_2020("Barnet", "WD13")
# bpo_rmd_2020("Bromley", "WD13")
# bpo_rmd_2020("Haringey", "WD13")
# bpo_rmd_2020("Kingston", "WD13")
# bpo_rmd_2020("Lambeth", "WD13")
# bpo_rmd_2020("Merton", "WD13")
# bpo_rmd_2020("Wandsworth", "WD13")
# bpo_rmd_2020("Hounslow", "WD13")
# 
# bpo_rmd_2020("Barking and Dagenham", "WD22", "barking_and_dagenham")
# bpo_rmd_2020("Brent", "WD22")
# bpo_rmd_2020("Harrow", "WD22")
# bpo_rmd_2020("Tower Hamlets", "WD22", "tower_hamlets")
# bpo_rmd_2020("Westminster", "WD22")
# bpo_rmd_2020("Redbridge", "WD22")
# bpo_rmd_2020("Newham", "WD22")
