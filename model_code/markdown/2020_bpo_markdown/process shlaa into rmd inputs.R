library(dplyr)

shlaa_rmd_2020 <- function(borough_name, wards){
  
  message(paste(borough_name, "SHLAA RMD"))
  
  gss_code_to_name <- readRDS("input_data/lookup/gss_code_to_name.rds")
  borough_gss <- filter(gss_code_to_name, gss_name == borough_name)$gss_code
  assertthat::assert_that(length(borough_gss)==1, msg = "problem with borough name")
  
  borough_name <- borough_name %>% tolower()
  
  scenario_upper <- lapply(upper_proj, function(x) filter(x, gss_code == borough_gss))
  scenario_lower <- lapply(lower_proj, function(x) filter(x, gss_code == borough_gss))
  
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
    ward_data$borough_name <- paste(toupper(substr(borough_name, 1, 1)), substr(borough_name, 2, nchar(borough_name)), sep="")
    ward_data$dev_trajectory <- "SHLAA"
    ward_data <<- ward_data
    wards <<- wards
    rmarkdown::render("model_code/markdown/2020_bpo_markdown/2020_bpo_markdown.Rmd",
                      output_file = paste0(ward_code,"_",ward_data$ward_name,".html"),
                      output_dir = paste0("outputs/markdown/2020_shlaa/",wards,"/",borough_name))
  }
  
}

#-------------------------------------------------------------------------------

wards <- "WD13"

projections <- list.dirs("outputs/smallarea_model", recursive = FALSE)
projections <- projections[grepl(wards, projections)]

upper_path <- projections[grepl("upper", tolower(projections))]
lower_path <- projections[grepl("lower", tolower(projections))]

upper_proj <- list()
lower_proj <- list()

for(x in c("population","births","deaths","in_migration","out_migration","net_migration")){
  message(paste("reading", x))
  upper_proj[[x]] <- readRDS(paste0(upper_path, "/", x, ".rds")) 
  lower_proj[[x]] <- readRDS(paste0(lower_path, "/", x, ".rds")) 
}

lookup <- readRDS("input_data/smallarea_model/lookups/ward_2013_name_lookup.rds") %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  select(gss_code, la_name) %>%
  distinct()

for(i in 1:nrow(lookup)){
  shlaa_rmd_2020(lookup[i,2], wards)
}

#-------------------------------------------------------------------------------

wards <- "WD22"

projections <- list.dirs("outputs/smallarea_model", recursive = FALSE)
projections <- projections[grepl(wards, projections)]

upper_path <- projections[grepl("upper", tolower(projections))]
lower_path <- projections[grepl("lower", tolower(projections))]

upper_proj <- list()
lower_proj <- list()

for(x in c("population","births","deaths","in_migration","out_migration","net_migration")){
  message(paste("reading", x))
  upper_proj[[x]] <- readRDS(paste0(upper_path, "/", x, ".rds")) 
  lower_proj[[x]] <- readRDS(paste0(lower_path, "/", x, ".rds")) 
}

lookup <- readRDS("input_data/smallarea_model/lookups/ward_2013_name_lookup.rds") %>% 
  filter(substr(gss_code,1,3)=="E09") %>% 
  select(gss_code, la_name) %>%
  distinct()

for(i in 1:nrow(lookup)){
  shlaa_rmd_2020(lookup[i,2], wards)
}


