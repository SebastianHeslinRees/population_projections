run_bpo_markdown <- function(projection_name, shlaa=FALSE){
  
  # project_root <- 'model_code/markdown/2019_bpo_markdown'
  # g <- getwd()
  # setwd(project_root)
  
  message("creating bpo htmls")
  
  #bespoke functions for this process
  source('model_code/markdown/2019_bpo_markdown/functions/get_bpo_data.R')
  source('model_code/markdown/2019_bpo_markdown/functions/prep_for_rmd.R')
  source('model_code/markdown/2019_bpo_markdown/functions/bpo_charts.R')
  
  #using the projection name find the relevant projections
  bpo_dir <- "outputs/housing_led/2019/bpo/"
  if(shlaa == FALSE){
    scenario_1 <- list.dirs(bpo_dir, recursive=FALSE)[grepl(paste0(projection_name, "_scenario_1"), list.dirs(bpo_dir, recursive=FALSE))]
    scenario_2 <- list.dirs(bpo_dir, recursive=FALSE)[grepl(paste0(projection_name, "_scenario_2"), list.dirs(bpo_dir, recursive=FALSE))]
    scenario_3 <- list.dirs(bpo_dir, recursive=FALSE)[grepl(paste0(projection_name, "_scenario_3"), list.dirs(bpo_dir, recursive=FALSE))]
  } else {
    scenario_1 <- "outputs/housing_led/2019/BPO_scenario_1_21-04-09_1536"
    scenario_2 <- "outputs/housing_led/2019/BPO_scenario_2_21-04-09_1536"
    scenario_3 <- "outputs/housing_led/2019/BPO_scenario_3_21-04-09_1536"
  }
  #put them in a list
  x <- list('scenario 1' = scenario_1,
            'scenario 2' = scenario_2,
            'scenario 3' = scenario_3)
  
  #get borough gss code
  gss_borough <- readRDS("input_data/lookup/gss_code_to_name.rds") %>% 
    filter(substr(gss_code,1,3) == "E09") %>% 
    filter(substr(toupper(gss_name),1,4)==substr(toupper(projection_name),1,4)) %>% 
    .$gss_code %>% 
    unique()
  
  #read in the data
  message("reading bpo data")
  bpo_data <- get_bpo_data(x, gss_borough)
  
  #make a list of wards in the borough
  message("running markdown")
  ward_list <- bpo_data[['population']]$gss_code %>% unique()
  
  #Get ready to write rmd
  project_root <- getwd()
  rmd_root <- paste0(project_root,"/outputs/housing_led/2019/bpo/_markdown/")
  output_dir <- paste0(rmd_root, projection_name, "/")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  data_for_rmd <- list()
  
  for(w in ward_list){
    area_name <- filter(bpo_data[[3]], gss_code == w)$ward_name %>% unique()
    borough_name <- filter(bpo_data[[3]], substr(gss_code,1,3) == "E09")$ward_name %>% unique()
    
    #make all the charts and tables
    x <- prep_data_for_rmd(bpo_data, w, area_name)
    x$borough_name <- borough_name
    
    #write out the markdown
    file_name <- paste0(output_dir,w,"_",area_name)
    if(area_name == borough_name){file_name <- paste0(file_name,"_TOTAL")}
    
    rmarkdown::render("model_code/markdown/2019_bpo_markdown/functions/2019_bpo_output.rmd", 
                      params = list(x = x),
                      output_file = file_name)
  }
  
  #Zip files
  message("zipping files")
  setwd(output_dir)
  html_files <- list.files(pattern = "html", full.names = FALSE)
  utils::zip(zipfile = paste0(rmd_root, "_zips/", projection_name),
             files = html_files,
             zip = "c:/rtools40/usr/bin/zip.exe")
  setwd(project_root)
  
  rm(list=setdiff(ls(),"create_htmls"))
}

