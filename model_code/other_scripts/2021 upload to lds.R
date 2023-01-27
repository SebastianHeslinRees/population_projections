#Upload BPO Excels
library(stringr)
library(dplyr)
library(ldndatar)

source("model_code/other_scripts/upload_2021_bpo_excel_and_rmd.R")
lookup <- readRDS("input_data/lookup/gss_code_to_name_(2021_geog).rds")

bpos <- list.files("config_scripts/flexible_area_model/2021_based/bpo/",full.names = FALSE)
bpos <- str_remove_all(bpos, pattern = ".R")

for(borough in bpos){
  
  for(i in c("5","10")){
    
    nm <- str_replace_all(borough, pattern = "_", " ")
    
    lds_title <- paste0(nm, " - BPO - ", i, "-year migration")
    file_path <- paste0("outputs/flexible_area_model/2021_based/bpo/",
                        borough, "_", i,"-year_constrained/",
                        borough, "_", i,"-year_constrained.xlsx")
    
    lds_description <- paste0("2021-based BPO population projection for ",
                              nm,
                              ". Borough development scenario. ",
                              i, "-year migration trend. NUTS2 Constrained.")
    
    
    upload_projection(file_path, lds_title, lds_description, borough)
    
  }
  
  
}

#RMD BPO Profiles
source("model_code/markdown/2021_bpo_markdown/process bpo into rmd.R")
bpos <- str_replace_all(bpos, pattern = "_", " ")
for(borough in bpos){
  bpo_rmd_2021(borough)
}
