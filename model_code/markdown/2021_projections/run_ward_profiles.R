library(dplyr)

dir.create("outputs/markdown/2021_ward_profiles", showWarnings = FALSE, recursive = TRUE)

ward_profile_data <- readRDS("outputs/markdown/2021_ward_profile_data.rds")
all_wards <- setdiff(names(ward_profile_data), "london")[-1] # not City

#Loop from here
tm <- Sys.time()
for(ward_code in all_wards){
  
  ward_name <- ward_profile_data[[ward_code]]$ward_name
  borough_name <- ward_profile_data[[ward_code]]$borough_name %>% 
    stringr::str_replace_all(" ", "_")
  
  rmarkdown::render("model_code/markdown/2021_projections/ward_profile.rmd",
                    output_file = paste0(ward_code,"_",ward_name,".html"),
                    output_dir = paste0("outputs/markdown/2021_ward_profiles/",borough_name))
}

rmarkdown::render("model_code/markdown/2021_projections/ward_profile_index.rmd",
                  output_file = "ward_profile_index",
                  output_dir = "outputs/markdown/2021_ward_profiles/")

tm <- Sys.time()-tm
