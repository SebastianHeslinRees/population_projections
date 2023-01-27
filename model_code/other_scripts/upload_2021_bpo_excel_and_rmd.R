library(dplyr)
library(ldndatar)
library(assertthat)

camel <- function(x){ #function for camel case
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}

upload_projection <- function(file_path, lds_title, lds_description, borough){
  
  #confirm file
  assertthat::assert_that(file.exists(file_path),
                          msg= paste0("no file found at specified location:",
                                      file_path))
  
  borough_name <- tolower(borough)

  #DO NOT SAVE THIS FILE WITH AN API KEY
  lds_api_key <- Sys.getenv("lds_api_key")
  if(nchar(lds_api_key)==0){stop("LDS API key is of length zero. Expects an .Renviorn file at the directory root containing the lds_api_key variable")}
  
  #hub names lookup
  borough_name_lookup <- data.frame(full = c("City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
                                             "Camden","Croydon","Ealing",
                                             "Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow",
                                             "Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea", "Kingston",
                                             "Lambeth","Lewisham","Merton","Newham","Redbridge", "Richmond",
                                             "Southwark","Sutton","Tower Hamlets",
                                             "Waltham Forest","Wandsworth","Westminster")) %>%
    mutate(full = tolower(gsub(" ","-",full))) %>%
    mutate(short = substr(full,1,4))
  
  borough_df <- data.frame(short = substr(borough_name,1,4), stringsAsFactors = FALSE) %>%
    left_join(borough_name_lookup, by="short") 
  
  #validate
  if(nrow(borough_df)==0){stop("problem identifying borough for ", full_name)}
  if(nrow(borough_df)>1){stop("more than 1 borough identified for ", full_name)}
  
  #slug
  bposlug <- paste0(as.character(borough_df$full), "-housing-led-projections---borough-preferred-option")
  dataset_resources <- lds_meta_dataset(slug = bposlug, api_key = lds_api_key)
  
  #metadata for datastore
  x <- filter(dataset_resources, resource_title == lds_title)
  
  if(nrow(x) == 0){
    message("uploading ", lds_title)
    
    lds_add_resource(file_path = file_path,
                     slug = bposlug,
                     api_key = lds_api_key,
                     res_title = lds_title,
                     description = lds_description)
  }
  
  else if(nrow(x)==1){
    message("repalcing ", lds_title)
    
    res_id <- x$resource_id
    
    lds_replace_resource(file_path = file_path,
                         slug = bposlug,
                         api_key = lds_api_key,
                         res_title = lds_title,
                         description = lds_description,
                         res_id = res_id)
  }
  
  else {
    stop('more than 1 data set found with same name')
  }
  
  Sys.sleep(10)
  
  
  message(paste(borough, "bpo upload complete"))
}

#---

zip_rmds <- function(folder_path, borough){
  
  rmd_root <- "outputs/markdown/2021_bpo/"
  borough_name <- tolower(borough)
  proj_name <- tolower(proj_name)
  
  all_rmds <- list.dirs(rmd_root)
  bpo_rmd <- all_rmds[grepl(proj_name, all_rmds)]
  
  assert_that(length(bpo_rmd)==1)
  
  wards <- substr(bpo_rmd, nchar(bpo_rmd)-3, nchar(bpo_rmd))
  ward_year <- ifelse(wards == "WD13", "2013", "2022")
  
  proj_dir <- getwd()
  setwd(bpo_rmd)
  html_files <- list.files(pattern = "html", full.names = FALSE)
  
  utils::zip(zipfile = paste0(proj_dir, "/", rmd_root, proj_name, "_", wards),
             files = html_files,
             zip = "c:/rtools40/usr/bin/zip.exe")
  
  setwd(proj_dir)
  
  message("zip file created")
  
}

upload_zip <- function(borough, proj_name = borough){
  
  rmd_root <- "outputs/markdown/2020_bpo/"
  borough_name <- tolower(borough)
  proj_name <- tolower(proj_name)
  
  all_zips <- list.files(rmd_root, "zip", full.names = TRUE)
  bpo_zip <- all_zips[grepl(proj_name, all_zips)]
  
  assert_that(length(bpo_zip)==1)
  
  wards <- substr(bpo_zip, nchar(bpo_zip)-7, nchar(bpo_zip)-4)
  ward_year <- ifelse(wards == "WD13", "2013", "2022")
  
  #DO NOT SAVE THIS FILE WITH AN API KEY
  lds_api_key <- Sys.getenv("lds_api_key")
  if(nchar(lds_api_key)==0){stop("LDS API key is of length zero. Expects an .Renviorn file at the directory root containing the lds_api_key variable")}
  
  #hub names lookup
  borough_name_lookup <- data.frame(full = c("City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
                                             "Camden","Croydon","Ealing",
                                             "Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow",
                                             "Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea", "Kingston",
                                             "Lambeth","Lewisham","Merton","Newham","Redbridge", "Richmond",
                                             "Southwark","Sutton","Tower Hamlets",
                                             "Waltham Forest","Wandsworth","Westminster")) %>%
    mutate(full = tolower(gsub(" ","-",full))) %>%
    mutate(short = substr(full,1,4))
  
  borough_df <- data.frame(short = substr(borough_name,1,4), stringsAsFactors = FALSE) %>%
    left_join(borough_name_lookup, by="short") 
  
  #validate
  if(nrow(borough_df)==0){stop("problem identifying borough for ", full_name)}
  if(nrow(borough_df)>1){stop("more than 1 borough identified for ", full_name)}
  
  #slug
  bposlug <- paste0(as.character(borough_df$full), "-housing-led-projections---borough-preferred-option")
  dataset_resources <- lds_meta_dataset(slug = bposlug, api_key = lds_api_key)
  
  #resource desc and title
  desc <- paste0("2020-based BPO projections Ward profiles (BPO development, ", ward_year," wards)")
  title <- paste0("Ward profiles (BPO development, ", ward_year, " wards)")
  
  #metadata for datastore
  x <- filter(dataset_resources, resource_title == title)
  
  if(nrow(x) == 0){
    message("uploading ", title)
    
    lds_add_resource(file_path = bpo_zip,
                     slug = bposlug,
                     api_key = lds_api_key,
                     res_title = title,
                     description = desc)
  }
  
  else if(nrow(x)==1){
    message("repalcing ", title)
    
    res_id <- x$resource_id
    
    lds_replace_resource(file_path = bpo_zip,
                         slug = bposlug,
                         api_key = lds_api_key,
                         res_title = title,
                         description = desc,
                         res_id = res_id)
  }
  
  else {
    stop('more than 1 data set found with same name')
  }
  
}


# borough <- "newham"
# upload_excels(borough)
# zip_rmds(borough)
# upload_zip(borough)
