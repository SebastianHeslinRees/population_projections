#' Upload a BPO excel file to the datastore
#' 
#' Using the ldndatar package to leverage the datastore api
#'
#' @param bpo_name String. The folder name within the \code{bpo_root} folder that
#'  contains the file to be uploaded
#' @param bpo_root String. The directory path to the bpo outputs folder.
#'   Default \code{outputs/housing_led/2019/bpo/}
#' @param variants Character. The projection variants. Defaults to \code{c("high_migration","medium_migration", "low_migration","medium_migration_trend_fertility")}
#' 
#' @import dplyr
#' @import ldndatar
#' @import stringr
#' 
#' @export

upload_bpo_to_datastore <- function(bpo_name,
                                    variants,
                                    bpo_root = "outputs/housing_led/2019/bpo/"){

  #DO NOT SAVE THIS FILE WITH AN API KEY
  lds_api_key <- Sys.getenv("lds_api_key")
  if(nchar(lds_api_key)==0){stop("LDS API key is of length zero. Expects an .Renviorn file at the directory root containing the lds_api_key variable")}
  
  #today's date
  today <- format(Sys.time(), "%d/%m/%Y")
  bpo_name <- tolower(bpo_name)
  
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
  
  borough_df <- data.frame(short = substr(bpo_name,1,4), stringsAsFactors = FALSE) %>%
    left_join(borough_name_lookup, by="short") 
  
  #validate
  if(nrow(borough_df)==0){stop("problem identifying borough for ", full_name)}
  if(nrow(borough_df)>1){stop("more than 1 borough identified for ", full_name)}
  
  borough <- as.character(borough_df$full)
  
  #slug
  bposlug <- paste0(borough, "-housing-led-projections---borough-preferred-option")
  dataset_resources <- lds_meta_dataset(slug = bposlug, api_key = lds_api_key)
  
  #sort out file paths and validate
  bpo_root <- ifelse(str_sub(bpo_root, -1 ,-1)!="/", paste0(bpo_root,"/"), bpo_root)
  all_bpos <- list.dirs(path = bpo_root, full.names = FALSE, recursive = FALSE)
  all_bpos_no_date <- str_sub(all_bpos,1,-15)
  
  for(i in 1:length(variants)){
    
    #expected file name
    full_name <- paste0(bpo_name, "_", variants[i])
    
    #find folder with expected name
    bpo_matches <- all_bpos[all_bpos_no_date==full_name]
    
    if(length(bpo_matches)==0){stop(paste0("no mataches found for ", full_name))}
    if(length(bpo_matches)>1){stop(paste0("more than 1 match found for ", full_name))}
    
    #find excel file inside folder
    bpo_file <- paste0(bpo_root, bpo_matches, "/", full_name, "_BPO_2019.xlsx" )
    if(!file.exists(bpo_file)){stop(paste0("excel file not found for ", full_name))}
    
    #metadata for datastore
    bpo_name_title <- gsub("_", " ", bpo_name)
    bpo_name_title <- gsub("-", " ", bpo_name_title)
    bpo_name_title <- camel(bpo_name_title)
    bpo_name_title <- gsub("And", "and", bpo_name_title)
    
    resource_var <- str_replace_all(variants[i], "_", " ")
    substr(resource_var, 1, 1) <- toupper(substr(resource_var, 1, 1))
    res_title <- paste0("BPO projection - ", bpo_name_title, " - ", resource_var)
    
    traj <- ifelse(grepl("shlaa", full_name, ignore.case = TRUE), "SHLAA", "Borough-specified")
    resource_desc <- paste0("2019-based BPO projection. ", traj, " development trajectory. ",
                            resource_var, " migration assumptions. ",
                            "Uploaded ",today)                
    
    #Stop the datastore breaking
    Sys.sleep(10)
    
    #check resource doesn't exist and then upload
    if(res_title %in% dataset_resources$resource_title){
      
      message("replacing ",full_name)
      
      resource_id <- filter(dataset_resources, resource_title == res_title)$resource_id
      
      lds_replace_resource(file_path = bpo_file,
                           slug = bposlug,
                           api_key = lds_api_key,
                           res_title = res_title,
                           description = resource_desc,
                           res_id = resource_id)
      
    } else {
      
      message("uploading ",full_name)
      
      lds_add_resource(file_path = bpo_file,
                       slug = bposlug,
                       api_key = lds_api_key,
                       res_title = res_title,
                       description = resource_desc)
    }
    
    rm(bpo_file, full_name, bpo_matches, 
       resource_desc, resource_var, res_title)
    
  }
  
  message(paste(borough, "bpo upload complete"))
}

camel <- function(x){ #function for camel case
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}
