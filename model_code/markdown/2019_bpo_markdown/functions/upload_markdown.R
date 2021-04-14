library(ldndatar)

upload_markdown <- function(bpo_name){
  
  lds_api_key <- Sys.getenv("lds_api_key")
  
  borough <- borough_slug_lookup(bpo_name)

  #slug
  bposlug <- paste0(borough, "-housing-led-projections---borough-preferred-option")
  
  markdown_root <- "outputs/housing_led/2019/bpo/_markdown/_zips/"
  
  bpo_name_title <- gsub("_", " ", bpo_name)
  bpo_name_title <- gsub("shlaa", "SHLAA", bpo_name_title)
  bpo_name_title <- camel(bpo_name_title)
  bpo_name_title <- gsub("And", "and", bpo_name_title)
  
  traj <- ifelse(grepl("shlaa", bpo_name, ignore.case = TRUE), "SHLAA", "BPO")
  res_title <- paste0("Ward and borough profiles (",bpo_name_title,")")
  desc <- paste0("Ward reports on the ", traj," scenario projections")
  
  Sys.sleep(10)
  
  #ZIP
  dataset_resources <- lds_meta_dataset(slug = bposlug, api_key = lds_api_key)
  if(!res_title %in% dataset_resources$resource_title){
    lds_add_resource(file_path = paste0(markdown_root,bpo_name,".zip"),
                     slug = bposlug,
                     api_key = lds_api_key,
                     res_title = res_title,
                     description = desc)
  } else {
    resource_id <- filter(dataset_resources, res_title == resource_title)$resource_id
    
    lds_replace_resource(file_path = paste0(markdown_root,bpo_name,".zip"),
                         slug = bposlug,
                         api_key = lds_api_key,
                         res_title = res_title,
                         description = desc,
                         res_id = resource_id)
  }
  
  return(bposlug)
  
}

borough_slug_lookup <- function(bpo_name){
  
  borough_name_lookup <- data.frame(full = c("City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
                                             "Camden","Croydon","Ealing",
                                             "Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow",
                                             "Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea", "Kingston",
                                             "Lambeth","Lewisham","Merton","Newham","Redbridge", "Richmond",
                                             "Southwark","Sutton","Tower Hamlets",
                                             "Waltham Forest","Wandsworth","Westminster")) %>%
    mutate(full = tolower(gsub(" ","-",full))) %>%
    mutate(short = substr(full,1,4))
  
  bpo_name <- tolower(bpo_name)
  
  borough_df <- data.frame(short = substr(bpo_name,1,4), stringsAsFactors = FALSE) %>%
    left_join(borough_name_lookup, by="short") 
  
  #validate
  if(nrow(borough_df)==0){stop("problem identifying borough for ", full_name)}
  if(nrow(borough_df)>1){stop("more than 1 borough identified for ", full_name)}
  
  borough <- as.character(borough_df$full)
  
  return(borough)
}

camel <- function(x){ #function for camel case
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}
