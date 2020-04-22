#' Upload a sinngle file to the datastore
#' 
#' Using the ldndatar package to leverage the datastore api
#'
#' The function calculates the difference between a population and a (lower
#' resolution) target, and then returns updated domestic in- and outmigration
#' rates that include the residual.
#'
#' @param bpo_root The directory path to the bpo outputs folder
#' @param bpo_folder The folder name within the \code{bpo_root} that contains the
#' file to be uploaded
#' @param resource_title The title of the resource. This is the text displayed on
#' the datastore page
#' @param resource_desc A description of the resource. This is the text displayed
#' on the datastore page
#' @file_name The full name of the file to be uploaded including the file type suffix
#' @partial_name A part of the full name to be uploaded. Useful when there is only one
#' of a particular type of file in the \code{bpo_folder} - for example an xslx file.
#' In this case this parameter can be used instead of file_name to search for the file
#' constinaing 'xslx'.
#' 
#' @import dplyr
#' @import ldndatar
#' @import Knitr

upload_single_file <- function(bpo_root, bpo_folder, resource_title, resource_desc,
                               file_name = NA, partial_name = NA){
  
  library(dplyr)
  library(ldndatar)
  library(knitr)
  
  if(is.na(file_name) & is.na(partial_name)){stop("must specifiy either file_name or partial_name")}

  if(!is.null(partial_name)){
    file_name <- list.files(paste0(bpo_root,"/",bpo_folder), pattern = partial_name)
  }
  
  
  #DO NOT SAVE THIS FILE WITH AN API KEY
  source("Q:/Teams/D&PA/Demography/Projections/2019_development/notebooks_and_analysis/mats_api_key.txt")
  api_key <- get_api_key()
  
  #hub names
  ds_borough_name_lookup <- data.frame(full = c("City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
                                                "Camden","Croydon","Ealing",
                                                "Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow",
                                                "Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea", "Kingston",
                                                "Lambeth","Lewisham","Merton","Newham","Redbridge",
                                                "Southwark","Sutton","Tower Hamlets",
                                                "Waltham Forest","Wandsworth","Westminster")) %>%
    mutate(full = tolower(gsub(" ","-",full))) %>%
    mutate(short = substr(full,1,4))
  
  borough <- data.frame(short = substr(bpo_folder,1,4), stringsAsFactors = FALSE) %>%
    left_join(ds_borough_name_lookup, by="short") %>%
    select(full) %>%
    as.character()
  
  
  #slug
  bposlug <- paste0(borough, "-housing-led-projections---borough-preferred-option")
  
  #Upload file
  file <- paste0(bpo_root,"/",bpo_folder,"/",file_name)
  
  lds_add_resource(file_path = file,
                   slug = bposlug,
                   api_key = api_key,
                   res_title = resource_title,
                   description = resource_desc)
  
  message(paste(borough, "upload complete"))
  
}