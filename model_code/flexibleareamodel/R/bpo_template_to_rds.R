#' Process a standard bpo template csv file into 2 rds files
#' 
#' Take a standard ward-level csv bpo template and create 2
#' rds files - 1 ward and 1 borough - where additional data
#' not contained in the template is taken from the SHLAA
#'
#' @param csv_name String. The name of the dwelling trajectory csv saved in the
#'   \code{bpo_dir} folder. With or without the "csv" suffix.
#' @param bpo_dir String. The folder containing the dwelling trajectory csv.
#' @param trajectory_range Numeric. The years covered by the input trajectory.
#' @param wards String indicating the ward geography in the projection: either WD13 or WD22
#'
#' @importFrom assertthat assert_that
#' @importFrom tidyr pivot_longer replace_na
#' @import dplyr
#' @importFrom data.table fread
#' 
#' @export

bpo_template_to_rds <- function(csv_name,
                                bpo_dir,
                                trajectory_range,
                                wards){
  
  if(wards == "WD22"){
    ward_code_lookup <- readRDS("input_data/flexible_area_model/lookups/dummy_WD22_codes.rds") %>%select(gss_code_ward, dummy_code, gss_code)
    ward_shlaa_trajectory <- readRDS("input_data/flexible_area_model/development_data/ward_savills_trajectory_WD22CD.rds")
    ldd_past_development <- readRDS("input_data/flexible_area_model/development_data/ldd_backseries_dwellings_ward_WD22CD.rds")
  }
  
  if(wards == "WD13"){
    ward_code_lookup <- readRDS("input_data/flexible_area_model/lookups/ward_2013_name_lookup.rds") %>% select(gss_code_ward, gss_code)
    ward_shlaa_trajectory <-  readRDS("input_data/flexible_area_model/development_data/ward_savills_trajectory_WD13CD.rds")
    ldd_past_development <- readRDS("input_data/flexible_area_model/development_data/ldd_backseries_dwellings_ward_WD13CD.rds")
  }
  
  
  #file names and paths
  if(!grepl(".csv$", csv_name)){csv_name <- paste0(csv_name,".csv")}
  csv_path <- paste0(bpo_dir, "csvs/", csv_name)
  bpo_name <- substr(csv_name,1,nchar(csv_name)-4)
  
  #Read in data, do a check and find where the data starts and finishes
  tbl <- fread(csv_path, header=FALSE) %>% as.data.frame()
  assert_that(ncol(tbl)==32, msg="number of columns in input housing trajectory is wrong")
  unc_row <- match("Non-self-contained", tbl[[1]])
  no_of_gss_codes <- filter(tbl, substr(V1,1,2)=="E0")[,1] %>% unique() %>% length()
  
  #read the data in again but this time as 2 dataframes
  conventional_in <- fread(csv_path, header=TRUE, skip = "gss_code_ward",  colClasses = rep("character",32))[1:no_of_gss_codes,]
  non_conventional_in <- fread(csv_path, header=TRUE, skip = unc_row+1, colClasses = rep("character",32))[1:no_of_gss_codes,]
  
  conventional <- .process_input_csv(conventional_in, trajectory_range, wards, ward_code_lookup)
  non_conventional <- .process_input_csv(non_conventional_in, trajectory_range, wards, ward_code_lookup)
  
  trajectory <- rbind(conventional, non_conventional) %>% 
    group_by(year, gss_code_ward) %>% 
    summarise(dev = sum(dev), .groups = 'drop_last') %>% 
    data.frame()
  
  #Identify wards and borough codes
  codes_of_interest <- unique(conventional$gss_code_ward)
  borough_gss <- ward_code_lookup %>%
    filter(gss_code_ward %in% codes_of_interest) %>%
    select(gss_code) %>%
    unique() %>%
    as.character()
  assert_that(length(borough_gss) == 1, msg = "more than one borough found in bpo csv")
  assert_that(borough_gss!="character(0)", msg = "no borough returned in bpo csv")
  
  #Fill in with LDD/SHLAA where necessary
  bpo_ward_trajectory <- ward_shlaa_trajectory %>%
    filter(year > 2019) %>% 
    rbind(ldd_past_development) %>% 
    left_join(trajectory, by = c("gss_code_ward","year")) %>%
    mutate(units = ifelse(is.na(dev), units, dev)) %>% 
    select(-dev)
  
  saveRDS(bpo_ward_trajectory, paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds"))

  return(borough_gss)
  
}

#function to reformat template data
.process_input_csv <- function(x, trajectory_range, wards, ward_code_lookup=NULL){
  
  a <- pivot_longer(x,
               cols = 3:32,
               names_to = "year_char",
               values_to = "dev") %>%
    data.frame() %>% 
    mutate(year = as.numeric(year_char),
           dev = as.numeric(dev)) 
  
  if(wards == "WD22"){
  a <- a %>% 
    rename(dummy_code = gss_code_ward) %>% 
    left_join(ward_code_lookup, by = "dummy_code")
  }
  
  a <- a %>% 
    select(year, gss_code_ward, dev) %>%
    filter(year %in% trajectory_range) %>%
    as.data.frame() %>%
    tidyr::replace_na(list(dev = 0))%>% 
    data.frame() 
  
  return(a)
  
}
