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
#'
#' @importFrom assertthat assert_that
#' @importFrom tidyr pivot_longer replace_na
#' @import dplyr
#' @importFrom data.table fread
#' 
#' @export

bpo_template_to_rds <- function(csv_name,
                                bpo_dir,
                                trajectory_range){
  
  ward_code_lookup <- readRDS("input_data/new_ward_model/lookups/dummy_WD22_codes.rds") %>% 
    select(gss_code_ward, dummy_code, gss_code)
  
  #file names and paths
  if(!grepl(".csv$", csv_name)){csv_name <- paste0(csv_name,".csv")}
  csv_path <- paste0(bpo_dir, csv_name)
  bpo_name <- substr(csv_name,1,nchar(csv_name)-4)
  
  #Read in data, do a check and find where the data starts and finishes
  tbl <- fread(csv_path, header=FALSE) %>% as.data.frame()
  assert_that(ncol(tbl)==32, msg="number of columns in input housing trajectory is wrong")
  unc_row <- match("Non-self-contained", tbl[[1]])
  no_of_gss_codes <- filter(tbl, substr(V1,1,3)=="E09")[,1] %>% unique() %>% length()
  
  #read the data in again but this time as 2 dataframes
  conventional_in <- fread(csv_path, header=TRUE, skip = "gss_code_ward",  colClasses = rep("character",32))[1:no_of_gss_codes,]
  non_conventional_in <- fread(csv_path, header=TRUE, skip = unc_row+1, colClasses = rep("character",32))[1:no_of_gss_codes,]
  
  #rm(tbl, unc_row, no_of_gss_codes)
  
  conventional <- .process_input_csv(conventional_in)
  non_conventional <- .process_input_csv(non_conventional_in)
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
  
  #read in standard shlaa trajectories
  ward_shlaa_trajectory <-  readRDS("input_data/new_ward_model/development_data/ward_shlaa_trajectory_WD22CD.rds")
  borough_shlaa_trajectory <- readRDS("input_data/new_ward_model/development_data/borough_shlaa_trajectory.rds")
  
  #Ward trajectory
  bpo_ward_trajectory <- ward_shlaa_trajectory %>%
    left_join(conventional, by = c("gss_code_ward","year")) %>%
    mutate(units = ifelse(is.na(dev), units, dev)) %>% 
    select(-dev)
  
  #Borough trajectory
  bpo_borough_trajectory <- bpo_ward_trajectory %>%
    left_join(ward_code_lookup, by = "gss_code_ward") %>%
    group_by(year, gss_code) %>% 
    summarise(units = sum(units), .groups = 'drop_last') %>% 
    data.frame()
  
  assert_that(nrow(bpo_ward_trajectory)==680*39)
  assert_that(nrow(bpo_borough_trajectory)==33*39)
  
  saveRDS(bpo_ward_trajectory, paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds"))
  saveRDS(bpo_borough_trajectory, paste0(bpo_dir,"rds/bpo_borough_trajectory_",bpo_name,".rds"))
  
  return(borough_gss)
  
}

#function to reformat template data
.process_input_csv <- function(x){
  
  pivot_longer(x,
                    cols = 3:32,
                    names_to = "year_char",
                    values_to = "dev") %>%
    data.frame() %>% 
    mutate(year = as.numeric(year_char),
           dev = as.numeric(dev)) %>%
    
    rename(dummy_code = gss_code_ward) %>% 
    left_join(ward_code_lookup, by = "dummy_code") %>% 
    select(year, gss_code_ward, dev) %>%
    filter(year %in% trajectory_range) %>%
    as.data.frame() %>%
    tidyr::replace_na(list(dev = 0))%>% 
    data.frame() 
  
}
