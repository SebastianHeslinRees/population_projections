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
#' 
#' @export

bpo_template_to_rds <- function(csv_name,
                                bpo_dir,
                                trajectory_range){

  #file names and paths
  if(!grepl(".csv$", csv_name)){csv_name <- paste0(csv_name,".csv")}
  csv_path <- paste0(bpo_dir,"csvs/",csv_name)
  bpo_name <- substr(csv_name,1,nchar(csv_name)-4)

  #Read in data, do a check and find where the data starts and finishes
  tbl <- data.table::fread(csv_path, header=FALSE) %>% as.data.frame()
  assert_that(ncol(tbl)==32, msg="number of columns in input housing trajectory is wrong")
  unc_row <- match("Non-Conventional", tbl[[1]])
  no_of_gss_codes <- filter(tbl, substr(V1,1,3)=="E05")[,1] %>% unique() %>% length()
  
  #read the data in again but this time as 2 dataframes
  conventional_in <- data.table::fread(csv_path, header=TRUE, skip = "GSS.Code.Ward",  colClasses = rep("character",32))[1:no_of_gss_codes,]
  non_conventional_in <- data.table::fread(csv_path, header=TRUE, skip = unc_row, colClasses = rep("character",32))[1:no_of_gss_codes,]
  
  #Identify wards and borough codes
  codes_of_interest <- unique(conventional_in$GSS.Code.Ward)
  borough_gss <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>%
    filter(gss_code_ward %in% codes_of_interest) %>%
    select(gss_code) %>%
    unique() %>%
    as.character()
  assert_that(length(borough_gss) == 1)
  
  #read in standard shlaa trajectories
  ward_shlaa_trajectory <- readRDS("input_data/small_area_model/ward_shlaa_trajectory_2020.rds")
  borough_shlaa_trajectory <- readRDS("input_data/housing_led_model/borough_shlaa_trajectory_2020.rds")
  
  rm(tbl, unc_row, no_of_gss_codes)
  
  #function to reformat template data
  process_input_csv <- function(x){
    
    pivot_longer(x,
                 cols = 3:32,
                 names_to = "year_long",
                 values_to = "dev") %>%
      mutate(year = as.numeric(substr(year_long,6,9)),
             dev = as.numeric(dev)) %>%
      rename(gss_code_ward = GSS.Code.Ward) %>%
      select(year, gss_code_ward, dev) %>%
      filter(year %in% trajectory_range) %>%
      as.data.frame() %>%
      tidyr::replace_na(list(dev = 0))
      
  }
  
  conventional <- process_input_csv(conventional_in)
  
  non_conventional <- process_input_csv(non_conventional_in) %>%
    rbind(conventional) %>%
    mutate(gss_code = borough_gss) %>%
    group_by(year, gss_code) %>%
    summarise(dev = sum(dev)) %>%
    as.data.frame()

  #Ward trajectory
  bpo_ward_trajectory <- ward_shlaa_trajectory %>%
    left_join(conventional, by = c("gss_code_ward","year")) %>%
    mutate(units = ifelse(is.na(dev), units, dev)) %>% 
    select(-dev)
  
  #Borough trajectory
  bpo_borough_trajectory <- borough_shlaa_trajectory %>%
    left_join(non_conventional, by = c("gss_code","year")) %>%
    mutate(units = ifelse(is.na(dev), units, dev)) %>% 
    select(-dev)
  
  assert_that(nrow(bpo_ward_trajectory)==nrow(ward_shlaa_trajectory))
  assert_that(nrow(bpo_borough_trajectory)==nrow(borough_shlaa_trajectory))
  
  saveRDS(bpo_ward_trajectory, paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds"))
  saveRDS(bpo_borough_trajectory, paste0(bpo_dir,"rds/bpo_borough_trajectory_",bpo_name,".rds"))
  
  return(borough_gss)
  
}
