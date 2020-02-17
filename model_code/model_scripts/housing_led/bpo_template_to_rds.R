#' Process a standard bpo template csv file into 2 rds files
#' 
#' Take a standard ward-level csv bpo template and create 2
#' rds files - 1 ward and 1 borough - where additional data
#' not contained in the template is taken from the SHLAA
#'
#' @param csv_name The name of the dwelling trajectory csv saved in the folder
#'   \code{bpo_dir} folder.
#' @param bpo_dir The folder containing the dwelling trajectory csv.
#' @param shlaa_first_year The first year in ehich to use shlaa development data.
#'   Effectively the final year of the supplied trajectory plus 1. \code{Default 2042}.  

bpo_template_to_rds <- function(csv_name,
                                bpo_dir,
                                shlaa_first_year = 2042,
                                dev_first_year = 2011){
  
  #file names and paths
  if(!grepl(".csv$", csv_name)){csv_name <- paste0(csv_name,".csv")}
  csv_path <- paste0(bpo_dir,"csvs/",csv_name)
  bpo_name <- substr(csv_name,1,nchar(csv_name)-4)
  
  #Read in conventional & non-conventional as separate dataframes
  tbl <- data.table::fread(csv_path, header=FALSE) %>% as.data.frame()
  assertthat::assert_that(ncol(tbl)==32, msg="number of columns in input housing trajectory is wrong")
  
  unc_row <- match("Non-Conventional", tbl[[1]])
  no_of_gss_codes <- filter(tbl, substr(V1,1,3)=="E05")[,1] %>% unique() %>% length()
  
  conventional_in <- data.table::fread(csv_path, header=TRUE, skip = "GSS.Code.Ward",  colClasses = rep("character",32))[1:no_of_gss_codes,]
  non_conventional_in <- data.table::fread(csv_path, header=TRUE, skip = unc_row, colClasses = rep("character",32))[1:no_of_gss_codes,]
  
  #Identify wards and borough codes
  codes_of_interest <- unique(conventional_in$GSS.Code.Ward)
  borough_gss <- readRDS("input_data/lookup/2011_ward_to_district.rds") %>%
    filter(gss_code_ward %in% codes_of_interest) %>%
    select(gss_code) %>%
    unique() %>%
    as.character()
  assertthat::assert_that(length(borough_gss) == 1)
  
  #read in stadard shlaa trajectories
  ward_shlaa_trajectory <- readRDS("input_data/small_area_model/ward_shlaa_trajectory.rds")
  borough_shlaa_trajectory <- readRDS("input_data/housing_led_model/borough_shlaa_trajectory.rds")
  
  rm(tbl, unc_row, no_of_gss_codes)
  
  #function to reformat template data
  process_input_csv <- function(x){
    
    x <- tidyr::pivot_longer(x,
                             cols = 3:32,
                             names_to = "year_long",
                             values_to = "dev") %>%
      mutate(year = as.numeric(substr(year_long,6,9)),
             units = as.numeric(dev)) %>%
      select(year,
             gss_code_ward = GSS.Code.Ward,
             units) %>%
      filter(year < shlaa_first_year) %>%
      as.data.frame() %>%
      tidyr::replace_na(list(units = 0))
  }
  
  conventional <- process_input_csv(conventional_in)
  
  non_conventional <- process_input_csv(non_conventional_in) %>%
    rbind(conventional) %>%
    mutate(gss_code = borough_gss) %>%
    group_by(year, gss_code) %>%
    summarise(units = sum(units)) %>%
    as.data.frame()
  
  
  #Ward trajectory
  additional_shlaa_ward <- ward_shlaa_trajectory  %>%
    filter(year >= shlaa_first_year,
           gss_code_ward %in% codes_of_interest)
  
  #years upto and inc 2018 will be replaced by LDD in the model
  #If there is a gap between the start of the bpo trajectory and
  #the end of the LDD (2018) this needs to be filled with SHLAA data

  if(dev_first_year > 2019){
    additional_shlaa_ward <- ward_shlaa_trajectory  %>%
      filter(year < dev_first_year,
             gss_code_ward %in% codes_of_interest) %>%
      rbind(additional_shlaa_ward)
    
    conventional <- filter(conventional, year >= dev_first_year)
  }
  
  conventional <- conventional %>%
    rbind(additional_shlaa_ward)
  
  bpo_ward_trajectory <- ward_shlaa_trajectory %>%
    filter(!gss_code_ward %in% codes_of_interest) %>%
    rbind(conventional)
  
  
  #Borough trajectory
  additional_shlaa_borough <- borough_shlaa_trajectory  %>%
    filter(year >= shlaa_first_year,
           gss_code == borough_gss)
  
  if(dev_first_year > 2019){
    additional_shlaa_borough <- borough_shlaa_trajectory  %>%
      filter(year < dev_first_year,
             gss_code == borough_gss) %>%
      rbind(additional_shlaa_borough)
    
    non_conventional <- filter(non_conventional, year >= dev_first_year)
  }
  
  non_conventional <- non_conventional %>%
    rbind(additional_shlaa_borough)
  
  bpo_borough_trajectory <- borough_shlaa_trajectory %>%
    filter(!gss_code == borough_gss) %>%
    rbind(non_conventional)
  
  assertthat::assert_that(nrow(bpo_ward_trajectory)==nrow(ward_shlaa_trajectory))
  assertthat::assert_that(nrow(bpo_borough_trajectory)==nrow(borough_shlaa_trajectory))
  
  saveRDS(bpo_ward_trajectory, paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds"))
  saveRDS(bpo_borough_trajectory, paste0(bpo_dir,"rds/bpo_borough_trajectory_",bpo_name,".rds"))
  
  return(borough_gss)
  
}

