library(dplyr)

create_bpo_trajectory <- function(bpo_name,
                                  csv_name = bpo_name,
                                  shlaa_first_year = 2042){
  
  #file names and paths
  if(!grepl(".csv$", csv_name)){csv_name <- paste0(csv_name,".csv")}
  bpo_dir <- "Q:/Teams/D&PA/Demography/Projections/bpo_2018_based/"
  csv_path <- paste0(bpo_dir,"csvs/",csv_name)
  
  #Read in conventional & non-conventional as separate dataframes
  tbl <- data.table::fread(csv_path, header=FALSE) %>% as.data.frame()
  unc_row <- match("Non-Conventional", tbl[[1]])
  no_of_gss_codes <- filter(tbl, substr(V1,1,3)=="E05")[,1] %>% unique() %>% length()
  conventional_in <- data.table::fread(csv_path, header=TRUE, skip = "GSS.Code.Ward")[1:no_of_gss_codes,]
  non_conventional_in <- data.table::fread(csv_path, header=TRUE, skip = unc_row)[1:no_of_gss_codes,]
  
  #Idntify wards and borough codes
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
  doo <- function(x){
    
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
  
  conventional <- doo(conventional_in)
  non_conventional <- doo(non_conventional_in) %>%
    rbind(conventional) %>%
    mutate(gss_code = borough_gss) %>%
    group_by(year, gss_code) %>%
    summarise(units = sum(units)) %>%
    as.data.frame()
  
  
  #Ward trajectory
  additional_shlaa_ward <- ward_shlaa_trajectory  %>%
    filter(year >= shlaa_first_year,
           gss_code_ward %in% codes_of_interest)
  
  conventional <- conventional %>%
    rbind(additional_shlaa_ward)
  
  bpo_ward_trajectory <- ward_shlaa_trajectory %>%
    filter(!gss_code_ward %in% codes_of_interest) %>%
    rbind(conventional)
  
  
  #Borough trajectory
  additional_shlaa_borough <- borough_shlaa_trajectory  %>%
    filter(year >= shlaa_first_year,
           gss_code == borough_gss)
  
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

