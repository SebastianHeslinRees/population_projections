#' Process a standard bpo template csv file into 2 rds files
#' 
#' Take a standard ward-level csv bpo template and create 2
#' rds files - 1 ward and 1 borough - where additional data
#' not contained in the template is taken from the SHLAA
#'
#' @param data The \code{[['csv']]} element from a ward projection
#' @param output_dir The directory in which to save the Excel file
#' @param projection_name The name of the output Excel file
#' @param small_area_dev_trajectory_path The path to the ward-level development
#'   trajectory in rds format
#' @param borough_dev_trajectory_path  The path to the borough-level development
#'   trajectory in rds format
#' @param bpo_gss_code The gss code of the bpo projection
#' 
#' @import xlsx
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' 
#' @export

output_bpo_excel_file <- function(data, output_dir, projection_name,
                                  small_area_dev_trajectory_path,
                                  borough_dev_trajectory_path,
                                  bpo_gss_code){
  
  bpo_data <- function(x, col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name", "sex", "age")){
    
    x %>%
      as.data.frame() %>%
      dtplyr::lazy_dt() %>%
      filter(gss_code == bpo_gss_code) %>%
      mutate(gss_code_ward = gss_code,
             ward_name = paste0(borough, " (total)")) %>%
      group_by_at(col_aggregation) %>%
      summarise_all(.funs=sum) %>%
      as.data.frame() %>%
      rbind(x) %>%
      dtplyr::lazy_dt() %>%
      filter(gss_code == bpo_gss_code) %>%
      as.data.frame()
  }
  
  persons <- data[["persons"]]
  females <- data[["females"]]
  males <- data[["males"]]
  components <- data[["components"]]
  
  ldd_backseries_dwellings_borough <- readRDS("input_data/housing_led_model/ldd_backseries_dwellings_borough.rds") %>%
    filter(year %in% 2012:2018)
  ldd_backseries_dwellings_ward <- readRDS("input_data/small_area_model/ldd_backseries_dwellings_ward.rds") %>%
    filter(year %in% 2012:2018)
  
  ward_dev_dataframe <- readRDS(small_area_dev_trajectory_path) %>%
    filter(year > 2018) %>%
    rbind(ldd_backseries_dwellings_ward) %>%
    left_join(readRDS("input_data/lookup/2011_ward_to_district.rds"), by="gss_code_ward") %>%
    filter(gss_code == bpo_gss_code) %>%
    left_join(data.table::fread("input_data/lookup/lad18_code_to_name.csv"), by="gss_code") %>%
    select(gss_code, borough=gss_name, gss_code_ward, ward_name, year, units) %>%
    arrange(year) %>%
    mutate(units = round(units,2)) %>%
    pivot_wider(names_from = year, values_from = units)
  
  assumed_dev_dataframe <- readRDS(borough_dev_trajectory_path) %>%
    filter(year > 2018) %>%
    rbind(ldd_backseries_dwellings_borough) %>%
    filter(gss_code == bpo_gss_code) %>%
    mutate(borough = unique(ward_dev_dataframe$borough),
           gss_code_ward = gss_code,
           ward_name = paste0(borough, " (total)")) %>%
    select(gss_code, borough, gss_code_ward, ward_name, year, units) %>%
    arrange(year)  %>%
    mutate(units = round(units,2)) %>%
    pivot_wider(names_from = year, values_from = units) %>%
    rbind(ward_dev_dataframe) %>%
    as.data.frame()
  
  wb <- loadWorkbook("input_data/housing_led_model/ward_housing_led_2018_based_template.xlsx")
  wb_sheets<- getSheets(wb)
  
  addDataFrame(bpo_data(persons), wb_sheets$Persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(bpo_data(males), wb_sheets$Males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(bpo_data(females), wb_sheets$Females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(bpo_data(components, col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name","year")),
                     wb_sheets$Components, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(assumed_dev_dataframe, wb_sheets$Assumed, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #dev data source 
  dev_source_text <- as.data.frame(paste0("4. These projections incorporate assumptions about future development provided by the London Borough of ",
                                          unique(assumed_dev_dataframe$borough)))
  date_of_projection <- as.data.frame(paste0("Projection run on ",format(Sys.time(), "%d/%m/%Y")))
  
  addDataFrame(dev_source_text, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 11, startColumn = 1)
  addDataFrame(date_of_projection, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #Write xlsx file
  wb_filename <- paste0(output_dir, projection_name,"_BPO.xlsx")
  saveWorkbook(wb, wb_filename)
  
}

