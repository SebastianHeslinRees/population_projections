# TODO move this to an @importFrom call once this is in a package
library(xlsx)

#' Create the BPO excel output
#' 
#' Process population, components and developmnet data and place it into an
#' Excel template.
#'
#' @param data The \code{[['csv']]} element from a ward projection
#' @param output_dir The directory in which to save the Excel file
#' @param projection_name The name of the output Excel file
#' @param bpo_gss_code The gss code of the bpo projection
#' @param file_suffix A string to append to the end of the filename

output_bpo_excel_file <- function(data, output_dir, projection_name, bpo_gss_code,
                                  file_suffix = "_BPO_2018.xlsx"){
  
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
 
  ward_dev_dataframe <- data.table::fread(paste0(output_dir,"ward/assumed_dev_ward.csv"),
                                          header = TRUE) %>%
    as.data.frame() %>% 
    filter(gss_code == bpo_gss_code)
  
  assumed_dev_dataframe <- data.table::fread(paste0(output_dir,"csv/assumed_dev.csv"),
                                             header = TRUE) %>%
    as.data.frame() %>% 
    filter(gss_code == bpo_gss_code) %>%
    tidyr::pivot_longer(cols=as.character(2012:2050), values_to = "units", names_to = "year") %>%
    mutate(gss_code_ward = gss_code,
           ward_name = paste0(borough, " (total)")) %>%
    select(gss_code, borough, gss_code_ward, ward_name, year, units) %>%
    arrange(year)  %>%
    mutate(units = round(units,2)) %>%
    tidyr::pivot_wider(names_from = year, values_from = units) %>%
    rbind(ward_dev_dataframe) %>%
    as.data.frame()
  
  wb <- xlsx::loadWorkbook("input_data/housing_led_model/ward_housing_led_2018_based_template.xlsx")
  wb_sheets<- xlsx::getSheets(wb)
  
  xlsx::addDataFrame(bpo_data(persons), wb_sheets$Persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(bpo_data(males), wb_sheets$Males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(bpo_data(females), wb_sheets$Females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(bpo_data(components, col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name","year")),
                     wb_sheets$Components, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(assumed_dev_dataframe, wb_sheets$Assumed, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #dev data source 
  dev_source_text <- as.data.frame(paste0("4. These projections incorporate assumptions about future development provided by the London Borough of ",
                                          unique(assumed_dev_dataframe$borough)))
  date_of_projection <- as.data.frame(paste0("Projection run on ",format(Sys.time(), "%d/%m/%Y")))
  
  xlsx::addDataFrame(dev_source_text, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 11, startColumn = 1)
  xlsx::addDataFrame(date_of_projection, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #Write xlsx file
  wb_filename <- paste0(output_dir, projection_name, file_suffix)
  xlsx::saveWorkbook(wb, wb_filename)
  
}

