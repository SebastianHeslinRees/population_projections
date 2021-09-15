#' Create the 2020-based housing-led projection excel output
#' 
#' Process population, components and development data and place it into an
#' Excel template. London borough data.
#'
#' @param output_dir The directory from which to read the data and save the Excel file
#' @param projection_name The name of the output Excel file
#' @param file_suffix A string to append to the end of the file name
#' 
#' @import xlsx
#' @import dplyr
#' @importFrom data.table fread
#' 
#' @export

output_borough_excels <- function(output_dir, projection_name, file_suffix = ".xlsx"){
  
  persons <- fread(paste0(output_dir,"csv/persons.csv"), header = TRUE) %>% as.data.frame() 
  females <- fread(paste0(output_dir,"csv/females.csv"), header = TRUE) %>% as.data.frame() 
  males <- fread(paste0(output_dir,"csv/males.csv"), header = TRUE) %>% as.data.frame() 
  
  components <- fread(paste0(output_dir,"csv/components.csv"), header = TRUE) %>% 
    as.data.frame() %>% 
    rename(population = popn)
  
  names(components) <- stringr::str_replace_all(names(components), "_", " ")
  
  assumed_dev_dataframe <- fread(paste0(output_dir,"csv/assumed_dev.csv"), header = TRUE)
  stock_dataframe <- fread(paste0(output_dir,"csv/housing_stock.csv"), header = TRUE)
  
  #-----------------------------------------------------------------------------
  
  wb <- xlsx::loadWorkbook("input_data/excel_templates/housing_led_2020_based_template.xlsx")
  wb_sheets<- xlsx::getSheets(wb)
  cs <- CellStyle(wb) + Font(wb, isBold=TRUE)
  
  addDataFrame(persons, wb_sheets$persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(males, wb_sheets$males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(females, wb_sheets$females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(components, wb_sheets$components, col.names = TRUE, row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs)
  addDataFrame(assumed_dev_dataframe, wb_sheets$`assumed development`, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(stock_dataframe, wb_sheets$`housing stock`, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #-----------------------------------------------------------------------------
  
  #Write borough file
  if(substr(file_suffix, nchar(file_suffix)-4, nchar(file_suffix)) != ".xlsx"){
    file_suffix <- paste0(file_suffix,".xlsx")
  }
  output_dir <- paste0(output_dir, "excels/")
  dir.create(output_dir, showWarnings = FALSE)
  wb_filename <- paste0(output_dir, projection_name, file_suffix)
  xlsx::saveWorkbook(wb, wb_filename)
  
}
