#' Create the 2020-based housing-led projection excel output
#' 
#' Process population, components and development data and place it into an
#' Excel template. London borough data.
#'
#' @param data The \code{[['csvs']]} element from a ward projection
#' @param output_dir The directory in which to save the Excel file
#' @param projection_name The name of the output Excel file
#' @param file_suffix A string to append to the end of the file name
#' 
#' @import xlsx
#' @import dplyr
#' @importFrom data.table fread
#' 
#' @export

output_borough_excels <- function(data, output_dir, projection_name, file_suffix = ".xlsx"){
  
  prep_data <- function(x, col_aggregation = c("gss_code", "borough", "sex", "age")){
    
    if("ward_name" %in% names(x)){
      x <- x %>%
        as.data.frame() %>%
        select(-gss_code_ward, -ward_name)
    }
    
    y <- x %>% 
      as.data.frame() %>%
      mutate(gss_code = "E12000007",
             borough = "London (total)") %>% 
      dtplyr::lazy_dt() %>%
      group_by_at(col_aggregation) %>%
      summarise_all(.funs=sum) %>%
      as.data.frame()
    
    z <- x %>% 
      dtplyr::lazy_dt() %>%
      group_by_at(col_aggregation) %>%
      summarise_all(.funs=sum) %>%
      as.data.frame()
    
    return(rbind(y,z))
    
  }
  
  #-----------------------------------------------------------------------------
  
  persons <- data[["persons"]] %>% prep_data()
  females <- data[["females"]] %>% prep_data()
  males <- data[["males"]] %>% prep_data()
  
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
