#' Create the BPO excel output
#' 
#' Process population, components and development data and place it into an
#' Excel template.
#'
#' @param data The \code{[['csv']]} element from a ward projection
#' @param output_dir The directory in which to save the Excel file
#' @param projection_name The name of the output Excel file
#' @param popn_adjustment_path path to the population adjustment RDS file
#' @param file_suffix A string to append to the end of the file name
#' 
#' @import xlsx
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' 
#' @export

output_housing_led_excel_file <- function(data, output_dir, projection_name,
                                          popn_adjustment_path, dev_trajectory_path,
                                          file_suffix = "_2019.xlsx"){
  
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
    
    x <- filter(a, substr(gss_code,1,3)=="E09") %>% 
      arrange(gss_code, year, sex, age)
    
  }
  
  persons <- data[["persons"]] %>% prep_data()
  females <- data[["females"]] %>% prep_data()
  males <- data[["males"]] %>% prep_data()
  
  components <- data.table::fread(paste0(output_dir,"csv/components.csv"), header = TRUE) %>% 
    as.data.frame() %>% 
    rename(population = popn)
  
  names(components) <- stringr::str_replace_all(names(components), "_", " ")
  
  assumed_dev_dataframe <- readRDS(dev_trajectory_path) %>%
    as.data.frame() %>% 
    filter(year > 2011) %>% 
    mutate(units = round(units,0)) %>% 
    left_join(unique(select(persons, gss_code, borough)), by= "gss_code") %>% 
    tidyr::pivot_wider(names_from = "year", values_from = "units") %>% 
    as.data.frame() %>% 
    prep_data(col_aggregation = c("gss_code","borough"))
  
  stock_dataframe <- readRDS(dev_trajectory_path) %>%
    as.data.frame() %>% 
    group_by(gss_code) %>% 
    mutate(units = round(cumsum(units),0)) %>% 
    data.frame() %>% 
    left_join(unique(select(persons, gss_code, borough)), by= "gss_code") %>% 
    tidyr::pivot_wider(names_from = "year", values_from = "units") %>% 
    as.data.frame() %>% 
    prep_data(col_aggregation = c("gss_code","borough"))
  
  wb <- xlsx::loadWorkbook("input_data/excel_templates/housing_led_2019_based_template.xlsx")
  wb_sheets<- xlsx::getSheets(wb)
  cs <- CellStyle(wb) + Font(wb, isBold=TRUE)
  
  addDataFrame(persons, wb_sheets$Persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(males, wb_sheets$Males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(females, wb_sheets$Females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(components, wb_sheets$Components, col.names = TRUE, row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = cs)
  addDataFrame(assumed_dev_dataframe, wb_sheets$Assumed, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(stock_dataframe, wb_sheets$Housing, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #dev data source 
  if(grepl("scenario_1", projection_name, fixed = TRUE)){
    dev_source_text <- as.data.frame("4. These projections incorporate assumptions about future development adapted from the 2017 SHLAA and Savills Research")
    subtitle <- as.data.frame("Scenario 1 - Standard Development Assumptions")  
  }
  if(grepl("scenario_2", projection_name, fixed = TRUE)){
    dev_source_text <- as.data.frame("4. These projections assume future development at a level consistent with the LDD average for the period 2012-2019")
    subtitle <- as.data.frame("Scenario 1 - LDD Average Development Assumptions")
  }
  if(grepl("scenario_3", projection_name, fixed = TRUE)){
    dev_source_text <- as.data.frame("4. These projections assume future development at a level adapted from the 2021 London Plan delivery target")
    subtitle <- as.data.frame("Scenario 1 - London Plan Development Assumptions")
  }
  
  date_of_projection <- as.data.frame(paste0("Projection run ",format(Sys.time(), "%B %Y")))
  
  addDataFrame(dev_source_text, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 12, startColumn = 1)
  addDataFrame(date_of_projection, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 3, startColumn = 1)
  addDataFrame(subtitle, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  
  #Write xlsx file
  if(substr(file_suffix, nchar(file_suffix)-4, nchar(file_suffix)) != ".xlsx"){
    file_suffix <- paste0(file_suffix,".xlsx")
  }
  output_dir <- paste0(output_dir, "excels/")
  dir.create(output_dir, showWarnings = FALSE)
  wb_filename <- paste0(output_dir, projection_name, file_suffix)
  xlsx::saveWorkbook(wb, wb_filename)
  
  #Ward template
  covid_disclaimer <- data.frame(c("9. Deaths from covid-19  are not  modelled at ward level and so are not included in the ward-level outputs.",
                                   "The impact of coivd-19 on ward populations is applied in the model through the constraining process."))
  blank <- data.frame(rep("",10))
  
  wb <- xlsx::loadWorkbook("input_data/excel_templates/ward_housing_led_2019_based_template.xlsx")
  wb_sheets<- xlsx::getSheets(wb)
  addDataFrame(dev_source_text, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 11, startColumn = 1)
  addDataFrame(subtitle, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(covid_disclaimer, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 18, startColumn = 1)
  addDataFrame(blank, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 20, startColumn = 1)
  removeSheet(wb, wb_sheets$Assumed)
  wb_filename <- paste0(output_dir, "ward_", projection_name, file_suffix)
  xlsx::saveWorkbook(wb, wb_filename)
  
  #MSOA template
  covid_disclaimer <- data.frame(c("9. Deaths from covid-19  are not  modelled at MSOA level and so are not included in the MSOA-level outputs.",
                                   "The impact of coivd-19 on MSOA populations is applied in the model through the constraining process."))
  addDataFrame(covid_disclaimer, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 18, startColumn = 1)
  
  for(i in 3:6){
    rows <- getRows(wb_sheets[[i]])
    cells <- getCells(rows)
    setCellValue(cells$`1.3`, "gss code msoa")
    setCellValue(cells$`1.4`, "msoa name")
  }
  
  wb_filename <- paste0(output_dir, "msoa_", projection_name, file_suffix)
  xlsx::saveWorkbook(wb, wb_filename)
}
