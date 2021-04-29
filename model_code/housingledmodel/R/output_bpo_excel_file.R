#' Create the BPO excel output
#' 
#' Process population, components and development data and place it into an
#' Excel template.
#'
#' @param data The \code{[['csv']]} element from a ward projection
#' @param output_dir The directory in which to save the Excel file
#' @param projection_name The name of the output Excel file
#' @param bpo_gss_code The gss code of the bpo projection
#' @param popn_adjustment_path path to the population adjustment RDS file
#' @param file_suffix A string to append to the end of the file name
#' 
#' @import xlsx
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' 
#' @export

output_bpo_excel_file <- function(data, output_dir, projection_name, bpo_gss_code,
                                  popn_adjustment_path,
                                  file_suffix = "_BPO_2019.xlsx"){
  
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
  
  components <- data[["components"]] %>% 
    bpo_data(col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name","year"))

  popn_adjustment <- readRDS(popn_adjustment_path) %>% 
    filter(gss_code == bpo_gss_code) %>% 
    mutate(borough = unique(components$borough),
           gss_code_ward = gss_code,
           ward_name = paste0(borough, " (total)")) %>% 
    group_by(gss_code, borough, gss_code_ward, ward_name, year) %>% 
    summarise(covid_deaths = sum(upc)*-1, .groups = 'drop_last') %>%
    data.frame() %>% 
    mutate(covid_deaths = round(covid_deaths, 2))
  
  components <- left_join(components, popn_adjustment,
                          by = c("gss_code", "borough", "gss_code_ward", "ward_name","year")) %>% 
    mutate(covid_deaths = ifelse(gss_code_ward==bpo_gss_code & is.na(covid_deaths),0,covid_deaths)) %>% 
    mutate(migration = ifelse(gss_code_ward==bpo_gss_code, migration + covid_deaths, migration)) %>% 
    select(gss_code, borough, gss_code_ward, ward_name, year,
           popn, births, deaths, migration, covid_deaths, total_change)
  
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
    pivot_wider(names_from = year, values_from = units) %>%
    rbind(ward_dev_dataframe) %>%
    as.data.frame()
  
  wb <- xlsx::loadWorkbook("input_data/excel_templates/ward_housing_led_2019_based_template.xlsx")
  wb_sheets<- xlsx::getSheets(wb)
  
  addDataFrame(bpo_data(persons), wb_sheets$Persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(bpo_data(males), wb_sheets$Males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(bpo_data(females), wb_sheets$Females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(components, wb_sheets$Components, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  addDataFrame(assumed_dev_dataframe, wb_sheets$Assumed, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #dev data source 
  if(grepl("shlaa", projection_name, fixed = TRUE)){
    dev_source_text <- as.data.frame("4. These projections incorporate assumptions about future development adapted from the 2017 SHLAA")
  } else {
    dev_source_text <- as.data.frame(paste0("4. These projections incorporate assumptions about future development provided by the London Borough of ",
                                            unique(assumed_dev_dataframe$borough)))
  }
  date_of_projection <- as.data.frame(paste0("Projection run ",format(Sys.time(), "%d %B %Y")))
  
  
  addDataFrame(dev_source_text, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 11, startColumn = 1)
  addDataFrame(date_of_projection, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #Write xlsx file
  if(substr(file_suffix, nchar(file_suffix)-4, nchar(file_suffix)) != ".xlsx"){
    file_suffix <- paste0(file_suffix,".xlsx")
  }
  wb_filename <- paste0(output_dir, projection_name, file_suffix)
  xlsx::saveWorkbook(wb, wb_filename)
  
}