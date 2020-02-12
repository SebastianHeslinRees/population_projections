output_bpo_excel_file <- function(data, output_dir, projection_name,
                                  small_area_dev_trajectory_path,
                                  borough_dev_trajectory_path,
                                  bpo = bpo){
  
  bpo_data <- function(x, bpo_gss=bpo,
                       col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name", "sex", "age")){
    x %>%
      dtplyr::lazy_dt() %>%
      filter(gss_code == bpo_gss) %>%
      mutate(gss_code_ward = gss_code,
             ward_name = paste0(borough, " (total)")) %>%
      group_by_at(col_aggregation) %>%
      summarise_all(.funs=sum) %>%
      as.data.frame() %>%
      rbind(x) %>%
      dtplyr::lazy_dt() %>%
      filter(gss_code == bpo_gss) %>%
      as.data.frame()
  }
  
  persons <- data[["persons"]]
  females <- data[["females"]]
  males <- data[["males"]]
  components <- data[["components"]]
  
  ward_dev_dataframe <- readRDS(small_area_dev_trajectory_path) %>%
    left_join(readRDS("input_data/lookup/2011_ward_to_district.rds"), by="gss_code_ward") %>%
    filter(gss_code == bpo) %>%
    left_join(data.table::fread("input_data/lookup/lad18_code_to_name.csv"), by="gss_code") %>%
    select(gss_code, borough=gss_name, gss_code_ward, ward_name, year, units) %>%
    tidyr::pivot_wider(names_from = year, values_from = units)
  
  assumed_dev_dataframe <- readRDS(borough_dev_trajectory_path) %>%
    filter(gss_code == bpo) %>%
    mutate(borough = unique(ward_dev_dataframe$borough),
           gss_code_ward = gss_code,
           ward_name = paste0(borough, " (total)")) %>%
    select(gss_code, borough, gss_code_ward, ward_name, year, units) %>%
    tidyr::pivot_wider(names_from = year, values_from = units) %>%
    rbind(ward_dev_dataframe) %>%
    as.data.frame()
  
  wb <- xlsx::loadWorkbook("input_data/housing_led_model/ward_housing_led_2018_based_template.xlsx")
  wb_sheets<- getSheets(wb)
  
  xlsx::addDataFrame(bpo_data(persons), wb_sheets$Persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(bpo_data(males), wb_sheets$Males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(bpo_data(females), wb_sheets$Females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(bpo_data(components, col_aggregation = c("gss_code", "borough", "gss_code_ward", "ward_name","year")),
                     wb_sheets$Components, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(assumed_dev_dataframe, wb_sheets$Assumed, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #dev data source 
  dev_source_text <- as.data.frame(paste0("4. These projections incorporate assumptions about future development provided by the London Borough of ",
                                          unique(assumed_dev_dataframe$borough)))
  
  xlsx::addDataFrame(dev_source_text, wb_sheets$Metadata, col.names = FALSE, row.names = FALSE, startRow = 11, startColumn = 1)
  
  #Write xlsx file
  wb_filename <- paste0(output_dir, projection_name,"_BPO.xlsx")
  saveWorkbook(wb, wb_filename)
  
}




