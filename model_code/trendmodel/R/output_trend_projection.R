#' Output a trend population projection
#'
#' Write csv, rds and xlsx model outputs from the \code{trend_core} cohort component
#' model. Input is taken from the \code{arrange_trend_core_outputs} function.
#'
#' @param projection A list. The model outputs from \code{arrange_trend_core_outputs}
#' @param output_dir String. The output location folder path
#' @param write_excel Logical. Should the Excel outputs be written
#' @param n_csv_elements Numeric. The function will output csv files for elements
#'  1:\code{n_csv_elements} of the \code{projection} list.
#' @param projection_name String. Name of the projection.
#'
#' @import dplyr
#' @import popmodules
#' @importFrom stringr str_detect
#' @importFrom data.table fwrite
#' @importFrom tidyr pivot_wider

output_trend_projection <- function(projection, output_dir, write_excel, n_csv_elements,
                                    projection_name) {
  
  projection[1:16] <- lapply(projection[1:16], reorder_for_output) 
  
  #RDS
  for(i in c(1:16)) {
    saveRDS(projection[[i]], paste0(output_dir, names(projection)[[i]], ".rds"), compress = "gzip")
  }
  
  #CSV
  csv_dir <- paste0(output_dir,"csv/")
  dir.create(csv_dir, showWarnings = FALSE)
  
  make_csvs <- function(data, name_stub){
    
    data_col <- names(data)[ncol(data)]
    cols <- setdiff(names(data), data_col)
    cols <- c("gss_code", "gss_name", cols[cols!="gss_code"])
    
    data <- left_join(data, get_gss_names(), by="gss_code") %>%
      filter(year >= 2011) %>%
      select_at(c(cols, data_col))
    
    if(str_detect(name_stub, "births_by_mothers_age")){
      
      b_m_a <- filter(data, sex == "female", age %in% 15:49) %>%
        rename(rounded = !!data_col) %>%
        mutate(rounded = round(rounded, 3)) %>%
        pivot_wider(names_from = year, values_from = rounded) %>%
        reorder_for_output()
      
      data.table::fwrite(b_m_a, paste0(name_stub,".csv"))
      
    } else {
      
      female <- filter(data, sex == "female") %>%
        rename(rounded = !!data_col) %>%
        mutate(rounded = round(rounded, 3)) %>%
        arrange(year) %>% # so that pivot_wider behaves
        pivot_wider(names_from = year, values_from = rounded) %>%
        arrange(gss_code, sex, age) %>%
        reorder_for_output()
      
      male <- filter(data, sex == "male")  %>%
        rename(rounded = !!data_col) %>%
        mutate(rounded = round(rounded, 3))%>%
        arrange(year) %>%
        pivot_wider(names_from = year, values_from = rounded) %>%
        arrange(gss_code, sex, age) %>%
        reorder_for_output()
      
      persons <- data %>%
        mutate(sex = "persons") %>%
        rename(value = !!data_col) %>%
        group_by_at(cols) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(value = round(value, 3))%>%
        arrange(year) %>%
        pivot_wider(names_from = year, values_from = value) %>%
        arrange(gss_code, sex, age) %>%
        reorder_for_output()
      
      data.table::fwrite(female, paste0(name_stub,"_female.csv"))
      data.table::fwrite(male, paste0(name_stub,"_male.csv"))
      data.table::fwrite(persons, paste0(name_stub,"_persons.csv"))
      
    }
  }
  
  for(i in 1:n_csv_elements) { 
    make_csvs(projection[[i]], paste0(csv_dir, names(projection)[[i]]))
  }
  make_csvs(projection$popn_adjustment, paste0(csv_dir, "popn_adjustment"))
  
  #Excel
  if(write_excel){
    trend_datastore_outputs(population = projection$population,
                            births = projection$births,
                            deaths = projection$deaths,
                            int_in = projection$int_in,
                            int_out = projection$int_out,
                            dom_in = projection$dom_in,
                            dom_out = projection$dom_out,
                            popn_adjustment = projection$popn_adjustment,
                            output_dir = output_dir,
                            excel_file_name = paste0(projection_name,".xlsx"))
  }
}