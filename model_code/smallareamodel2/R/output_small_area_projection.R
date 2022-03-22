#' Write model outputs to RDS and CSV
#' 
#' Outputs from the small area housing-led model, passed through the \code{arrange}
#' function are written out a RDS and CSV files
#' 
#' @param projection A list. The projection as processed through the arrange function
#' @param output_dir String. The path to the output folder where files will be written
#' @param model  A string. Either 'trend' or 'housing-led' indicating what data is
#'  being passed to the function. Used to select which list elements to output as CSV.
#' 
#' @import dplyr
#' @importFrom data.table fwrite
#' @importFrom stringr str_detect
#' 
#' @export

output_small_area_projection <- function(projection,
                                         output_dir = config_list$output_dir,
                                         model){
  
  
  #RDS
  message("writing rds files")
  
  for(i in names(projection)) {
    saveRDS(projection[[i]], paste0(output_dir, i, ".rds"), compress = "gzip")
  }
  
  #-----------------------------------------------------------------------------
  
  #CSV
  
  message("writing csv files")
 
  csv_dir <- paste0(output_dir,"csv/")
  dir.create(csv_dir, showWarnings = FALSE)
  csv_elements <- c(1:6, 9:10)
  
  if(model == "housing-led"){ csv_elements <- c(csv_elements, 11, 15:22, 24:25) }
  
  for(i in csv_elements) { 
    .make_csvs(projection[[i]], paste0(csv_dir, names(projection)[[i]]))
  }
  
  #-----------------------------------------------------------------------------
  
  projection$detailed_components <- projection$detailed_components %>%                  
    mutate(across(where(is.numeric) & !c(year, age), ~round(.x, digits=3))) %>% 
    data.frame()

  fwrite(projection$detailed_components, paste0(output_dir,"/csv/components_of_change_detailed.csv"))
  fwrite(projection$summary, paste0(output_dir,"/csv/components_of_change_summary.csv"))
  fwrite(projection$borough_data.summary, paste0(output_dir,"/csv/borough_components_of_change_summary.csv"))
  
  #-----------------------------------------------------------------------------
  
  if(model == "housing-led"){
    
    fwrite(projection$ahs, paste0(output_dir,"/csv/ahs.csv"))
    fwrite(projection$ahs_detail, paste0(output_dir,"/csv/ahs_detailed.csv"))
    fwrite(projection$households_detail, paste0(output_dir,"/csv/households_detail.csv"))
    
  }
  
  #-----------------------------------------------------------------------------
 
  invisible()
  
}

.make_csvs <- function(data, name_stub){
  
  name_stub <- str_remove_all(name_stub, "data.")
  
  data_col <- last(names(data))
  cols <- setdiff(names(data), data_col)
  arrange_by <- intersect(c("gss_code", "gss_code_ward", "age"), cols)
  
  data <- data %>% filter(year >= 2011) %>% lazy_dt()
  
  
  if(str_detect(name_stub, "births_by_mothers_age")){
    
    b_m_a <- filter(data, sex == "female", age %in% 15:49) %>%
      rename(rounded = !!data_col) %>%
      mutate(rounded = round(rounded, 3)) %>%
      pivot_wider(names_from = year, values_from = rounded) %>% 
      data.frame()
    
    fwrite(b_m_a, paste0(name_stub,".csv"))
    
  } else if(!"sex" %in% cols){
    
    x <- data %>%
      rename(rounded = !!data_col) %>%
      mutate(rounded = round(rounded, 3)) %>%
      arrange(year)  %>% 
      data.frame() %>%
      pivot_wider(names_from = year, values_from = rounded) %>%
      arrange(across(arrange_by))
    
    fwrite(x, paste0(name_stub,".csv"))
    
  } else {
    
    female <- filter(data, sex == "female") %>%
      rename(rounded = !!data_col) %>%
      mutate(rounded = round(rounded, 3)) %>%
      arrange(year)  %>% 
      data.frame() %>%
      pivot_wider(names_from = year, values_from = rounded) %>%
      arrange(across(arrange_by))
    
    male <- filter(data, sex == "male")  %>%
      rename(rounded = !!data_col) %>%
      mutate(rounded = round(rounded, 3))%>%
      arrange(year) %>% 
      data.frame() %>%
      pivot_wider(names_from = year, values_from = rounded) %>%
      arrange(across(arrange_by))
    
    persons <- data %>%
      mutate(sex = "persons") %>%
      rename(value = !!data_col) %>%
      group_by(across(!!cols)) %>%
      summarise(value = sum(value), .groups = 'drop_last') %>%
      data.frame() %>%
      mutate(value = round(value, 3))%>%
      arrange(year) %>%
      pivot_wider(names_from = year, values_from = value) %>%
      arrange(across(arrange_by)) %>% 
      data.frame() %>% 
      rename_with(.cols = starts_with("X"), .fn = function(x){substr(x,2,5)})
    
    fwrite(female, paste0(name_stub,"_female.csv"))
    fwrite(male, paste0(name_stub,"_male.csv"))
    fwrite(persons, paste0(name_stub,"_persons.csv"))
    
  }
}
