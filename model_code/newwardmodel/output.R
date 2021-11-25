output_projection <- function(projection,
                              output_dir = config_list$output_dir,
                              n_csv_elements = 8){
  
  
  #RDS
  for(i in names(projection)) {
    saveRDS(projection[[i]], paste0(output_dir, i, ".rds"), compress = "gzip")
  }
  
  #CSV
  csv_dir <- paste0(output_dir,"csv/")
  dir.create(csv_dir, showWarnings = FALSE)
  
  make_csvs <- function(data, name_stub){
    
    data_col <- names(data)[ncol(data)]
    cols <- setdiff(names(data), data_col)
    cols <- c("gss_code", "gss_code_ward", "gss_name", cols[!cols %in% c("gss_code","gss_code_ward")])
    
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
        summarise(value = sum(value), .groups = 'drop_last') %>%
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
  
  projection$net_migration <- select(projection$net_migration, -outflow, -inflow)
  for(i in 1:n_csv_elements) { 
    make_csvs(projection[[i]], paste0(csv_dir, names(projection)[[i]]))
  }
  fwrite(projection$detailed_components, paste0(output_dir,"/csv/components of change.csv"))
  fwrite(projection$summary, paste0(output_dir,"/csv/summary.csv"))
  
  #Excel
  # if(write_excel){
  #   excel_wb_name <- substr(projection_name,1,nchar(projection_name)-14)
  #   variant_name <- paste("Variant trend projection:",str_replace_all(excel_wb_name, "_", " "))
  #   create_trend_model_excels(output_dir, excel_wb_name, variant_name, FALSE)
  # }
}