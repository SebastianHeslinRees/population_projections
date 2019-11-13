output_projection <- function(projection, output_dir, timestamp, write_excel) {
  
  #RDS
  lapply(seq_along(projection), 
         function(i) saveRDS(projection[[i]], paste0(output_dir, names(projection)[[i]], "_", timestamp, ".rds"), compress = "gzip")) %>%
    invisible()
  
  #CSV
  csv_dir <- paste0(output_dir,"csv_", timestamp, "/")
  dir.create(csv_dir)
  
  make_csvs <- function(data, name_stub){
   
    data_col <- names(data)[ncol(data)]
    cols <- setdiff(names(data), data_col)
    cols <- c("gss_code", "gss_name", cols[cols!="gss_code"])
    
    data <- left_join(data, get_gss_names(), by="gss_code") %>%
      filter(year >= 2011) %>%
      select_at(c(cols, data_col))
    
    female <- filter(data, sex == "female") %>%
      rename(rounded = data_col) %>%
      mutate(rounded = round(rounded, 3)) %>%
      pivot_wider(names_from = year, values_from = rounded)

    male <- filter(data, sex == "male")  %>%
      rename(rounded = data_col) %>%
      mutate(rounded = round(rounded, 3))%>%
      pivot_wider(names_from = year, values_from = rounded)
    
    persons <- data %>%
      mutate(sex = "persons") %>%
      rename(value = !!data_col) %>%
      group_by_at(cols) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(value = round(value, 3))%>%
      pivot_wider(names_from = year, values_from = value)
    
    data.table::fwrite(female, paste0(name_stub,"_female.csv"))
    data.table::fwrite(male, paste0(name_stub,"_male.csv"))
    data.table::fwrite(persons, paste0(name_stub,"_persons.csv"))
    
  }
  
  #TODO Hard-coding the 7 here is a risk
  lapply(seq(7), 
         function(i) make_csvs(projection[[i]], paste0(csv_dir, names(projection)[[i]]))) %>%
    invisible()
  
  #Excel
  datastore_outputs(population = projection$population,
                    births = projection$births,
                    deaths = projection$deaths,
                    int_in = projection$int_in,
                    int_out = projection$int_out,
                    dom_in = projection$dom_in,
                    dom_out = projection$dom_out,
                    output_dir = output_dir,
                    file_name = paste0("datastore_",timestamp,".xlsx"),
                    output_date = config_list$timestamp,
                    write_excel = write_excel)
  #Config
  #TODO Test
  # config_output <- data.frame()
  # for(i in seq_along(config_list)){
  #   config_output <- rbind(config_output,
  #                          data.frame(config = paste(names(config_list)[[i]], config_list[[i]], sep=": ")))
  # }
  # data.table::fwrite(config_output, paste0(output_dir, "config.txt"), col.names = FALSE)
  
}