output_projection <- function(projection, output_dir, timestamp) {
  
  #RDS
  lapply(seq_along(projection), 
         function(i) saveRDS(projection[[i]], paste0(output_dir, names(projection)[[i]],timestamp, ".rds"), compress = "gzip")) %>%
    invisible()
  
  #CSV
  csv_dir <- paste0(output_dir,"csv/")
  dir.create(csv_dir)
  
  make_csvs <- function(data, name_stub){
    
    data_col <- names(data)[ncol(data)]
    cols <- setdiff(names(data), data_col)
    
    female <- filter(data, sex == "female")
    male <- filter(data, sex == "male")
    persons <- data %>%
      mutate(sex = "persons") %>%
      rename(value = !!data_col) %>%
      group_by_at(cols) %>%
      summarise(!!data_col := sum(value)) %>%
      ungroup()
    
    data.table::fwrite(female, paste0(name_stub,"_female.csv"))
    data.table::fwrite(male, paste0(name_stub,"_male.csv"))
    data.table::fwrite(persons, paste0(name_stub,"_persons.csv"))
    
  }
  
  lapply(seq_along(projection), 
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
                    output_dir = config_list$outputs_dir,
                    file_name = paste0("datastore_",timestamp,".xlsx"),
                    output_date = config_list$timestamp,
                    write_excel = config_list$write_excel)
}


#Config
#TODO Test
config_output <- data.frame()
for(i in seq_along(config_list)){
  config_output <- rbind(config_output,
                         data.frame(config = paste(names(config_list)[[i]], config_list[[i]], sep=": ")))
}
data.table::fwrite(config_output, "config.txt", col.names = FALSE)
