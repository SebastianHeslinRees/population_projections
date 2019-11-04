output_projection <- function(projection, output_dir, timestamp) {
  
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  # TODO add timestamp to file name and pass in from control.R
  
  if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")
  
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
  
}