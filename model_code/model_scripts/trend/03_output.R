output_projection <- function(projection, output_dir, timestamp) {
  
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  # TODO add timestamp to file name and pass in from control.R
  
  if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")
  
  lapply(seq_along(projection), 
         function(i) saveRDS(projection[[i]], paste0(output_dir, names(projection)[[i]],timestamp, ".rds"), compress = "gzip")) %>%
    invisible()
  
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
