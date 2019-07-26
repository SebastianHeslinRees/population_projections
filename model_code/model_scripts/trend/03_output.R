output_projection <- function(projection, output_dir, timestamp) {
  # TODO create dir if it doesn't exist
  
  # TODO add timestamp to file name and pass in from control.R
  
  if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")
  
  lapply(seq_along(projection), 
         function(i) write_rds(projection[[i]], paste0(output_dir, names(projection)[[i]],timestamp, ".rds"), "gz")) %>%
    invisible()
  
}

