output_housing_led_projection <- function(projection, output_dir, timestamp){
  
  dir.create(output_dir, recursive = T)
  
  lapply(seq_along(projection),
         function(i) saveRDS(projection[[i]],
                             paste0(output_dir, names(projection)[i],"_",timestamp,".rds"))) %>%
    invisible()
  
}