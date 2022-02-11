#' Source a python function to output Ward and/or MSOA excel files
#' 
#' A wrapper for a python function which efficiently creates model output
#' Excel files.
#'  
#' There is a problem with reticulate/python/renv that I can't get to the bottom of
#' The first time a python script is sourced the function throws an error. It can't find
#' rpytools. If you source again it works. The temp work around is to wrap the source in
#' try() so the error is caught and then run it a second time. Its ugly but it works.
#' See also trendmodel::create_household_model_excels, etc
#'
#' @param output_dir string. The directory from which to read the data and save the Excel file
#' @param wb_filename String. The name of the output Excel file. With or without '.xlsx'
#' @param projection_name String. The projection name.
#' @param bpo FALSE or a gss code indicating which borough's bpo this is
#' @param smallarea String. Projection type: either 'ward' or 'msoa'. Default 'ward'.
#' 
#' @return Output Excel workbooks
#' 
#' @import reticulate
#' @import dplyr
#' @import popmodules
#' @import stringr
#' @importFrom data.table fread
#'
#' @export

create_excel <- function(output_dir, wb_filename, projection_name, bpo = FALSE, smallarea = "ward"){
  
  message(paste("Creating", smallarea, "Excel workbook"))
  output_dir <- .add_slash(output_dir)

  #Create workbook names
  if(str_sub(wb_filename,-5,-1)!=".xlsx"){ wb_filename <- paste0(wb_filename,".xlsx") }
  
  #read in the data from csv
  persons <- .read_and_filter("persons", output_dir, bpo)
  females <- .read_and_filter("female", output_dir, bpo)
  males <- .read_and_filter("male", output_dir, bpo)
  components <- .read_and_filter("components", output_dir, bpo)
  
  try( reticulate::source_python('model_code/other_scripts/python_to_excel_small_area2.py'), silent = TRUE)
  reticulate::source_python('model_code/other_scripts/python_to_excel_small_area2.py') 
  python_to_excel_smallarea2(persons, females, males, components,
                             output_dir,
                             wb_filename,
                             projection_name,
                             smallarea)
  
}


.read_and_filter <- function(x, output_dir, bpo){
  
  if(x != "components"){
    
    a <- fread(paste0(output_dir, "csv/population_",x,".csv"), header = TRUE) %>%
      as.data.frame() %>% 
      arrange(gss_code, gss_code_ward, age)
    
    b <- as.numeric(last(names(a)))
    
    if(b>2041){
      a <- select(a, -c(as.character(2042:b)))
    }
    
  } else {
    
    a <- readRDS(paste0(output_dir, "summary.rds")) %>%
      as.data.frame() %>% 
      filter(year %in% 2011:2041)
  }
  
  if(bpo!=FALSE){
    a <- filter(a, gss_code == bpo)
  }
  
  return(a)
  
}
