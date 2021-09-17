#' Source a python function to output Ward and/or MSOA excel files
#' 
#' A wrapper for a python function which efficiently creates model output
#' Excel files. Run time is 3 minutes for a ward projection and 5 minutes for
#' an MSOA projection.
#'
#' @param output_dir Character. The root model output directory.
#' @param wb_filename Character. The name of the output Excel Workbook.
#' @param ward Logical. Should the ward file be output
#' @param msoa Logical. Should the MSOA file be output
#' 
#' @return Output Excel workbooks
#' 
#' @import reticulate
#' @import dplyr
#' @import popmodules
#' @importFrom data.table fread
#'
#' @export

create_small_area_excels <- function(output_dir, wb_filename, smallarea = "ward"){
  
  #Create excels output directory
  output_dir <- .add_slash(output_dir)
  dir.create(paste0(output_dir,"excels"), showWarnings = FALSE)
  
  #Create workbook names
  if(substr(wb_filename, nchar(wb_filename)-4, nchar(wb_filename)) == ".xlsx"){
    wb_filename <- substr(wb_filename,1,nchar(wb_filename)-5)
  } 
  
  filename <- paste0(wb_filename,"_",smallarea,".xlsx")
  
  #read in the data from csv
  persons = fread(paste0(output_dir, smallarea,"/persons_",smallarea,".csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  
  females = fread(paste0(output_dir, smallarea,"/females_",smallarea,".csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  
  males = fread(paste0(output_dir, smallarea,"/males_",smallarea,".csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  
  components = fread(paste0(output_dir, smallarea,"/components_",smallarea,".csv"), header = TRUE) %>%
    as.data.frame() %>% 
    filter(year <= 2041)
  
  
  #Source python function
  
  #TODO
  #FIXME
  #There is a problem with reticulate/python/renv that I can't get to the bottom of
  #The first time a python script is sourced the function throws an error. It can't find
  #rpytools. If you source again it works. The temp work arund is to wrap the source in
  #try() so the error is caught and then run it a second time. Its ugly but it works.
  #See also trendmodel::create_household_model_excels, etc
  
  try(source_python('model_code/other_scripts/python_to_excel_small_area.py'))
  source_python('model_code/other_scripts/python_to_excel_small_area.py') 
  
  python_to_excel_smallarea(persons, females, males, components, output_dir, filename, smallarea) #~3.5 mins
  message(paste(smallarea, "excel output complete"))
  
}