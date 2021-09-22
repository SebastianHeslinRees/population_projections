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
#' @import stringr
#' @importFrom data.table fread
#'
#' @export

create_small_area_excels <- function(output_dir, wb_filename, projection_name, smallarea = "ward"){
  
  message(paste("Creating", smallarea, "Excel workbook"))
  
  #Create excels output directory
  output_dir <- .add_slash(output_dir)
  excel_dir <- paste0(output_dir,"excels/")
  dir.create(excel_dir, showWarnings = FALSE)
  
  #Create workbook names
  if(str_sub(wb_filename,-5,-1)==".xlsx"){
    wb_filename <- substr(wb_filename,1,nchar(wb_filename)-5)
  }
  wb_filename <- paste0(wb_filename,"_",smallarea,".xlsx")
  
  #read in the data from csv
  persons <- doob("persons", smallarea, output_dir)
  females <- doob("females", smallarea, output_dir)
  males <- doob("males", smallarea, output_dir)
  
  components = fread(paste0(output_dir, smallarea,"/components_",smallarea,".csv"), header = TRUE) %>%
    as.data.frame() %>% 
    filter(year <= 2041)
  
  #Source python function
  #TODO
  #FIXME
  #There is a problem with reticulate/python/renv that I can't get to the bottom of
  #The first time a python script is sourced the function throws an error. It can't find
  #rpytools. If you source again it works. The temp work around is to wrap the source in
  #try() so the error is caught and then run it a second time. Its ugly but it works.
  #See also trendmodel::create_household_model_excels, etc
  
  try(source_python('model_code/other_scripts/python_to_excel_small_area.py'))
  source_python('model_code/other_scripts/python_to_excel_small_area.py') 
  
  python_to_excel_smallarea(persons, females, males, components,
                            excel_dir, wb_filename, projection_name, smallarea)
  
}

#If the projection horizon is >2041 remove extra years, if not use max(year)
doob <- function(x, smallarea, output_dir){
  
  a <- fread(paste0(output_dir, smallarea,"/",x,"_",smallarea,".csv"), header = TRUE) %>%
    as.data.frame()
  
  b <- as.numeric(last(names(a)))
  
  if(b>2041){
    a <- select(a, -c(as.character(2042:b)))
  }
  
  return(a)
  
}