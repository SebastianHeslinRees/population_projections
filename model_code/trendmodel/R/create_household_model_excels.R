#' A wrapper for a Python function to output household projection Excels
#'
#' The Python function read-in model output CSV files and outputs 2 Excel files:
#' one for the ONS model and one for the DCLG model
#'
#' @param output_dir string. The directory from which to read the data and save the Excel file
#' @param wb_filename String. The name of the output Excel file. With or without '.xlsx'
#' @param projection_name String. The projection name.
#' @param model String. Household to output: 'ons', 'dclg' or 'both'. Default 'both'.
#' 
#' @return Output Excel workbooks
#' 
#' @import reticulate
#' @import popmodules
#' 
#' @export

create_household_model_excels <- function(output_dir, wb_filename, projection_name, model="both"){
  
  #datastore directory
  output_dir <- .add_slash(output_dir)
  datastore_dir <- paste0(output_dir,"datastore")
  dir.create(datastore_dir, recursive = T, showWarnings = F)
  
  #excel file name
  if(str_sub(wb_filename,-5,-1)==".xlsx"){
    wb_filename <- substr(wb_filename,1,nchar(wb_filename)-5)
  }
  
  #TODO
  #FIXME
  #There is a problem with reticulate/python/renv that I can't get to the bottom of
  #The first time a python script is sourced the function throws an error. It can't find
  #rpytools. If you source again it works. The temp work around is to wrap the source in
  #try() so the error is caught and then run it a second time. Its ugly but it works.
  #See also trendmodel::create_household_model_excels, etc
  
  try(source_python('model_code/other_scripts/python_to_excel_households.py'), silent = TRUE)
  source_python('model_code/other_scripts/python_to_excel_households.py') 
  
  if(model %in% c("ons", "both")){
    python_to_excel_households(output_dir, paste0(wb_filename,"_ons_households.xlsx"), projection_name, "ons")
  }
  if(model %in% c("dclg", "both")){
    python_to_excel_households(output_dir, paste0(wb_filename,"_dclg_households.xlsx"), projection_name, "dclg")
  }
  
}