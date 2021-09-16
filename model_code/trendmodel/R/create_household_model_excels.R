#' A wrapper for a Python function to output household projection Excels
#'
#' The Python function read-in model output CSV files and outputs 2 Excel files:
#' one for the ONS model and one for the DCLG model
#'
#' @param output_dir String. The root output directory of the projection
#' @param projection_name String. The name of the projection
#' 
#' @return Output Excel workbooks
#' 
#' @import reticulate
#' 
#' @export

create_household_model_excels <- function(output_dir, projection_name, model="both"){
  
  #TODO
  #FIXME
  #There is a problem with reticulate/python/renv that I can't get to the bottom of
  #The first time a python script is sourced the function throws an error. It can't find
  #rpytools. If you source again it works. The temp work around is to wrap the source in
  #try() so the error is caught and then run it a second time. Its ugly but it works.
  #See also smallareamodel::output_small_area_excels
  
  try(source_python('model_code/other_scripts/python_to_excel_households.py'), silent = TRUE)
  source_python('model_code/other_scripts/python_to_excel_households.py') 
  
  if(model %in% c("ons", "both")){
    python_to_excel_households(output_dir, paste0(projection_name,"_ons_households.xlsx"), "ons")
  }
  if(model %in% c("dclg", "both")){
    python_to_excel_households(output_dir, paste0(projection_name,"_dclg_households.xlsx"), "dclg")
  }
  
}