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

create_household_model_excels <- function(output_dir, projection_name){
  
  source_python('model_code/other_scripts/python_to_excel_households.py') 
  python_to_excel_households(output_dir, paste0(projection_name,"_ons_households.xlsx"), "ons")
  python_to_excel_households(output_dir, paste0(projection_name,"_dclg_households.xlsx"), "dclg")
  
}