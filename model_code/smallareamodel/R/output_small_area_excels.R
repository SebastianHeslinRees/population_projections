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
#'
#' @export

output_small_area_excels <- function(output_dir, wb_filename, ward = TRUE, msoa = FALSE){
  
  #Create excels output directory
  output_dir <- .add_slash(output_dir)
  dir.create(paste0(output_dir,"excels"), showWarnings = FALSE)
  
  #Create workbook names
  if(substr(wb_filename, nchar(wb_filename)-4, nchar(wb_filename)) == ".xlsx"){
    wb_filename <- substr(wb_filename,1,nchar(wb_filename)-5)
  } 
  
  ward_filename <- paste0(wb_filename,"_ward.xlsx")
  msoa_filename <- paste0(wb_filename,"_msoa.xlsx")
  
  #Source python function
  source_python('model_code/other_scripts/python_to_excel_small_area.py') 
  
  if(ward==TRUE){
    python_to_excel_small_area(output_dir, ward_filename, "ward") #~3.5 mins
    message("ward output complete")
  }
  
  if(msoa==TRUE){
    python_to_excel_small_area(output_dir, msoa_filename, "msoa") #~5 mins
    message("MSOA output complete")
  }
  
}