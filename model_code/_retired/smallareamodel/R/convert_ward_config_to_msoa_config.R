#' A simple function to convert a ward model config to an msoa model config
#' 
#' Goes through the ward config list and replaces the string \code{ward} with
#' the string \code{msoa}. Also changes the path of the small area to district
#' lookup file.
#' 
#' @param ward_config_list A small area model ward config list
#'  
#' @return An msoa config list for the small area model
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export

convert_ward_config_to_msoa_config <- function(ward_config_list){
  
  convert_elements <- function(x){
    if(is.numeric(x)){
      return(x)
    }
    if(is.character(x)){
      x <- str_replace_all(x, "ward", "msoa")
    }
    if(x == "input_data/lookup/2011_msoa_to_district.rds"){
      x <- "input_data/lookup/msoa_to_district.rds"
    }
    return(x)
  }
  
  return(lapply(ward_config_list, convert_elements))
}