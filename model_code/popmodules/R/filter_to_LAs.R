#' Filter a dataframe to standard model geography
#'
#' Filter a dataframe so that it includes only district-level data for England
#' and Wales and national-level data for Northern Ireland and Scotland
#'
#' @param df A dataframe to be filtered
#'
#' @import assertthat
#' @importFrom dtplyr lazy_dt
#' @import dplyr
#' @export

filter_to_LAs <- function(df) {
  
  if("gss_code" %in% names(df)){
    origin_destination <- FALSE
  } else if("gss_in" %in% names(df)){
    origin_destination <- TRUE
  }
  else{
    print(names(df))
    stop("filter_to_LAs could not find gss_column")
  }
  
  if(origin_destination == FALSE){ 
  
      df <- dtplyr::lazy_dt(df) %>%
      filter(substr(gss_code, 1, 2) %in% c("E0", "W0", "S9", "N9")) 
    
  } else {
    
    df <- dtplyr::lazy_dt(df) %>%
      filter(substr(gss_in, 1, 2) %in% c("E0", "W0", "S9", "N9"),
             substr(gss_out, 1, 2) %in% c("E0", "W0", "S9", "N9"))
  }
  
  df <- as.data.frame(df)
  
}
