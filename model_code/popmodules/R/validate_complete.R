#' Check that all expected aggreation levels are present
#' 
#' @param x A dataframe
#' @param col_aggregation Character. The aggregation levels to be tested
#' @param col_geog Charater. The column containing geography. These do not
#'  have to be included in the col_aggregation parameter.
#' @param additional_cols Character. Columns not included in col_aggregation
#'  but which need to be included in the test. Default NULL.
#' @warn_only Boolean. If TRUE the function will return a warning, if false
#'  an error is produced. Default FALSE.
#' 
#' @import dplyr
#' @import assertthat
#' 
#' @export

validate_complete <- function(x,
                              col_aggregation = c("year","gss_code","gss_code_ward","sex","age"),
                              col_geog = c("gss_code", "gss_code_ward"),
                              additional_cols = NULL,
                              warn_only = FALSE){
  
  non_geog_cols <- col_aggregation[!col_aggregation %in% col_geog]
  non_geog_cols <- c(non_geog_cols, additional_cols)
  unique_geog <-  x[col_geog] %>% unique()
  
  a <- nrow(unique_geog)
  for(i in non_geog_cols){
    a <- a*length(unique(x[[i]]))
  }
  
  if(nrow(x)!=a){
    if(!warn_only){
      stop(msg = "validate_complete: missing values in input dataframe. Is the col_geog parameter set correctly?")
    } else {
      warning(msg = "validate_complete: missing values in input dataframe. Is the col_geog parameter set correctly?")
    }
  }
  
  invisible()
}


