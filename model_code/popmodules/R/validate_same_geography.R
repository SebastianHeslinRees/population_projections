#' Confirm two datasets have matching geography
#'
#' Performs a check that the unique set of gss_codes in one
#' dataframe is the same as the unique set of gss_codes in
#' another
#'
#' @param df_1 A dataframe containing a gss_code column.
#' @param df_2 A dataframe containing a gss_code column.
#' @param col_1 Character. The name of the column containing gss_codes in df_1
#'   (default 'gss_code')
#' @param col_2 Character. The name of the column containing gss_codes in df_2
#'   (default 'gss_code')
#' @return A report on whether the test passed
#'
#' @import dplyr
#' @export

validate_same_geog <- function(df_1, df_2, col_1="gss_code", col_2="gss_code"){
  
  nm_1 <- deparse(substitute(df_1))
  nm_2 <- deparse(substitute(df_2))
  
  df_1 <- df_1[col_1] %>% unique()
  df_2 <- df_2[col_2] %>% unique()
  
  x <- setdiff(df_1, df_2)
  y <- setdiff(df_2, df_1)
  
  if(nrow(rbind(x,y))==0){
    message(paste("same geography in",nm_1,"&",nm_2))
  }
  
  if(nrow(x)>0){
    message(paste(nrow(x),"codes in",nm_1,"not in",nm_2))
  }
  
  if(nrow(y)>0){
    message(paste(nrow(y),"codes in",nm_2,"not in",nm_1))
  }
  
}
