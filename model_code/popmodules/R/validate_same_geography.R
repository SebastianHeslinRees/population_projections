#' Confirm two datasets have matching geography
#'
#' Performs a check that the unique set of gss_codes in one
#' dataframe is the same as the unique set of gss_codes in
#' another
#'
#' @param df_1 A dataframe containing a column of gss codes
#' @param df_2 A dataframe containing a column of gss codes
#' @param col_1 Character. The name of the column containing gss codes in df_1
#'   (default 'gss_code')
#' @param col_2 Character. The name of the column containing gss codes in df_2
#'   (default 'gss_code')
#' @return Warnings if there differences in the gss codes supplied
#'
#' @import dplyr
#' @importFrom tidyselect all_of
#' @export

validate_same_geog <- function(df_1, df_2, col_1="gss_code", col_2="gss_code"){
  
  nm_1 <- deparse(substitute(df_1))
  nm_2 <- deparse(substitute(df_2))
  
  df_1 <- df_1[all_of(col_1)] %>% unique() %>% rename("gss_code" = col_1)
  df_2 <- df_2[all_of(col_2)] %>% unique() %>% rename("gss_code" = col_2)
  
  x <- setdiff(df_1$gss_code, df_2$gss_code)
  y <- setdiff(df_2$gss_code, df_1$gss_code)

  if(length(x)>0){
    warning(paste(length(x),"codes in",nm_1,"not in",nm_2, ": " , x))
  }
  
  if(length(y)>0){
    warning(paste(length(y),"codes in",nm_2,"not in",nm_1, ": " , y))
  }
  
  invisible()
  
}
