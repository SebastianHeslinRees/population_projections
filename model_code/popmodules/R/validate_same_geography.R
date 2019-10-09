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

  df_1 <- select(df_1, col_1) %>% as.character
  df_2 <- select(df_2, col_2) %>% as.character

  x <- setdiff(df_1, df_2)
  y <- setdiff(df_2, df_1)

  if(length(c(x,y))==0){
    message("same geography")
  }else{
    if(length(x>0)){print(x)
      message("in d1_1, not in df_2")
    }else{print(y)
      message("in df_2, not in df_1")}
  }
}
