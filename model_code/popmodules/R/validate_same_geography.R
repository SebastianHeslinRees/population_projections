#' Confirm two datasets have matching geography
#'
#' Performs a check that the unique set of gss_codes in one
#' dataframe is the same as the unique set of gss_codes in
#' another
#'
#' @param df_1 A dataframe containing a column with geography codes or names
#' @param df_2 A dataframe containing a column with geography codes or names
#' @param col_1 Character. The name of the column to be tested in df_1
#'   (default 'gss_code')
#' @param col_2 Character. The name of the column to be tested in df_2
#'   (default to same as col_1)
#' @param error_or_warn String. Should the function return a warning or an error.
#'   Set to either 'error' to stop operation or 'warn' to give a warning only.
#'   Default 'error'. 
#' @return Warnings or errors if there differences in the gss codes supplied
#'
#' @import dplyr
#' @importFrom tidyselect all_of
#' @importFrom assertthat assert_that
#' 
#' @export

validate_same_geog <- function(df_1, df_2, col_1="gss_code", col_2=col_1,
                               error_or_warn = "error"){
  
  #validate function inputs
  assert_that(error_or_warn %in% c("warn","error"),
              msg = paste0("in validte_same_geog, param error_or_warn is '",
                           error_or_warn, "' - must be 'error' or 'warn'"))
  assert_that(all(col_1 %in% names(df_1)),
              msg = paste0("in validte_same_geog, col_1 not in df_1"))
  assert_that(all(col_2 %in% names(df_2)),
              msg = paste0("in validte_same_geog, col_2 not in df_2"))
  assert_that(length(col_1)==length(col_2),
              msg = "in validate_same_geog vectors of different lengths passed to col_1 and col_2")
  browser()
  #check that the geography columns contain the same set of codes
  for(i in 1:length(col_1)){
    
    are_equal <- setequal(df_1[[col_1[i]]], df_2[[col_2[i]]])
    
    if(!are_equal){
      same_geog_report(df_1, df_2, col_1[i], col_2[i])
    }
  }
  
  invisible()
  
}

#if they don't match then output useful info about the differences

same_geog_report <- function(df_1, df_2, col_1, col_2){
  
  nm_1 <- deparse(substitute(df_1))
  nm_2 <- deparse(substitute(df_2))
  
  df_1 <- df_1[all_of(col_1)] %>% unique() %>% rename("gss_code___" = col_1)
  df_2 <- df_2[all_of(col_2)] %>% unique() %>% rename("gss_code___" = col_2)
  
  x <- setdiff(df_1$gss_code___, df_2$gss_code___)
  if(length(x)>0 && error_or_warn == "warn"){
    warning(paste(length(x),"codes in",nm_1,"not in",nm_2, ": " , x))
  }
  if(length(x)>0 && error_or_warn == "error"){
    stop(paste(length(x),"codes in",nm_1,"not in",nm_2, ": " , x))
  }
  
  
  y <- setdiff(df_2$gss_code___, df_1$gss_code___)
  if(length(y)>0 && error_or_warn == "warn"){
    warning(paste(length(y),"codes in",nm_2,"not in",nm_1, ": " , y))
  }
  if(length(y)>0 && error_or_warn == "error"){
    stop(paste(length(y),"codes in",nm_2,"not in",nm_1, ": " , y))
  }
  
}
