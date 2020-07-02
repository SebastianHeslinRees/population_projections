#' Return a dataframe containing rates or flows for use in the trend model
#'
#' A function to read in data and where necessary create transitional data between
#' one known data point and the next. The returned dataframe contains a data column
#' \code{data_col} with the rate or flow to use in the model and also columns \code{value_1}
#' and \code{value_2} to be used in the next call to this function.
#'
#' @param df A data frame or NULL. The dataframe is the output of this function in
#'   the previous year iteration
#' @param df_info A data frame. The output from the function \code{get_rates_flows_info}
#' @param projection_year Numeric
#' @param first_proj_yr Numeric. The first projection year
#' @param col_aggregation Character. The non-data columns in the dataframe \code{df}
#' @param data_col String. The column containing the flow or rate information to be
#'   in the model
#'
#' @return A dataframe containing rates or flows to use in the model and also
#'   rates or flows to be used in future calculations
#'
#' @import dplyr

get_rates_or_flows <- function(df, df_info, projection_year, first_proj_yr,
                                col_aggregation, data_col){
 
  #value_1 = data_col for df_info$path
  #value_2 = data_col for df_info$next_path
  #data_col = actual rate used in projection_year-1

  curr_yr_info <- filter(df_info, year == projection_year)
  assertthat::assert_that(nrow(curr_yr_info)==1,msg="too many rows in df_info dataframe")
  assertthat::assert_that(!is.null(df) | !is.na(curr_yr_info$path),
                          msg = "Must provide input df or a path to read from in the current year of df_info")

  # return input if rates will be the same as last year
  if(is.na(curr_yr_info$path) && !curr_yr_info$transition) {
    return(df)
  }
  
  if(!is.null(df) & data_col %in% names(df)){
    df <- select(df, -!!data_col)
  }
  
  if(!is.na(curr_yr_info$path)){
    
    if(projection_year == first_proj_yr){
      
      #create the df in the first year
      df <- readRDS(curr_yr_info$path) %>%
        validate_rates_or_flows(col_aggregation, data_col) %>%
        rename(value_2 = !!data_col)
    }
    
    df <- df %>%
      select_at(c(col_aggregation, "value_2")) %>%
      rename(value_1 = value_2)
    
    if(!is.na(curr_yr_info$next_path)){
      
      #add next set of data
      df <- readRDS(curr_yr_info$next_path) %>%
        validate_rates_or_flows(col_aggregation, data_col) %>%
        rename(value_2 = !!data_col) %>%
        full_join(df, by = col_aggregation) %>% 
        tidyr::replace_na(list(value_1 = 0, value_2 = 0))
    }
    
  }
  
  if(curr_yr_info$transition){
    
    df <- df %>% 
      #select(-!!data_col) %>% 
      mutate(increment = (value_2-value_1)/curr_yr_info$transition_period,
             value = value_1+(increment*curr_yr_info$period_step)) %>%
      select(-increment) %>%
      check_negative_values("value", set_to_zero = TRUE)
    
  } else {
    
    df <- df %>% 
      mutate(value = value_1) #this is not a rename
  }
  
  df <- df %>%
    rename(!!data_col := value)
  
  return(df)
  
}


validate_rates_or_flows <- function(df, col_aggregation, data_col) {
  if(all(c("gss_out", "gss_in") %in% names(df))) {
    validate_population(df, col_aggregation, col_data = data_col, test_complete = FALSE, test_unique = TRUE)
  } else {
    validate_population(df, col_aggregation, data_col, test_complete = TRUE, test_unique = TRUE)
  }
  if(data_col == "rate") {
    assert_that(max(df$rate) <= 1 & min(df$rate) >= 0, msg = "rate data contains rates outside the range 0-1")
  }
  invisible(df)
}