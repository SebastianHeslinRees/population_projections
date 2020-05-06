#' Aggregate district-level data to regional totals
#'
#' @param x A data frame containing population/component data.
#' @param lookup A dataframe containing a district to region gss code lookup
#' @param col_aggregation A string of column names on which to aggregate the data
#' @param england Logical indicating whether or not an England total should also 
#'   be calculated. Default \code{FALSE}
#' 
#' @return The input data frame with additional rows for English regions and Wales
#'
#' @import dplyr
#'
#' @export

aggregate_regions <- function(x, lookup,
                              col_aggregation = c("year", "region_gss_code", "age", "sex"),
                              england = FALSE){
  
  if(any(c("E12", "E92") %in% substr(x$gss_code, 1, 3))) {
    warning("aggregate_regions called on data frame with regional England data. Returning unchanged.")
    return(x)
  }
  
  data_col <- last(names(x))
  
  regions <- x %>%
    rename(value := !!data_col) %>%
    dtplyr::lazy_dt() %>%
    filter(substr(gss_code,1,1) %in% c("E","W")) %>% 
    left_join(lookup, by="gss_code") %>%
    group_by_at(col_aggregation) %>% 
    summarise(value = sum(value)) %>%
    as.data.frame() %>%
    rename(gss_code = region_gss_code,
           !!data_col := value)
  
  output_df <- rbind(x, regions)
  
  if(england){
    
    eng <- x %>%
      rename(value := !!data_col) %>%
      dtplyr::lazy_dt() %>%
      filter(substr(gss_code,1,3) %in% c("E06","E07","E08","E09")) %>% 
      mutate(region_gss_code = "E92000001") %>%
      group_by_at(col_aggregation) %>% 
      summarise(value = sum(value)) %>%
      as.data.frame() %>%
      rename(gss_code = region_gss_code,
             !!data_col := value)
    
    output_df <- rbind(output_df, eng)
    
  }
  
  return(output_df)
  
}