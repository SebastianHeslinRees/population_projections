#' Reorder a dataframe by geographies for output
#'
#' Reorders a data frame to put national GSS codes first, followed by regional,
#' then local authorities. Uses the file in
#' \code{input_data/lookup/output_order.rds} to reorder by default.
#'
#' @param df A data frame.
#' @param gss_col A string containing the name of the column with gss codes.
#'   Default "gss_code".
#' @param path_to_reorder A string with a path to a file with data to reorder
#'   by. Default \code{"input_data/lookup/output_order.rds"}. Must contain a
#'   column matching \code{gss_col} and a column named \code{output_order}.
#' @param desc Logical. Reverse the order.
#' 
#' @return The input data frame, reordered by the rankings in the file given by
#'   \code{path_to_reorder}.
#'   
#' @import dplyr
#' @export


reorder_for_output <- function(df,
                               gss_col = "gss_code",
                               path_to_reorder = "input_data/lookup/output_order.rds",
                               desc = FALSE) {
  
  output_order <- readRDS(path_to_reorder)
  
  if(desc) {
    output_order <- output_order %>% 
      mutate(output_order = max(output_order) - output_order)
  }
  
  df %>%
    left_join(output_order, by = gss_col) %>%
    arrange(output_order) %>%
    select(-output_order)
}