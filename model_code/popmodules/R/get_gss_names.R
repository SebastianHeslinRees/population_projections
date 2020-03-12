#' Return a data frame with local authority codes and names
#'
#' @importFrom data.table fread
#'
#' @export

get_gss_names <- function(){

  gss_names <- data.table::fread("input_data/lookup/lad18_code_to_name.csv") %>%
    data.frame() %>%
    popmodules::recode_gss_to_2011(col_aggregation = c("gss_code","gss_name"), aggregate_data = FALSE)

}




