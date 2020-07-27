#' Return a data frame with local authority codes and names
#'
#' @importFrom data.table fread
#'
#' @export

get_gss_names <- function(){

  gss_names <- readRDS("input_data/lookup/lad18_and_region_code_to_name.rds") %>%
    data.frame()
}




