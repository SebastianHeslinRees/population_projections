get_gss_names <- function(){

  gss_names <- readRDS("input_data/lookup/lad18_and_region_code_to_name.rds") %>%
    data.frame() %>%
    popmodules::recode_gss_codes(col_aggregation = c("gss_code","gss_name"), aggregate_data = FALSE)

}




