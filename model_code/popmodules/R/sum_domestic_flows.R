#' Calculate a residual between populations and convert to domestic migration
#' flows
#'
#' A method of reconciling the differnece between a population and a target
#' constraint. Rather than scaling the whole population to match, the difference
#' can be used to calculate additional domestic migration flows that are added
#' or removed from the population.
#'
#' Convert origin destiation flow data to gross flow by origin or destination
#'
#' @param domestic_flow A data frame containing sya/sex flows between geographic areas
#' @param in_or_out A string indicating whether the flows should be summed for
#'  the destination (in) or origin (out) area.
#'
#' @return A dataframe with flows into or out of an area summed
#'
#' @import dplyr
#' @importFrom dtplyr lazydt
#' @importFrom tidyr complete
#'
#' @export

sum_domestic_flows <- function(domestic_flow, in_or_out){
  
  gss_col <- ifelse(in_or_out == "in", "gss_in", "gss_out")
  
  data_col <- ifelse(in_or_out == "in", "dom_in", "dom_out")
  
  dom <- dtplyr::lazy_dt(domestic_flow) %>%
    group_by_at(c("year", gss_col, "sex", "age")) %>%
    summarise(flow = sum(flow)) %>%
    as.data.frame() %>%
    rename(gss_code = !!gss_col) %>%
    tidyr::complete(year, gss_code, age=0:90, sex, fill=list(flow=0)) %>%
    rename(!!data_col := flow)
  
}
