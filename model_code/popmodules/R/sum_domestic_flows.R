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
#' @param flow_col A string. The name of the column containg the flow data. Defaul \code{flow}
#'
#' @return A dataframe with flows into or out of an area summed
#'
#' @import dplyr
#' @importFrom dtplyr lazy_dt
#' @importFrom tidyr complete
#' @importFrom assertthat assert_that
#' 
#' @export

sum_domestic_flows <- function(domestic_flow, in_or_out, flow_col = "flow"){
  
  assert_that(in_or_out %in% c("in", "out"),
              msg = "In sum_domestic_flows in_or_out must be either in or out")
  assert_that(flow_col %in% names(domestic_flow),
              msg = "In sum_domestic_flows flow_col must be in domestic_flow dataframe")
  
  gss_col <- ifelse(in_or_out == "in", "gss_in", "gss_out")
  
  data_col <- ifelse(in_or_out == "in", "dom_in", "dom_out")
  
  dom <- dtplyr::lazy_dt(domestic_flow) %>%
    rename(flow = !!flow_col) %>%
    group_by_at(c("year", gss_col, "sex", "age")) %>%
    summarise(flow = sum(flow)) %>%
    as.data.frame() %>%
    rename(gss_code = !!gss_col) %>%
    tidyr::complete(year, gss_code, age=0:90, sex, fill=list(flow=0)) %>%
    rename(!!data_col := flow)
  
}
