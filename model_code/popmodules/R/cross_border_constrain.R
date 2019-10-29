#' Scale one population to match the totals of another
#'
#' A wrapper for \code{get_scaling_factor} which prepares origin-destination
#' flow data to feed into the function and which applies the function's output
#' back to the flow data resulting in sacled flows.
#'
#' @param domestic_flow A data frame containing origin-destination flows
#' @param constraint A data frame containing cross-border totals data at the same
#'   resolution or lower.
#' @param col_flow String. Name of column in \code{domestic_flow} containing population
#'   counts. Default "flow"
#'
#' @return A data frame of scaled domestic flows by authority, sex and
#'   single year of age.

cross_border_constrain <- function(domestic_flow, constraint, col_flow = "flow")
  
  out_flow <- filter(domestic_flow, substr(gss_out,1,1) == "E" & substr(gss_in,1,1) != "E")
  in_flow <- filter(domestic_flow, substr(gss_out,1,1) != "E" & substr(gss_in,1,1) == "E")
  
  #Out Flow
  out_scaling <- out_flow %>%
    group_by(year, sex, age) %>%
    summarise(flow = sum(flow)) %>%
    get_scaling_factors(out_flow, constraint, col_popn=col_flow)
  
  scaled_out <- out_flow %>%
    left_join(scaling, by=c("year","sex","age")) %>%
    mutate(scaled = flow * scaling) %>%
    ungroup() %>%
    rename(scaled = col_flow) %>%
    select(names(domestic_flow)) 
  
  #In flow
  in_scaling <- in_flow %>%
    group_by(year, sex, age) %>%
    summarise(flow = sum(flow))%>%
    ungroup() %>%
    get_scaling_factors(out_flow, constraint, col_popn=col_flow)
  
  scaled_in <- in_flow %>%
    left_join(scaling, by=c("year","sex","age")) %>%
    mutate(scaled = flow * scaling) %>%
    rename(scaled = col_flow) %>%
    select(names(domestic_flow)) 
  
  #Put it back together
  scaled_flows <- filter(domestic_flow, substr(gss_out,1,1) == "E" & substr(gss_in,1,1) == "E") %>%
    rbind(scaled_out, scaled_in)
  
  testthat::expect_equal(nrow(scaled_flows),nrow(domestic_flow))
  
  return(scaled_flows)
  
  }