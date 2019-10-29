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

cross_border_constrain <- function(domestic_flow, in_constraint, out_constraint, col_flow = "flow"){
  
  out_flow <- filter(domestic_flow, substr(gss_out,1,1) == "E" & substr(gss_in,1,1) != "E")
  in_flow <- filter(domestic_flow, substr(gss_out,1,1) != "E" & substr(gss_in,1,1) == "E")
  
  #Out Flow
  out_scaling <- out_flow %>%
    rename(cross_out = col_flow) %>%
    group_by(year, sex, age) %>%
    summarise(cross_out = sum(cross_out)) %>%
    ungroup() %>%
    get_scaling_factors(out_constraint, col_popn="cross_out")
  
  scaled_out <- out_flow %>%
    left_join(out_scaling, by=c("year","sex","age")) %>%
    rename(flow = col_flow) %>%
    mutate(scaled = flow * scaling) %>%
    rename(!!col_flow := flow) %>%
    select(names(domestic_flow)) 
  
  #In flow
  in_scaling <- in_flow %>%
    rename(cross_in = col_flow) %>%
    group_by(year, sex, age) %>%
    summarise(cross_in = sum(cross_in))%>%
    ungroup() %>%
    get_scaling_factors(in_constraint, col_popn="cross_in")
  
  scaled_in <- in_flow %>%
    left_join(in_scaling, by=c("year","sex","age")) %>%
    rename(flow = col_flow) %>%
    mutate(scaled = flow * scaling) %>%
    rename(!!col_flow := flow) %>%
    select(names(domestic_flow)) 
  
  #Put it back together
  
  #TODO Whats a cleverer/better way to do this
  #Not setdiff it feels like it'll introduce errors
  #Also, this is slow
  pairs <- expand.grid(a = c("N","S","W"),b = c("N","S","W"),
                       stringsAsFactors = F) %>%
    rbind(data.frame(a="E",b="E"))
  dont_scale <- list()
  for(i in seq(nrow(pairs))){
    a <- pairs[i,1]
    b <- pairs[i,2]
    dont_scale[[i]] <- filter(domestic_flow, substr(gss_out,1,1) == a & substr(gss_in,1,1) == b)
  }
  dont_scale <- data.table::rbindlist(dont_scale)
  
  scaled_flows <- rbind(scaled_in, scaled_out, dont_scale) %>%
    data.frame()
  
  testthat::expect_equal(nrow(out_flow),nrow(scaled_out))
  testthat::expect_equal(nrow(in_flow),nrow(scaled_in))
  testthat::expect_equal(nrow(scaled_flows),nrow(domestic_flow))
  
  return(scaled_flows)
  
}