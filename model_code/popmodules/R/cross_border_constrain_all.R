#' Scale domestic outflows to matach a set of constraining flows
#'
#' A wrapper for \code{get_scaling_factor} which prepares origin-destination
#' flow data to feed into the function and which applies the function's output
#' back to the flow data resulting in sacled flows.
#' 
#' The function assumes all cross border flows in the system are scaled and
#' therefore only scales the inflows as this will, by definition, also scale
#' the outflows
#'
#' @param domestic_flow A data frame containing origin-destination flows
#' @param in_constraint A data frame containing cross-border inflow totals data 
#' @param col_flow String. Name of column in \code{domestic_flow} containing population
#'   counts. Default "flow"
#'
#' @return A data frame of scaled domestic flows by authority, sex and
#'   single year of age.


cross_border_constrain_all <- function(domestic_flow, in_constraint, col_flow = "flow"){
  
  in_scaling <- domestic_flow %>%
    mutate(country_out = substr(gss_out,1,1),
           country_in = substr(gss_in,1,1)) %>%
    filter(country_out != country_in) %>%
    rename(cross_in = col_flow,
           country = country_in) %>%
    group_by(year, sex, age, country) %>%
    summarise(cross_in = sum(cross_in)) %>%
    ungroup() %>%
    get_scaling_factors(in_constraint, col_popn="cross_in", col_aggregation = c("year", "sex", "age", "country")) %>%
    select(-cross_in)
  
  scaled_in <- domestic_flow %>%
    mutate(country_out = substr(gss_out,1,1),
           country_in = substr(gss_in,1,1)) %>%
    filter(country_out != country_in) %>%
    rename(country = country_in) %>%
    left_join(in_scaling, by=c("year","country","sex","age")) %>%
    mutate(scaled = !!sym(col_flow) * scaling) %>%
    select(-!!sym(col_flow)) %>%
    rename(!!col_flow := scaled) %>%
    select(names(domestic_flow))
  
  output <- domestic_flow %>%
    mutate(country_out = substr(gss_out,1,1),
           country_in = substr(gss_in,1,1)) %>%
    filter(country_out == country_in) %>%
    select(names(domestic_flow)) %>%
    rbind(scaled_in) %>%
    arrange(gss_out, gss_in, sex, age)
  
  return(output)
}