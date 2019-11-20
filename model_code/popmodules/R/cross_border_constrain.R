#' Scale one population to match the totals of another
#'
#' A wrapper for \code{get_scaling_factor} which prepares origin-destination
#' flow data to feed into the function and which applies the function's output
#' back to the flow data resulting in sacled flows.
#'
#' @param domestic_flow A data frame containing origin-destination flows
#' @param in_constraint A data frame containing cross-border inflows at the same
#'   resolution or lower.
#' @param out_constraint A data frame containing cross-border outflows at the
#'   same resolution or lower.
#' @param col_flow String. Name of column in \code{domestic_flow} containing
#'   population counts. Default "flow"
#'
#' @return A data frame of scaled domestic flows by authority, sex and single
#'   year of age.
#'
#' @import dplyr
#'
#' @export

cross_border_constrain <- function(domestic_flow, in_constraint, out_constraint, col_flow = "flow"){

  out_flow <- filter(domestic_flow, substr(gss_out,1,1) == "E" & substr(gss_in,1,1) != "E")
  in_flow <- filter(domestic_flow, substr(gss_out,1,1) != "E" & substr(gss_in,1,1) == "E")

  #Out Flow
  out_scaling <- out_flow %>%
    rename(cross_out = col_flow) %>%
    group_by(year, gss_out, sex, age) %>%
    summarise(cross_out = sum(cross_out)) %>%
    ungroup() %>%
    mutate(country = "E") %>%
    get_scaling_factors(out_constraint, col_popn="cross_out") %>%
    select(-cross_out, -country)

  scaled_out <- out_flow %>%
    left_join(out_scaling, by=c("year","gss_out","sex","age")) %>%
    mutate(scaled = !!sym(col_flow) * scaling) %>%
    select(-!!sym(col_flow)) %>%
    rename(!!col_flow := scaled) %>%
    select(names(domestic_flow))

  #In flow
  in_scaling <- in_flow %>%
    rename(cross_in = col_flow) %>%
    group_by(year, gss_in, sex, age) %>%
    summarise(cross_in = sum(cross_in))%>%
    ungroup() %>%
    mutate(country = "E") %>%
    get_scaling_factors(in_constraint, col_popn="cross_in") %>%
    select(-cross_in, -country)

  scaled_in <- in_flow %>%
    left_join(in_scaling, by=c("year","gss_in","sex","age")) %>%
    mutate(scaled = !!sym(col_flow) * scaling) %>%
    select(-!!sym(col_flow)) %>%
    rename(!!col_flow := scaled) %>%
    select(names(domestic_flow))

  #Put it back together

  #TODO this is slow
  scaled_flows <- setdiff(domestic_flow, rbind(out_flow,in_flow)) %>%
    rbind(scaled_in, scaled_out) %>%
    data.frame() %>%
    arrange(gss_out, gss_in)

  testthat::expect_equal(nrow(out_flow),nrow(scaled_out))
  testthat::expect_equal(nrow(in_flow),nrow(scaled_in))
  testthat::expect_equal(nrow(scaled_flows),nrow(domestic_flow))

  return(scaled_flows)

}
