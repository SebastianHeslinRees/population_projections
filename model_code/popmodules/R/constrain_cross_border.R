#' Scale a set of cross-border flows to match the totals of another
#'
#' A wrapper for \code{get_scaling_factor} which prepares origin-destination
#' flow data, constraining flows out of England to match the target data frame
#' in \code{in_constraint} and flows out of England to another the target data
#' frame in \code{out_constraint}.
#'
#' @param domestic_flow A data frame containing origin-destination flows
#' @param in_constraint A data frame containing cross-border totals data at the same
#'   resolution or lower.
#' @param out_constraint A data frame containing cross-border totals data at the same
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
#' @importFrom assertthat assert_that
#' @importFrom dtplyr lazydt
#'
#' @export

constrain_cross_border <- function(domestic_flow, in_constraint, out_constraint, col_flow = "flow"){
  
  cols <- names(domestic_flow)
  
  #this isn't the prettiest way of doing this but its the fastest I can find
  domestic_flow <- domestic_flow %>%
    mutate(country_in = substr(gss_in,1,1),
           country_out = substr(gss_out,1,1),
           scale = case_when(country_in == "E" & country_out != "E" ~ TRUE,
                             country_out == "E" & country_in != "E" ~ TRUE,
                             TRUE ~ FALSE))
  
  out_flow <- filter(domestic_flow, scale == TRUE & country_out == "E") %>%
    select(cols)
  
  in_flow <- filter(domestic_flow, scale == TRUE & country_in == "E") %>%
    select(cols)
  
  #dont_scale
  dont_scale <- filter(domestic_flow, scale == FALSE) %>%
    select(cols)
  
  #validate
  assert_that((nrow(out_flow)+nrow(in_flow)+nrow(dont_scale))==nrow(domestic_flow),
              msg="constrain_cross_border has lost some rows while figuring out what needs to be scaled")

  #Out Flow
  out_scaling <- lazy_dt(out_flow) %>%
    rename(cross_out = col_flow) %>%
    group_by(year, gss_out, sex, age) %>%
    summarise(cross_out = sum(cross_out)) %>%
    ungroup() %>%
    mutate(country = "E") %>%
    as.data.frame() %>%
    get_scaling_factors(out_constraint, col_popn="cross_out") %>%
    select(-cross_out, -country)
  
  scaled_out <- out_flow %>%
    left_join(out_scaling, by=c("year","gss_out","sex","age")) %>%
    mutate(scaled = !!sym(col_flow) * scaling) %>%
    select(-!!sym(col_flow)) %>%
    rename(!!col_flow := scaled) %>%
    select(cols)

  #In flow
  in_scaling <- lazy_dt(in_flow) %>%
    rename(cross_in = col_flow) %>%
    group_by(year, gss_in, sex, age) %>%
    summarise(cross_in = sum(cross_in))%>%
    ungroup() %>%
    mutate(country = "E") %>%
    as.data.frame() %>%
    get_scaling_factors(in_constraint, col_popn="cross_in") %>%
    select(-cross_in, -country)
  
  scaled_in <- in_flow %>%
    left_join(in_scaling, by=c("year","gss_in","sex","age")) %>%
    mutate(scaled = !!sym(col_flow) * scaling) %>%
    select(-!!sym(col_flow)) %>%
    rename(!!col_flow := scaled) %>%
    select(cols)
  
  #Put it back together
  scaled_flows <- rbindlist(list(dont_scale, scaled_in, scaled_out)) %>%
    as.data.frame() %>%
    arrange(gss_out, gss_in)
  
  #validate
  assert_that(nrow(out_flow)==nrow(scaled_out), msg="constrain_cross_border has lost some rows while calculating out flows")
  assert_that(nrow(in_flow)==nrow(scaled_in), msg="constrain_cross_border has lost some rows while calculating inflows")
  assert_that(nrow(scaled_flows)==nrow(domestic_flow), msg="constrain_cross_border has lost some rows")

  return(scaled_flows)
  
}
