#' Scale one population to match the totals of another in a houseing market area
#'
#' Given a cohort population and a data frame of target marginal population
#' subtotals, return the input population scaled so that its population
#' subtotals match the target's at each grouping level.
#'
#' @param popn A data frame containing population data.
#' @param constraint A data frame containing population data at the same
#'   resolution or lower.
#' @param hma_list A named list where the name is an housing market area and the
#'  elements are the gss_codes inside that HMA
#' @param col_aggregation A string or character vector giving the join mapping between
#'   \code{popn} and \code{constraint}. Equivalent to \code{by} in \code{dplyr} joins
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn".
#' @param col_constraint String. Name of column in \code{constraint} containing population
#'   counts. Default col_popn.
#'   
#' @return A data frame of component counts calculated as input popn * rate, with one row
#'   for each distinct level of the input \code{popn} dataframe
#'
#' @import dplyr
#' @importFrom tibble enframe
#' @importFrom tidyr unnest

constrain_to_hma <- function(popn, constraint, hma_list,
                             col_aggregation = c("year","hma","sex","age"),
                             col_popn, col_constraint = col_popn){
  
  hma_df <- hma_list %>% 
    tibble::enframe("hma","gss_code") %>% 
    tidyr::unnest(cols=c("hma","gss_code")) %>%
    as.data.frame()
  
  dont_scale <- filter(popn, !gss_code %in% hma_df$gss_code)
  
  scaled_popn <- filter(popn, gss_code %in% hma_df$gss_code) %>%
    left_join(hma_df, by="gss_code") %>%
    constrain_component(constraint = constraint,
                        col_aggregation = col_aggregation,
                        col_popn = col_popn,
                        col_constraint = col_constraint) %>%
    rbind(dont_scale) %>%
    select(names(popn))
  
  return(scaled_popn)
  
}
