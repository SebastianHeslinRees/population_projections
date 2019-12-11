#' Calculate a residual between populations and convert to domestic migration
#' flows
#'
#' A method of reconciling the differnece between a population and a target
#' constraint. Rather than scaling the whole population to match, the difference
#' can be used to calculate additional domestic migration flows that are added
#' or removed from the population.
#'
#' The function calculates the difference between a population and a (lower
#' resolution) target, and then returns updated domestic in- and outmigration
#' rates that include the residual.
#'
#' @param popn A data frame containing a population.
#' @param target A data frame containing a population at a lower resolution to
#'   be constrained to. Must share column names with \code{popn} to enable a
#'   join.
#' @param col_aggregation Character vector. Names of columns to join \code{popn}
#'   to \code{target}.
#' @param dom_in A data frame containing domestic migration inflows (usually
#'   from an earlier model step). It will be scaled to include additional
#'   inflows. Must contain a column named \code{dom_in} with inflows.
#' @param dom_out A data frame containing domestic migration outflows (usually
#'   from an earlier model step). It will be scaled to include additional
#'   outflows. Must contain a column named \code{dom_out} with outflows.
#' @param col_popn A string with the name of the column containing population
#'   counts in \code{popn}
#' @param col_target A string with the name of the column containing population
#'   counts in \code{target}
#'
#' @return A list of two data frames. \code{dom_in} contains the input domestic
#'   inflows scaled to include the calculated positive residuals. \code{dom_out}
#'   contains the input domestic outflows scaled to include the
#'   calulatednegative residuals.
#'
#' @import dplyr
#'
#' @export

adjust_domestic_migration <- function(popn, target, dom_in, dom_out,
                                      col_aggregation = c("year","gss_code"),
                                      col_popn, col_target){

  validate_join_population(popn, target, cols_common_aggregation = col_aggregation,
                           many2one = TRUE, one2many = FALSE, warn_unused_shared_cols = FALSE)

  #find diff between popn and target
  col_popn_new <- paste0(col_popn, ".x")
  col_target_new  <- paste0(col_target,  ".y")
  popn <- rename(popn, !!col_popn_new := !!col_popn)
  target <- rename(target, !!col_target_new := !!col_target)

  domestic_adjustment <- popn %>%
    group_by_at(col_aggregation) %>%
    summarise(!!col_popn_new := sum(!!sym(col_popn_new))) %>%
    ungroup() %>%
    left_join(target, by=col_aggregation) %>%
    mutate(net_dom_adjustment = !!sym(col_target_new) - !!sym(col_popn_new)) %>%
    select(c(col_aggregation, net_dom_adjustment))

  #if the change is poisitive then its an addition to in migration
  #if negative then add to out migration
  gross_in_adjustment <- domestic_adjustment %>%
    mutate(net_dom_adjustment = ifelse(net_dom_adjustment > 0, net_dom_adjustment, 0))
  gross_out_adjustment <- domestic_adjustment %>%
    mutate(net_dom_adjustment = ifelse(net_dom_adjustment < 0, net_dom_adjustment, 0))

  dom_in_target <- dom_in %>%
    group_by_at(col_aggregation) %>%
    summarise(dom_in_orig = sum(dom_in)) %>%
    ungroup() %>%
    left_join(gross_in_adjustment, by = col_aggregation) %>%
    mutate(dom_in_target = dom_in_orig + net_dom_adjustment)  %>%
    select(c(col_aggregation, dom_in_target))

  dom_out_target <- dom_out %>%
    group_by_at(col_aggregation) %>%
    summarise(dom_out_orig = sum(dom_out)) %>%
    ungroup() %>%
    left_join(gross_in_adjustment, by = col_aggregation) %>%
    mutate(dom_out_target = dom_out_orig + net_dom_adjustment)  %>%
    select(c(col_aggregation, dom_out_target))

  assert_that(all(complete.cases(dom_in_target)))
  assert_that(all(complete.cases(dom_out_target)))

  #Adust the gross flows
  adjusted_dom_in <- constrain_component(popn = dom_in,
                                         constraint = dom_in_target,
                                         col_aggregation = col_aggregation,
                                         col_popn = "dom_in",
                                         col_constraint = "dom_in_target")

  adjusted_dom_out <- constrain_component(popn = dom_out,
                                          constraint = dom_out_target,
                                          col_aggregation = col_aggregation,
                                          col_popn = "dom_out",
                                          col_constraint = "dom_out_target")

  return(list(dom_in = adjusted_dom_in,
              dom_out = adjusted_dom_out))

}


