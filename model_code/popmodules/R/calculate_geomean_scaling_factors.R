#' Calculate a set of scaling factors to apply to fertility or mortality rates
#'
#' For a backseries of years, iven population for small areas and
#' fertility/mortality rates for a higher geography, calculate a set of
#' implied births/deaths. Compare these to observed births/deaths and
#' create scaling facrots for the rates. Finally, calculate the geometric
#' mean of the scaling factors.
#'
#' The function calculates the difference between a population and a (lower
#' resolution) target, and then returns updated domestic in- and outmigration
#' rates that include the residual.
#'
#' @param popn A data frame containing a population at small area (ward or msoa)
#'   by sex and age.
#' @param future_rates A data frame containing mortality or fertility rates at
#'   local authority level by sex and age.
#' @param data_years Numeric vector of years to average.
#' @param constraint A dataframe of total births or deaths for each small area.
#' @param constraint_data_col A string giving the column with data in the
#'   constraint file.
#'
#' @return A dataframe of scaling factors by sex and age for each small area.
#'
#' @import dplyr
#' @importFrom Envstats geoMean
#'
#' @export

calculate_geomean_scaling_factors <- function(popn, future_rates, data_years, constraint, constraint_data_col){
  
  component_from_rate <- popn_age_on(as.data.frame(popn),
                                   col_aggregation = c("year", "gss_code_small_area", "age", "sex")) %>%
    filter(year %in% data_years) %>%
    dtplyr::lazy_dt() %>%
    left_join(future_rates, by=c("gss_code","sex","age")) %>%
    mutate(value_from_rate = rate*popn) %>%
    group_by(year, gss_code, gss_code_small_area) %>%
    summarise(value_from_rate = sum(value_from_rate)) %>%
    as.data.frame()
  
  scaling_dataframe <- component_from_rate %>%
    calculate_scaling_factors(constraint = constraint,
                              col_aggregation = c("year","gss_code_small_area"),
                              col_popn = "value_from_rate",
                              col_constraint = constraint_data_col,
                              rows_to_constrain = TRUE) %>%
    dtplyr::lazy_dt() %>%
    group_by(gss_code_small_area) %>%
    summarise(scaling = geoMean(scaling)) %>%
    as.data.frame()
  
  return(scaling_dataframe)
}

  