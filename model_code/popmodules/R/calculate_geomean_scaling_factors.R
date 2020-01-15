#TODO Document

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
    summarise(scaling = EnvStats::geoMean(scaling)) %>%
    as.data.frame()
  
  return(scaling_dataframe)
}

  