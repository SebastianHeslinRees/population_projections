#' Calculate average international rates based on past data
#'
#' A wrepper for \code{calc_mean_rate} which prepares mye
#' and international in/out flow component data.
#'
#' @param popn_mye_path Character. The path to the mye backseries population.
#' @param component_path Character The path to the international in or out migration
#'   componet from the mye.
#' @param last_data_year Integer. The last year of death data on which to calculate
#'   averages.
#' @param years_to_avg Integer. The number of years to use in calculating averages.
#' @param data_col Character. The column in the component dataframe containing
#'   the flow data.
#' @param n_proj_years Numeric. The number of years to project forward
#'
#' @return A data frame of international migration probabilities.
#'
#' @import dplyr
#' @import assertthat
#' @import data.table
#'
#' @export

international_rates_and_flows <- function(popn_mye_path=NULL, births_mye_path=NULL, flow_or_rate,
                                component_path, last_data_year, years_to_avg, data_col, n_proj_yr) {

  component <- readRDS(component_path)

  if(flow_or_rate == "rate"){
  births <- readRDS(births_mye_path) %>%
    filter(age == 0) %>%
    mutate(year = year + 1) %>%
    rename(popn = births) %>%
    data.frame()

  population <- readRDS(popn_mye_path) %>%
    popn_age_on() %>%
    rbind(births) %>%
    data.frame()

  rate_backseries <- population %>%
    filter(year %in% unique(component$year)) %>%
    left_join(component, by=c("gss_code","year","sex","age")) %>%
    rename(value = data_col) %>%
    mutate(rate = ifelse(popn == 0, 0, value/popn)) %>%
    select(gss_code, year, sex, age, rate)

  rates <- calc_mean_rate(rate_backseries, years_to_avg, last_data_year, "rate")

  output_df <- project_forward_flat(df = rates, first_proj_yr = last_data_year+2,
                                  n_proj_yr = n_proj_yr, hold_yr = last_data_year+1) %>%
    select(year, gss_code, sex, age, rate)

  }

  if(flow_or_rate == "flow"){

    back_years <- c((last_data_year - years_to_avg + 1):last_data_year)

    flows <- component %>%
      rename(value = data_col) %>%
      filter(year %in% back_years) %>%
      group_by(gss_code, sex, age) %>%
      summarise(value = sum(value)/years_to_avg) %>%
      ungroup() %>%
      rename(!!data_col := value) %>%
      mutate(year = last_data_year+1)

    output_df <- project_forward_flat(df = flows, first_proj_yr = last_data_year+2,
                                  n_proj_yr = n_proj_yr, hold_yr = last_data_year+1) %>%
      select(year, gss_code, sex, age, data_col)

  }

  validate_population(output_df)

  return(output_df)

}

