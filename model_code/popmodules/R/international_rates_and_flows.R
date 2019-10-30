#' Calculate average international rates based on past data
#'
#' A wrapper for \code{calc_mean_rate} which prepares mye
#' and international in/out flow component data.
#'
#' @param popn_mye_path Character. The path to the mye backseries population.
#' @param births_mye_path Character. The path to the mye births backseries.
#' @param flow_or_rate Character. Either "flow" or "rate" - determines whether
#'   the function returns absolute flows or migration rates.
#' @param component_path Character The path to the international in or out migration
#'   component from the mye.
#' @param last_data_year Integer. The last year of data on which to calculate
#'   averages.
#' @param n_years_to_avg Integer. The number of years to use in calculating averages.
#' @param data_col Character. The column in the component dataframe containing
#'   the flow data.
#' @param n_proj_years Numeric. The number of years to project forward.
#'
#' @return A data frame of international migration probabilities.
#'
#' @import dplyr
#' @import assertthat
#' @import data.table
#'
#' @export

international_rates_and_flows <- function(popn_mye_path=NULL, births_mye_path=NULL, flow_or_rate,
                                          component_path, last_data_year, n_years_to_avg, data_col,
                                          first_proj_yr, n_proj_yr, rate_cap = 0.8) {

  component <- readRDS(component_path)

  if(flow_or_rate == "rate"){
    births <- readRDS(births_mye_path) %>%
      filter(age == 0) %>%
      rename(popn = births) %>%
      data.frame()

    population <- readRDS(popn_mye_path) %>%
      #TODO: when merged in this line can be edited and the births above removed
      popn_age_on() %>%
      rbind(births) %>%
      data.frame()

    rate_backseries <- population %>%
      filter(year %in% unique(component$year)) %>%
      left_join(component, by=c("gss_code","year","sex","age")) %>%
      rename(value = data_col) %>%
      mutate(rate = ifelse(popn == 0, 0, value/popn)) %>%
      select(gss_code, year, sex, age, rate)

    #TODO: Does this need a report?
    rates <- calculate_mean_from_backseries(rate_backseries, n_years_to_avg, last_data_year, "rate",
                                            col_aggregation = c("gss_code","sex","age")) %>%
      mutate(rate = ifelse(rate > rate_cap, rate_cap, rate))


    last_proj_yr <- n_proj_yr+first_proj_yr

    output_df <- project_forward_flat(df = rates,
                                      last_proj_yr = last_proj_yr) %>%
      rename(!!data_col := rate) %>%
      select(year, gss_code, sex, age, data_col)
  }

  if(flow_or_rate == "flow"){

    back_years <- c((last_data_year - n_years_to_avg + 1):last_data_year)

    assert_that(all(back_years %in% unique(component$year)),
                msg=("in international_rates_and_flows backyears must be present in component data"))

    flows <- component %>%
      rename(value = data_col) %>%
      filter(year %in% back_years) %>%
      group_by(gss_code, sex, age) %>%
      summarise(value = sum(value)/n_years_to_avg) %>%
      ungroup() %>%
      rename(!!data_col := value) %>%
      mutate(year = last_data_year+1)


    last_proj_yr <- n_proj_yr+first_proj_yr

    output_df <- project_forward_flat(df = flows,
                                      last_proj_yr = last_proj_yr) %>%
      select(year, gss_code, sex, age, data_col)

  }

  validate_population(output_df)

  return(output_df)

}

