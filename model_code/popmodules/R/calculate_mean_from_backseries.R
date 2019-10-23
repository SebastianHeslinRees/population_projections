#' Calculate averagesbased on past data
#'
#' Given a backseries of data the function will return an average
#'
#' @param data_backseries Dataframe. A set of rates or flows by LA, sex and age
#' @param years_to_avg numeric. Number of years data to include in the average
#' @param last_data_year numeric. The final year of data to include in the average
#' @param data_col Character. The name of the column containing the data
#'
#' @return A data frame of mortality probabilities, fertility rates, or
#' international migration flows/rates.
#'
#' @import dplyr
#' @import assertthat
#'
#' @export

calculate_mean_from_backseries <- function(data_backseries, years_to_avg, last_data_year, data_col, col_aggregation = c("gss_code","sex","age")){

  assert_that(is.data.frame(data_backseries),
              msg="calc_trend_rate expects that data_backseries is a dataframe")
  assert_that(is.numeric(years_to_avg),
              msg="calc_trend_rate expects that years_to_avg is an numeric")
  assert_that(is.numeric(last_data_year),
              msg="calc_trend_rate expects that last_data_year is an numeric")
  assert_that(is.character(data_col),
              msg="calc_trend_rate expects that data_col is a column")
  assert_that(data_col %in% names(data_backseries),
              msg = "in calc_trend_rate the rtae_col variable must be the name of a column in the data_backseries dataframe")

  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)

  averaged <- data_backseries %>%
    rename(value = data_col) %>%
    filter(year %in% back_years) %>%
    group_by_at(col_aggregation) %>%
    summarise(value = sum(value)/years_to_avg) %>%
    ungroup() %>%
    mutate(year = last_data_year+1) %>%
    select_at(c("year", col_aggregation, "value")) %>%
    rename(!!data_col := value)

  return(averaged)

}

